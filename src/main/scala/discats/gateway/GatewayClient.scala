package discats.gateway

import cats.effect.*
import cats.effect.syntax.all.*
import cats.effect.std.{Dispatcher, Queue}
import cats.syntax.all.*
import discats.DiscordConfig
import fs2.Stream
import io.circe.jawn.decode
import io.circe.syntax.*

import java.net.URI
import java.net.http.HttpClient as JHttpClient
import java.net.http.WebSocket as JWebSocket
import java.util.concurrent.CompletableFuture
import java.util.concurrent.CompletionStage
import scala.concurrent.duration.*
import scala.jdk.FutureConverters.*

/** Algebra for the Discord Gateway. */
trait GatewayClient[F[_]]:
  /** Continuous stream of typed Gateway events.
    * Manages the WebSocket lifecycle internally:
    *   - sends Identify on fresh connect (or Resume after reconnect)
    *   - drives the heartbeat loop
    *   - reconnects automatically on close or error
    */
  def events: Stream[F, GatewayEvent]

object GatewayClient:

  def apply[F[_]: Async](config: DiscordConfig): GatewayClient[F] =
    new Impl(config)

  private class Impl[F[_]](config: DiscordConfig)(using F: Async[F])
      extends GatewayClient[F]:

    def events: Stream[F, GatewayEvent] =
      Stream
        .eval(GatewayState.ref[F])
        .flatMap(stateRef => connectLoop(stateRef))

    private def connectLoop(stateRef: Ref[F, GatewayState]): Stream[F, GatewayEvent] =
      singleConnection(stateRef)
        .handleErrorWith(_ => Stream.empty) // normalize errors; reconnect is handled below
        .onComplete(Stream.eval(F.sleep(5.seconds)) >> connectLoop(stateRef))

    private def singleConnection(stateRef: Ref[F, GatewayState]): Stream[F, GatewayEvent] =
      Stream.eval(Ref[F].of(Option.empty[Fiber[F, Throwable, Unit]])).flatMap { heartbeatRef =>
        Stream.resource(openConnection).flatMap { case (ws, frameQueue) =>
          Stream
            .fromQueueNoneTerminated(frameQueue)
            .evalMap(text => handleFrame(text, ws, stateRef, heartbeatRef))
            .flatMap(Stream.emits)
            .onFinalize(heartbeatRef.get.flatMap(_.traverse_(_.cancel)))
        }
      }

    /** Open a Java 11 WebSocket and pipe incoming text frames into a Queue. */
    private def openConnection: Resource[F, (JWebSocket, Queue[F, Option[String]])] =
      for
        dispatcher <- Dispatcher.sequential[F]
        queue      <- Resource.eval(Queue.unbounded[F, Option[String]])
        ws         <- Resource.make(buildWebSocket(dispatcher, queue))(ws =>
                        F.delay(ws.sendClose(JWebSocket.NORMAL_CLOSURE, "").get()).void.handleError(_ => ())
                      )
      yield (ws, queue)

    private def buildWebSocket(
        dispatcher: Dispatcher[F],
        queue: Queue[F, Option[String]],
    ): F[JWebSocket] =
      F.async_[JWebSocket] { cb =>
        val listener = new JWebSocket.Listener:
          private val sb = new StringBuilder

          override def onText(
              ws: JWebSocket,
              data: CharSequence,
              last: Boolean,
          ): CompletionStage[?] =
            sb.append(data)
            if last then
              val msg = sb.toString
              sb.clear()
              ws.request(1)
              dispatcher.unsafeRunAndForget(queue.offer(Some(msg)))
            else
              ws.request(1)
            null // use default (completed) stage

          override def onClose(ws: JWebSocket, statusCode: Int, reason: String): CompletionStage[?] =
            dispatcher.unsafeRunAndForget(queue.offer(None))
            null

          override def onError(ws: JWebSocket, error: Throwable): Unit =
            dispatcher.unsafeRunAndForget(queue.offer(None))

        JHttpClient
          .newHttpClient()
          .newWebSocketBuilder()
          .header("Authorization", config.authHeader)
          .buildAsync(URI.create(config.gatewayUrl), listener)
          .thenAccept(ws => { ws.request(1); cb(Right(ws)) })
          .exceptionally(err => { cb(Left(err)); null })

        ()
      }

    private def handleFrame(
        text: String,
        ws: JWebSocket,
        stateRef: Ref[F, GatewayState],
        heartbeatRef: Ref[F, Option[Fiber[F, Throwable, Unit]]],
    ): F[List[GatewayEvent]] =
      decode[GatewayPayload](text) match
        case Left(err)      => F.delay(err.printStackTrace()).as(Nil)
        case Right(payload) => dispatchPayload(payload, ws, stateRef, heartbeatRef)

    private def dispatchPayload(
        payload: GatewayPayload,
        ws: JWebSocket,
        stateRef: Ref[F, GatewayState],
        heartbeatRef: Ref[F, Option[Fiber[F, Throwable, Unit]]],
    ): F[List[GatewayEvent]] =
      payload.op match

        case Opcode.Hello =>
          val interval = payload.d
            .flatMap(_.hcursor.downField("heartbeat_interval").as[Long].toOption)
            .getOrElse(41250L)
          for
            state <- stateRef.get
            _     <- stateRef.update(_.copy(heartbeatInterval = Some(interval)))
            fiber <- startHeartbeat(interval, ws, stateRef).start
            _     <- heartbeatRef.set(Some(fiber))
            _     <- sendIdentifyOrResume(ws, state)
          yield Nil

        case Opcode.Dispatch =>
          for
            _ <- stateRef.update(s => s.copy(seq = payload.s.orElse(s.seq)))
            events = GatewayEvent.fromPayload(payload).toList
            _ <- events.traverse_(e => updateStateFromEvent(e, stateRef))
          yield events

        case Opcode.HeartbeatAck => F.pure(Nil)

        case Opcode.Heartbeat =>
          stateRef.get.flatMap(s => sendFrame(ws, GatewayPayload.heartbeat(s.seq))).as(Nil)

        case Opcode.Reconnect =>
          F.raiseError(GatewayClosedException)

        case Opcode.InvalidSession =>
          val resumable = payload.d.flatMap(_.asBoolean).getOrElse(false)
          (if !resumable then stateRef.update(_.copy(sessionId = None, seq = None)) else F.unit)
            >> F.raiseError(GatewayClosedException)

        case _ => F.pure(Nil)

    private def updateStateFromEvent(event: GatewayEvent, stateRef: Ref[F, GatewayState]): F[Unit] =
      event match
        case GatewayEvent.Ready(_, _, sessionId, resumeUrl, _) =>
          stateRef.update(_.copy(sessionId = Some(sessionId), resumeUrl = Some(resumeUrl)))
        case _ => F.unit

    private def sendIdentifyOrResume(ws: JWebSocket, state: GatewayState): F[Unit] =
      state.sessionId match
        case Some(sid) =>
          sendFrame(ws, GatewayPayload.resume(config.token, sid, state.seq.getOrElse(0)))
        case None =>
          sendFrame(ws, GatewayPayload.identify(config.token, config.intents.value, IdentifyProperties()))

    private def startHeartbeat(
        interval: Long,
        ws: JWebSocket,
        stateRef: Ref[F, GatewayState],
    ): F[Unit] =
      val jitter = (interval * 0.8).toLong
      F.sleep(jitter.millis) >>
        Stream
          .fixedDelay[F](interval.millis)
          .evalMap(_ =>
            stateRef.get
              .flatMap(s => sendFrame(ws, GatewayPayload.heartbeat(s.seq)))
              .handleError(_ => ()) // WebSocket may be closing; fiber will be cancelled shortly
          )
          .compile
          .drain

    private def sendFrame(ws: JWebSocket, payload: GatewayPayload): F[Unit] =
      F.delay(ws.sendText(payload.asJson.noSpaces, true).get()).void

  object GatewayClosedException extends Exception("Gateway connection closed, reconnecting")
