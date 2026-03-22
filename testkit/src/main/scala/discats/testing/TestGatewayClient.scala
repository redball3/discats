package discats.testing

import cats.effect.kernel.Concurrent
import cats.effect.std.Queue
import cats.syntax.all.*
import discats.gateway.{GatewayClient, GatewayEvent}
import fs2.Stream

/** An in-memory [[GatewayClient]] that lets tests inject [[GatewayEvent]]s programmatically.
  *
  * The [[events]] stream consumes from an unbounded queue. Use [[emit]] to push events into
  * the queue and [[complete]] to signal end-of-stream (allowing `compile.drain` to finish).
  *
  * {{{
  * for
  *   gateway <- TestGatewayClient.create[IO]
  *   _       <- gateway.emit(GatewayEvent.MessageCreate(fakeMessage))
  *   _       <- gateway.complete
  *   events  <- gateway.events.compile.toList
  * yield expect(events.size == 1)
  * }}}
  *
  * Obtain an instance via [[TestGatewayClient.create]].
  */
final class TestGatewayClient[F[_]: Concurrent] private (queue: Queue[F, Option[GatewayEvent]])
    extends GatewayClient[F]:

  /** Stream of events; terminates when [[complete]] is called. */
  def events: Stream[F, GatewayEvent] = Stream.fromQueueNoneTerminated(queue)

  /** Push a single event into the stream. */
  def emit(event: GatewayEvent): F[Unit] = queue.offer(Some(event))

  /** Signal end-of-stream. After this, [[events]] will terminate. */
  def complete: F[Unit] = queue.offer(None)

object TestGatewayClient:

  /** Create a fresh [[TestGatewayClient]] backed by an unbounded queue. */
  def create[F[_]: Concurrent]: F[TestGatewayClient[F]] =
    Queue.unbounded[F, Option[GatewayEvent]].map(new TestGatewayClient(_))
