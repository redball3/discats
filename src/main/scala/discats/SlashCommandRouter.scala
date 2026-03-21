package discats

import cats.effect.*
import cats.effect.syntax.all.*
import cats.syntax.all.*
import discats.gateway.GatewayEvent
import discats.models.*
import discats.rest.RestClient
import fs2.Stream

/** A builder for routing slash command interactions to typed handlers.
  *
  * Usage:
  * {{{
  * val router = SlashCommandRouter[IO]
  *   .on("ping") { _ => IO.pure(InteractionResponse.reply("Pong!")) }
  *   .on("echo") { interaction =>
  *     val text = interaction.stringOption("text").getOrElse("(nothing)")
  *     IO.pure(InteractionResponse.reply(text))
  *   }
  *
  * client.events
  *   .through(router.pipe(client))
  *   .compile.drain
  * }}}
  */
final class SlashCommandRouter[F[_]: Async] private (
    handlers: Map[String, Interaction => F[InteractionResponse]],
    fallback: Option[Interaction => F[InteractionResponse]],
    applicationId: Option[Snowflake],
):
  /** Register a handler for the given slash command name. */
  def on(name: String)(handler: Interaction => F[InteractionResponse]): SlashCommandRouter[F] =
    new SlashCommandRouter(handlers + (name -> handler), fallback, applicationId)

  /** Register a handler for the given slash command name (simple text reply). */
  def onText(name: String)(handler: Interaction => F[String]): SlashCommandRouter[F] =
    on(name)(i => handler(i).map(InteractionResponse.reply))

  /** Register a catch-all handler for unrecognised commands. */
  def withFallback(handler: Interaction => F[InteractionResponse]): SlashCommandRouter[F] =
    new SlashCommandRouter(handlers, Some(handler), applicationId)

  /** Set the application ID used for editing/deleting deferred responses.
    * Required if you use [[RestClient.editInteractionResponse]].
    */
  def withApplicationId(id: Snowflake): SlashCommandRouter[F] =
    new SlashCommandRouter(handlers, fallback, Some(id))

  /** Dispatch a single interaction: look up the handler and respond via the REST client. */
  def dispatch(interaction: Interaction, client: RestClient[F]): F[Unit] =
    interaction.commandName match
      case Some(name) =>
        handlers.get(name).orElse(fallback) match
          case Some(handler) =>
            handler(interaction)
              .flatMap(response => client.respondToInteraction(interaction.id, interaction.token, response))
              .handleErrorWith { err =>
                client.respondToInteraction(
                  interaction.id,
                  interaction.token,
                  InteractionResponse.replyEphemeral(s"Internal error: ${err.getMessage}"),
                )
              }
          case None =>
            client.respondToInteraction(
              interaction.id,
              interaction.token,
              InteractionResponse.replyEphemeral(s"Unknown command: $name"),
            )
      case None =>
        Async[F].unit

  /** An fs2 pipe that consumes a stream of [[GatewayEvent]]s, routes any
    * [[GatewayEvent.InteractionCreate]] events to the registered handlers,
    * and re-emits all events unchanged.
    *
    * Side effects (responding to Discord) run concurrently via a background fiber per interaction.
    */
  def pipe(client: RestClient[F]): fs2.Pipe[F, GatewayEvent, GatewayEvent] =
    _.evalTap {
      case GatewayEvent.InteractionCreate(interaction) =>
        dispatch(interaction, client).start.void
      case _ =>
        Async[F].unit
    }

  /** Variant that only emits [[GatewayEvent.InteractionCreate]] events not handled here (pass-through filter). */
  def handleStream(events: Stream[F, GatewayEvent], client: RestClient[F]): Stream[F, GatewayEvent] =
    events.through(pipe(client))

object SlashCommandRouter:
  def apply[F[_]: Async]: SlashCommandRouter[F] =
    new SlashCommandRouter(Map.empty, None, None)
