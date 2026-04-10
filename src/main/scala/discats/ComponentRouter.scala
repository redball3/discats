package discats

import cats.effect.*
import cats.effect.syntax.all.*
import cats.syntax.all.*
import discats.gateway.GatewayEvent
import discats.models.*
import discats.rest.RestClient

/** A builder for routing MessageComponent interactions (button clicks, select menu selections)
  * to typed handlers.
  *
  * Component interactions are identified by a `custom_id` string that you set when building the
  * component. Use [[on]] for exact matches or [[onPrefix]] when your `custom_id` encodes extra
  * data (e.g. `"submit_task:1234567890"`).
  *
  * Usage:
  * {{{
  * val componentRouter = ComponentRouter[IO]
  *   .on("confirm_button") { interaction =>
  *     IO.pure(InteractionResponse.reply("Confirmed!"))
  *   }
  *   .onPrefix("submit_task:") { interaction =>
  *     val taskId = interaction.componentCustomId.getOrElse("").stripPrefix("submit_task:")
  *     val areas  = interaction.componentValues.getOrElse(Nil)
  *     IO.pure(InteractionResponse.reply(s"Submitted task $taskId for areas: ${areas.mkString(", ")}"))
  *   }
  *
  * client.events
  *   .through(commandRouter.pipe(client))
  *   .through(componentRouter.pipe(client))
  *   .compile.drain
  * }}}
  */
final class ComponentRouter[F[_]: Async] private (
    exact: Map[String, Interaction => F[InteractionResponse]],
    prefixes: List[(String, Interaction => F[InteractionResponse])],
    fallback: Option[Interaction => F[InteractionResponse]],
):
  /** Register a handler for an exact custom_id match. */
  def on(customId: String)(handler: Interaction => F[InteractionResponse]): ComponentRouter[F] =
    new ComponentRouter(exact + (customId -> handler), prefixes, fallback)

  /** Register a handler for any custom_id that starts with the given prefix. */
  def onPrefix(prefix: String)(handler: Interaction => F[InteractionResponse]): ComponentRouter[F] =
    new ComponentRouter(exact, prefixes :+ (prefix -> handler), fallback)

  /** Register a catch-all handler for unrecognised component custom IDs. */
  def withFallback(handler: Interaction => F[InteractionResponse]): ComponentRouter[F] =
    new ComponentRouter(exact, prefixes, Some(handler))

  private def findHandler(customId: String): Option[Interaction => F[InteractionResponse]] =
    exact
      .get(customId)
      .orElse(prefixes.collectFirst { case (prefix, h) if customId.startsWith(prefix) => h })
      .orElse(fallback)

  /** Dispatch a single MessageComponent interaction. No-ops for other interaction types. */
  def dispatch(interaction: Interaction, client: RestClient[F]): F[Unit] =
    if interaction.`type` != InteractionType.MessageComponent then Async[F].unit
    else
      interaction.componentCustomId match
        case Some(customId) =>
          findHandler(customId) match
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
              Async[F].unit
        case None =>
          Async[F].unit

  /** An fs2 pipe that routes [[GatewayEvent.InteractionCreate]] events where the interaction type
    * is [[InteractionType.MessageComponent]] to the registered handlers.
    *
    * All events are re-emitted unchanged so multiple pipes can be composed.
    */
  def pipe(client: RestClient[F]): fs2.Pipe[F, GatewayEvent, GatewayEvent] =
    _.evalTap {
      case GatewayEvent.InteractionCreate(interaction)
          if interaction.`type` == InteractionType.MessageComponent =>
        dispatch(interaction, client).start.void
      case _ =>
        Async[F].unit
    }

object ComponentRouter:
  def apply[F[_]: Async]: ComponentRouter[F] =
    new ComponentRouter(Map.empty, Nil, None)
