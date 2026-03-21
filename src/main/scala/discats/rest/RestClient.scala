package discats.rest

import cats.effect.*
import cats.syntax.all.*
import discats.DiscordConfig
import discats.models.{ApplicationCommand, Channel, Guild, Interaction, InteractionResponse, Message as DiscordMessage, MessageCreate, RegisteredCommand, Snowflake}
import io.circe.*
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.*
import org.http4s.client.Client
import org.typelevel.ci.CIStringSyntax

/** Algebra for the Discord REST API. */
trait RestClient[F[_]]:

  // ── Channels ──────────────────────────────────────────────────────────
  def getChannel(channelId: Snowflake): F[Channel]

  // ── Messages ──────────────────────────────────────────────────────────
  def getMessage(channelId: Snowflake, messageId: Snowflake): F[DiscordMessage]
  def getMessages(channelId: Snowflake, limit: Int = 50): F[List[DiscordMessage]]
  def sendMessage(channelId: Snowflake, body: MessageCreate): F[DiscordMessage]
  def deleteMessage(channelId: Snowflake, messageId: Snowflake): F[Unit]
  def editMessage(channelId: Snowflake, messageId: Snowflake, body: MessageCreate): F[DiscordMessage]

  // ── Guilds ────────────────────────────────────────────────────────────
  def getGuild(guildId: Snowflake): F[Guild]
  def getGuildChannels(guildId: Snowflake): F[List[Channel]]

  // ── Application commands ───────────────────────────────────────────────
  /** Register (upsert) a global slash command. Changes take ~1 hour to propagate. */
  def registerGlobalCommand(applicationId: Snowflake, command: ApplicationCommand): F[RegisteredCommand]

  /** Register (upsert) a guild-scoped slash command. Active instantly. */
  def registerGuildCommand(applicationId: Snowflake, guildId: Snowflake, command: ApplicationCommand): F[RegisteredCommand]

  /** List all global commands for this application. */
  def listGlobalCommands(applicationId: Snowflake): F[List[RegisteredCommand]]

  /** Delete a global command by ID. */
  def deleteGlobalCommand(applicationId: Snowflake, commandId: Snowflake): F[Unit]

  /** Delete a guild command by ID. */
  def deleteGuildCommand(applicationId: Snowflake, guildId: Snowflake, commandId: Snowflake): F[Unit]

  // ── Interaction responses ──────────────────────────────────────────────
  /** Send the initial response to an interaction (must be called within 3 seconds). */
  def respondToInteraction(interactionId: Snowflake, token: String, response: InteractionResponse): F[Unit]

  /** Edit the original interaction response (for deferred replies). */
  def editInteractionResponse(applicationId: Snowflake, token: String, body: MessageCreate): F[DiscordMessage]

  /** Delete the original interaction response. */
  def deleteInteractionResponse(applicationId: Snowflake, token: String): F[Unit]

object RestClient:

  def apply[F[_]: Async](
      client: Client[F],
      config: DiscordConfig,
      rateLimiter: RateLimiter[F],
  ): RestClient[F] = new Impl(client, config, rateLimiter)

  private class Impl[F[_]](
      client: Client[F],
      config: DiscordConfig,
      rl: RateLimiter[F],
  )(using F: Async[F])
      extends RestClient[F]:

    private val base = config.apiBase

    private def authHeader: Header.Raw =
      Header.Raw(ci"Authorization", config.authHeader)

    private def jsonHeader: Header.Raw =
      Header.Raw(ci"Content-Type", "application/json")

    // ── Request helpers ──────────────────────────────────────────────────

    private def get[A: Decoder](path: String): F[A] =
      val bucketKey = path
      rl(bucketKey) {
        val req = Request[F](
          method  = Method.GET,
          uri     = Uri.unsafeFromString(s"$base$path"),
          headers = Headers(authHeader),
        )
        client.run(req).use { resp =>
          rl.updateFromHeaders(bucketKey, resp.headers) >>
            expectSuccess(resp) >>
            resp.as[Json].flatMap(json => F.fromEither(json.as[A]))
        }
      }

    private def post[B: Encoder, A: Decoder](path: String, body: B): F[A] =
      val bucketKey = path
      rl(bucketKey) {
        val req = Request[F](
          method  = Method.POST,
          uri     = Uri.unsafeFromString(s"$base$path"),
          headers = Headers(authHeader, jsonHeader),
          body    = fs2.Stream.emits(body.asJson.noSpaces.getBytes),
        )
        client.run(req).use { resp =>
          rl.updateFromHeaders(bucketKey, resp.headers) >>
            expectSuccess(resp) >>
            resp.as[Json].flatMap(json => F.fromEither(json.as[A]))
        }
      }

    private def patch[B: Encoder, A: Decoder](path: String, body: B): F[A] =
      val bucketKey = path
      rl(bucketKey) {
        val req = Request[F](
          method  = Method.PATCH,
          uri     = Uri.unsafeFromString(s"$base$path"),
          headers = Headers(authHeader, jsonHeader),
          body    = fs2.Stream.emits(body.asJson.noSpaces.getBytes),
        )
        client.run(req).use { resp =>
          rl.updateFromHeaders(bucketKey, resp.headers) >>
            expectSuccess(resp) >>
            resp.as[Json].flatMap(json => F.fromEither(json.as[A]))
        }
      }

    private def postUnit[B: Encoder](path: String, body: B): F[Unit] =
      val bucketKey = path
      rl(bucketKey) {
        val req = Request[F](
          method  = Method.POST,
          uri     = Uri.unsafeFromString(s"$base$path"),
          headers = Headers(authHeader, jsonHeader),
          body    = fs2.Stream.emits(body.asJson.noSpaces.getBytes),
        )
        client.run(req).use { resp =>
          rl.updateFromHeaders(bucketKey, resp.headers) >>
            expectSuccess(resp).void
        }
      }

    private def delete(path: String): F[Unit] =
      val bucketKey = path
      rl(bucketKey) {
        val req = Request[F](
          method  = Method.DELETE,
          uri     = Uri.unsafeFromString(s"$base$path"),
          headers = Headers(authHeader),
        )
        client.run(req).use { resp =>
          rl.updateFromHeaders(bucketKey, resp.headers) >>
            expectSuccess(resp).void
        }
      }

    private def expectSuccess(resp: Response[F]): F[Unit] =
      if resp.status.isSuccess then F.unit
      else resp.as[String].flatMap(body => F.raiseError(DiscordApiException(resp.status.code, body)))

    // ── Implementations ──────────────────────────────────────────────────

    def getChannel(channelId: Snowflake): F[Channel] =
      get[Channel](s"/channels/${channelId.asString}")

    def getMessage(channelId: Snowflake, messageId: Snowflake): F[DiscordMessage] =
      get[DiscordMessage](s"/channels/${channelId.asString}/messages/${messageId.asString}")

    def getMessages(channelId: Snowflake, limit: Int): F[List[DiscordMessage]] =
      get[List[DiscordMessage]](s"/channels/${channelId.asString}/messages?limit=$limit")

    def sendMessage(channelId: Snowflake, body: MessageCreate): F[DiscordMessage] =
      post[MessageCreate, DiscordMessage](s"/channels/${channelId.asString}/messages", body)

    def editMessage(channelId: Snowflake, messageId: Snowflake, body: MessageCreate): F[DiscordMessage] =
      patch[MessageCreate, DiscordMessage](s"/channels/${channelId.asString}/messages/${messageId.asString}", body)

    def deleteMessage(channelId: Snowflake, messageId: Snowflake): F[Unit] =
      delete(s"/channels/${channelId.asString}/messages/${messageId.asString}")

    def getGuild(guildId: Snowflake): F[Guild] =
      get[Guild](s"/guilds/${guildId.asString}")

    def getGuildChannels(guildId: Snowflake): F[List[Channel]] =
      get[List[Channel]](s"/guilds/${guildId.asString}/channels")

    def registerGlobalCommand(applicationId: Snowflake, command: ApplicationCommand): F[RegisteredCommand] =
      post[ApplicationCommand, RegisteredCommand](s"/applications/${applicationId.asString}/commands", command)

    def registerGuildCommand(applicationId: Snowflake, guildId: Snowflake, command: ApplicationCommand): F[RegisteredCommand] =
      post[ApplicationCommand, RegisteredCommand](s"/applications/${applicationId.asString}/guilds/${guildId.asString}/commands", command)

    def listGlobalCommands(applicationId: Snowflake): F[List[RegisteredCommand]] =
      get[List[RegisteredCommand]](s"/applications/${applicationId.asString}/commands")

    def deleteGlobalCommand(applicationId: Snowflake, commandId: Snowflake): F[Unit] =
      delete(s"/applications/${applicationId.asString}/commands/${commandId.asString}")

    def deleteGuildCommand(applicationId: Snowflake, guildId: Snowflake, commandId: Snowflake): F[Unit] =
      delete(s"/applications/${applicationId.asString}/guilds/${guildId.asString}/commands/${commandId.asString}")

    def respondToInteraction(interactionId: Snowflake, token: String, response: InteractionResponse): F[Unit] =
      postUnit[InteractionResponse](s"/interactions/${interactionId.asString}/$token/callback", response)

    def editInteractionResponse(applicationId: Snowflake, token: String, body: MessageCreate): F[DiscordMessage] =
      patch[MessageCreate, DiscordMessage](s"/webhooks/${applicationId.asString}/$token/messages/@original", body)

    def deleteInteractionResponse(applicationId: Snowflake, token: String): F[Unit] =
      delete(s"/webhooks/${applicationId.asString}/$token/messages/@original")

final case class DiscordApiException(status: Int, body: String)
    extends Exception(s"Discord API error $status: $body")
