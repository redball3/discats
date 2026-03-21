package discats.gateway

import discats.models.*
import io.circe.*
import io.circe.generic.semiauto.*

/** Typed ADT for Gateway dispatch events.
  * Unknown/unhandled events are wrapped in [[GatewayEvent.Unknown]].
  */
enum GatewayEvent:
  case Ready(
      v: Int,
      user: User,
      sessionId: String,
      resumeGatewayUrl: String,
      application: Json,
  )
  case Resumed
  case MessageCreate(message: Message)
  case MessageUpdate(message: Message)
  case MessageDelete(
      id: Snowflake,
      channelId: Snowflake,
      guildId: Option[Snowflake],
  )
  case GuildCreate(guild: Guild)
  case GuildDelete(id: Snowflake, unavailable: Option[Boolean])
  case ChannelCreate(channel: Channel)
  case ChannelUpdate(channel: Channel)
  case ChannelDelete(channel: Channel)
  case InteractionCreate(interaction: Interaction)
  case Unknown(eventName: String, raw: Option[Json])

object GatewayEvent:
  private given Decoder[Ready] = (c: HCursor) =>
    for
      v      <- c.downField("v").as[Int]
      user   <- c.downField("user").as[User]
      sid    <- c.downField("session_id").as[String]
      resume <- c.downField("resume_gateway_url").as[String]
      app    <- c.downField("application").as[Json]
    yield Ready(v, user, sid, resume, app)

  private given Decoder[MessageDelete] = (c: HCursor) =>
    for
      id      <- c.downField("id").as[Snowflake]
      chanId  <- c.downField("channel_id").as[Snowflake]
      guildId <- c.downField("guild_id").as[Option[Snowflake]]
    yield MessageDelete(id, chanId, guildId)

  private given Decoder[GuildDelete] = (c: HCursor) =>
    for
      id          <- c.downField("id").as[Snowflake]
      unavailable <- c.downField("unavailable").as[Option[Boolean]]
    yield GuildDelete(id, unavailable)

  /** Decode a [[GatewayPayload]] into a [[GatewayEvent]].
    * Returns [[None]] for non-dispatch payloads.
    */
  def fromPayload(payload: GatewayPayload): Option[GatewayEvent] =
    if payload.op != Opcode.Dispatch then None
    else
      val data = payload.d
      payload.t match
        case Some("READY") =>
          data.flatMap(_.as[Ready].toOption).map(identity)
        case Some("RESUMED") =>
          Some(Resumed)
        case Some("MESSAGE_CREATE") =>
          data.flatMap(_.as[Message].toOption).map(MessageCreate(_))
        case Some("MESSAGE_UPDATE") =>
          data.flatMap(_.as[Message].toOption).map(MessageUpdate(_))
        case Some("MESSAGE_DELETE") =>
          data.flatMap(_.as[MessageDelete].toOption)
        case Some("GUILD_CREATE") =>
          data.flatMap(_.as[Guild].toOption).map(GuildCreate(_))
        case Some("GUILD_DELETE") =>
          data.flatMap(_.as[GuildDelete].toOption)
        case Some("CHANNEL_CREATE") =>
          data.flatMap(_.as[Channel].toOption).map(ChannelCreate(_))
        case Some("CHANNEL_UPDATE") =>
          data.flatMap(_.as[Channel].toOption).map(ChannelUpdate(_))
        case Some("CHANNEL_DELETE") =>
          data.flatMap(_.as[Channel].toOption).map(ChannelDelete(_))
        case Some("INTERACTION_CREATE") =>
          data.flatMap(_.as[Interaction].toOption).map(InteractionCreate(_))
        case Some(name) =>
          Some(Unknown(name, data))
        case None =>
          None
