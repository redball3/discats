package discats.models

import io.circe.*
import io.circe.generic.semiauto.*
import io.circe.syntax.*
import io.circe.derivation.{Configuration, ConfiguredDecoder, ConfiguredEncoder}

/** Discord interaction types. */
enum InteractionType(val value: Int):
  case Ping                           extends InteractionType(1)
  case ApplicationCommand             extends InteractionType(2)
  case MessageComponent               extends InteractionType(3)
  case ApplicationCommandAutocomplete extends InteractionType(4)
  case ModalSubmit                    extends InteractionType(5)
  case Unknown                        extends InteractionType(-1)

object InteractionType:
  def fromInt(i: Int): InteractionType = values.find(_.value == i).getOrElse(Unknown)
  given Encoder[InteractionType] = Encoder.encodeInt.contramap(_.value)
  given Decoder[InteractionType] = Decoder.decodeInt.map(fromInt)

/** A resolved option value from a slash command interaction. */
final case class InteractionOption(
    name: String,
    `type`: CommandOptionType,
    value: Option[Json],
    options: Option[List[InteractionOption]],
    focused: Option[Boolean],
)

object InteractionOption:
  given Decoder[InteractionOption] = deriveDecoder
  given Encoder[InteractionOption] = deriveEncoder

/** A Discord attachment, as returned in resolved interaction data. */
final case class Attachment(
    id: Snowflake,
    filename: String,
    url: String,
    proxyUrl: String,
    size: Int,
    contentType: Option[String] = None,
)

object Attachment:
  private given Configuration = Configuration.default.withSnakeCaseMemberNames
  given Decoder[Attachment] = ConfiguredDecoder.derived
  given Encoder[Attachment] = ConfiguredEncoder.derived

/** Resolved entities referenced by options (users, members, channels, roles, attachments). */
final case class ResolvedData(
    users: Option[Map[String, User]],
    members: Option[Map[String, GuildMember]],
    channels: Option[Map[String, Channel]],
    attachments: Option[Map[String, Attachment]] = None,
)

object ResolvedData:
  given Decoder[ResolvedData] = deriveDecoder
  given Encoder[ResolvedData] = deriveEncoder

/** Data payload for a MessageComponent interaction (button click, select menu choice, etc.). */
final case class MessageComponentData(
    customId: String,
    componentType: Int,
    values: Option[List[String]],
)

object MessageComponentData:
  private given Configuration = Configuration.default.withSnakeCaseMemberNames
  given Decoder[MessageComponentData] = ConfiguredDecoder.derived
  given Encoder[MessageComponentData] = ConfiguredEncoder.derived

/** Data payload for an ApplicationCommand interaction. */
final case class ApplicationCommandData(
    id: Snowflake,
    name: String,
    `type`: Int,
    resolved: Option[ResolvedData],
    options: Option[List[InteractionOption]],
    guildId: Option[Snowflake],
    targetId: Option[Snowflake],
)

object ApplicationCommandData:
  private given Configuration = Configuration.default.withSnakeCaseMemberNames
  given Decoder[ApplicationCommandData] = ConfiguredDecoder.derived
  given Encoder[ApplicationCommandData] = ConfiguredEncoder.derived

/** Common interface for all Discord interaction subtypes.
  *
  * Pattern match on [[GuildInteraction]] or [[ChannelInteraction]] to access
  * context-specific fields:
  * {{{
  * interaction match
  *   case g: GuildInteraction   => s"From guild ${g.guildId}, member ${g.member.nick}"
  *   case c: ChannelInteraction => s"From DM, user ${c.user.username}"
  * }}}
  */
sealed trait Interaction:
  def id: Snowflake
  def applicationId: Snowflake
  def `type`: InteractionType
  def data: Option[ApplicationCommandData]
  def componentData: Option[MessageComponentData]
  def channelId: Option[Snowflake]
  def token: String
  def version: Int

  /** The name of the command, if this is an application command interaction. */
  def commandName: Option[String] =
    if `type` == InteractionType.ApplicationCommand then data.map(_.name) else None

  /** Look up a string option by name. */
  def stringOption(name: String): Option[String] =
    data.flatMap(_.options).flatMap(_.find(_.name == name)).flatMap(_.value).flatMap(_.asString)

  /** Look up an integer option by name. */
  def intOption(name: String): Option[Long] =
    data.flatMap(_.options).flatMap(_.find(_.name == name)).flatMap(_.value).flatMap(_.asNumber).flatMap(_.toLong)

  /** Look up a boolean option by name. */
  def boolOption(name: String): Option[Boolean] =
    data.flatMap(_.options).flatMap(_.find(_.name == name)).flatMap(_.value).flatMap(_.asBoolean)

  /** Look up an attachment option by name, returning the resolved [[Attachment]]. */
  def attachmentOption(name: String): Option[Attachment] =
    for
      attachmentId <- data.flatMap(_.options).flatMap(_.find(_.name == name)).flatMap(_.value).flatMap(_.asString)
      resolved     <- data.flatMap(_.resolved)
      attachments  <- resolved.attachments
      attachment   <- attachments.get(attachmentId)
    yield attachment

  /** The custom_id of a MessageComponent interaction. */
  def componentCustomId: Option[String] = componentData.map(_.customId)

  /** The selected values of a MessageComponent select menu interaction. */
  def componentValues: Option[List[String]] = componentData.flatMap(_.values)

object Interaction:
  /** Decodes as [[GuildInteraction]] when `guild_id` is present, otherwise [[ChannelInteraction]]. */
  given Decoder[Interaction] = (c: HCursor) =>
    c.downField("guild_id").as[Option[Snowflake]].flatMap {
      case Some(_) => c.as[GuildInteraction]
      case None    => c.as[ChannelInteraction]
    }

  given Encoder[Interaction] = Encoder.instance {
    case g: GuildInteraction   => g.asJson
    case c: ChannelInteraction => c.asJson
  }

/** An interaction triggered within a guild.
  *
  * Both `guildId` and `member` are guaranteed to be present — use this type
  * when your command logic needs guild or member context.
  */
final case class GuildInteraction(
    id: Snowflake,
    applicationId: Snowflake,
    `type`: InteractionType,
    data: Option[ApplicationCommandData],
    guildId: Snowflake,
    channelId: Option[Snowflake],
    member: GuildMember,
    token: String,
    version: Int,
    componentData: Option[MessageComponentData] = None,
) extends Interaction:
  /** The guild member who triggered this interaction. May be absent for bots. */
  def invoker: Option[User] = member.user

object GuildInteraction:
  given Decoder[GuildInteraction] = (c: HCursor) =>
    for
      id            <- c.downField("id").as[Snowflake]
      applicationId <- c.downField("application_id").as[Snowflake]
      interType     <- c.downField("type").as[InteractionType]
      guildId       <- c.downField("guild_id").as[Snowflake]
      channelId     <- c.downField("channel_id").as[Option[Snowflake]]
      member        <- c.downField("member").as[GuildMember]
      token         <- c.downField("token").as[String]
      version       <- c.downField("version").as[Int]
      commandData   <- {
                        if interType == InteractionType.ApplicationCommand then
                          c.downField("data").as[Option[ApplicationCommandData]]
                        else Right(None)
                      }
      componentData <- {
                        if interType == InteractionType.MessageComponent then
                          c.downField("data").as[Option[MessageComponentData]]
                        else Right(None)
                      }
    yield GuildInteraction(id, applicationId, interType, commandData, guildId, channelId, member, token, version, componentData)

  given Encoder[GuildInteraction] = (g: GuildInteraction) =>
    val dataJson = g.data.map(_.asJson).orElse(g.componentData.map(_.asJson))
    Json.obj(
      "id"             -> g.id.asJson,
      "application_id" -> g.applicationId.asJson,
      "type"           -> g.`type`.asJson,
      "data"           -> dataJson.getOrElse(Json.Null),
      "guild_id"       -> g.guildId.asJson,
      "channel_id"     -> g.channelId.asJson,
      "member"         -> g.member.asJson,
      "token"          -> g.token.asJson,
      "version"        -> g.version.asJson,
    )

/** An interaction triggered outside a guild (DM or group DM).
  *
  * `user` is guaranteed to be present — use this type when your command
  * logic needs the invoking user directly.
  */
final case class ChannelInteraction(
    id: Snowflake,
    applicationId: Snowflake,
    `type`: InteractionType,
    data: Option[ApplicationCommandData],
    channelId: Option[Snowflake],
    user: User,
    token: String,
    version: Int,
    componentData: Option[MessageComponentData] = None,
) extends Interaction:
  /** The user who triggered this interaction. */
  def invoker: User = user

object ChannelInteraction:
  given Decoder[ChannelInteraction] = (c: HCursor) =>
    for
      id            <- c.downField("id").as[Snowflake]
      applicationId <- c.downField("application_id").as[Snowflake]
      interType     <- c.downField("type").as[InteractionType]
      channelId     <- c.downField("channel_id").as[Option[Snowflake]]
      user          <- c.downField("user").as[User]
      token         <- c.downField("token").as[String]
      version       <- c.downField("version").as[Int]
      commandData   <- {
                        if interType == InteractionType.ApplicationCommand then
                          c.downField("data").as[Option[ApplicationCommandData]]
                        else Right(None)
                      }
      componentData <- {
                        if interType == InteractionType.MessageComponent then
                          c.downField("data").as[Option[MessageComponentData]]
                        else Right(None)
                      }
    yield ChannelInteraction(id, applicationId, interType, commandData, channelId, user, token, version, componentData)

  given Encoder[ChannelInteraction] = (c: ChannelInteraction) =>
    val dataJson = c.data.map(_.asJson).orElse(c.componentData.map(_.asJson))
    Json.obj(
      "id"             -> c.id.asJson,
      "application_id" -> c.applicationId.asJson,
      "type"           -> c.`type`.asJson,
      "data"           -> dataJson.getOrElse(Json.Null),
      "channel_id"     -> c.channelId.asJson,
      "user"           -> c.user.asJson,
      "token"          -> c.token.asJson,
      "version"        -> c.version.asJson,
    )

// ── Interaction Responses ────────────────────────────────────────────────────

/** Type of interaction response. */
enum InteractionCallbackType(val value: Int):
  case Pong                             extends InteractionCallbackType(1)
  case ChannelMessageWithSource         extends InteractionCallbackType(4)
  case DeferredChannelMessageWithSource extends InteractionCallbackType(5)
  case DeferredUpdateMessage            extends InteractionCallbackType(6)
  case UpdateMessage                    extends InteractionCallbackType(7)
  case ApplicationCommandAutocomplete   extends InteractionCallbackType(8)
  case Modal                            extends InteractionCallbackType(9)

object InteractionCallbackType:
  given Encoder[InteractionCallbackType] = Encoder.encodeInt.contramap(_.value)
  given Decoder[InteractionCallbackType] = Decoder.decodeInt.map(i => values.find(_.value == i).getOrElse(ChannelMessageWithSource))

/** Message flags (can be combined). */
object MessageFlags:
  val Ephemeral: Int = 1 << 6 // visible only to the invoking user

/** The data payload of an interaction response. */
final case class InteractionCallbackData(
    content: Option[String] = None,
    embeds: Option[List[Embed]] = None,
    flags: Option[Int] = None,
    tts: Option[Boolean] = None,
    components: Option[List[ActionRow]] = None,
) {
  def asEphemeral: InteractionCallbackData = copy(flags = Some(flags.getOrElse(0) | MessageFlags.Ephemeral))
}

object InteractionCallbackData:
  given Encoder[InteractionCallbackData] = (d: InteractionCallbackData) =>
    Json.obj(
      "content"    -> d.content.fold(Json.Null)(Json.fromString),
      "embeds"     -> d.embeds.fold(Json.Null)(_.asJson),
      "flags"      -> d.flags.fold(Json.Null)(Json.fromInt),
      "tts"        -> d.tts.fold(Json.Null)(Json.fromBoolean),
      "components" -> d.components.fold(Json.Null)(_.asJson),
    ).dropNullValues

  given Decoder[InteractionCallbackData] = deriveDecoder

  def text(content: String): InteractionCallbackData                     = InteractionCallbackData(content = Some(content))
  def ephemeral(content: String): InteractionCallbackData                = text(content).asEphemeral
  def withEmbeds(embeds: List[Embed]): InteractionCallbackData           = InteractionCallbackData(embeds = Some(embeds))
  def withSelectMenu(menu: StringSelectMenu): InteractionCallbackData    = InteractionCallbackData(components = Some(List(ActionRow(menu))))
  def withSelectMenu(content: String, menu: StringSelectMenu): InteractionCallbackData =
    InteractionCallbackData(content = Some(content), components = Some(List(ActionRow(menu))))

/** A full interaction response (type + optional data). */
final case class InteractionResponse(
    `type`: InteractionCallbackType,
    data: Option[InteractionCallbackData] = None,
)

object InteractionResponse:
  given Encoder[InteractionResponse] = deriveEncoder
  given Decoder[InteractionResponse] = deriveDecoder

  def reply(content: String): InteractionResponse =
    InteractionResponse(InteractionCallbackType.ChannelMessageWithSource, Some(InteractionCallbackData.text(content)))

  def replyEphemeral(content: String): InteractionResponse =
    InteractionResponse(InteractionCallbackType.ChannelMessageWithSource, Some(InteractionCallbackData.ephemeral(content)))

  def defer(ephemeral: Boolean = false): InteractionResponse =
    InteractionResponse(
      InteractionCallbackType.DeferredChannelMessageWithSource,
      if ephemeral then Some(InteractionCallbackData(flags = Some(MessageFlags.Ephemeral))) else None,
    )

  def withEmbeds(embeds: List[Embed]): InteractionResponse =
    InteractionResponse(InteractionCallbackType.ChannelMessageWithSource, Some(InteractionCallbackData.withEmbeds(embeds)))

  def replyWithSelectMenu(menu: StringSelectMenu): InteractionResponse =
    InteractionResponse(
      InteractionCallbackType.ChannelMessageWithSource,
      Some(InteractionCallbackData.withSelectMenu(menu).asEphemeral),
    )

  def replyWithSelectMenu(content: String, menu: StringSelectMenu): InteractionResponse =
    InteractionResponse(
      InteractionCallbackType.ChannelMessageWithSource,
      Some(InteractionCallbackData.withSelectMenu(content, menu).asEphemeral),
    )
