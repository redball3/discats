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

/** Resolved entities referenced by options (users, members, channels, roles). */
final case class ResolvedData(
    users: Option[Map[String, User]],
    members: Option[Map[String, GuildMember]],
    channels: Option[Map[String, Channel]],
)

object ResolvedData:
  given Decoder[ResolvedData] = deriveDecoder
  given Encoder[ResolvedData] = deriveEncoder

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
) extends Interaction:
  /** The guild member who triggered this interaction. May be absent for bots. */
  def invoker: Option[User] = member.user

object GuildInteraction:
  private given Configuration = Configuration.default.withSnakeCaseMemberNames
  given Decoder[GuildInteraction] = ConfiguredDecoder.derived
  given Encoder[GuildInteraction] = ConfiguredEncoder.derived

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
) extends Interaction:
  /** The user who triggered this interaction. */
  def invoker: User = user

object ChannelInteraction:
  private given Configuration = Configuration.default.withSnakeCaseMemberNames
  given Decoder[ChannelInteraction] = ConfiguredDecoder.derived
  given Encoder[ChannelInteraction] = ConfiguredEncoder.derived

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
) {
  def asEphemeral: InteractionCallbackData = copy(flags = Some(flags.getOrElse(0) | MessageFlags.Ephemeral))
}

object InteractionCallbackData:
  given Encoder[InteractionCallbackData] = deriveEncoder
  given Decoder[InteractionCallbackData] = deriveDecoder

  def text(content: String): InteractionCallbackData                     = InteractionCallbackData(content = Some(content))
  def ephemeral(content: String): InteractionCallbackData                = text(content).asEphemeral
  def withEmbeds(embeds: List[Embed]): InteractionCallbackData           = InteractionCallbackData(embeds = Some(embeds))

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
