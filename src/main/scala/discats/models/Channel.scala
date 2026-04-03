package discats.models

import io.circe.generic.semiauto.*
import io.circe.{Decoder, Encoder}
import io.circe.derivation.{Configuration, ConfiguredDecoder, ConfiguredEncoder}

/** Channel types as defined by Discord. */
enum ChannelType(val value: Int):
  case GuildText     extends ChannelType(0)
  case DM            extends ChannelType(1)
  case GuildVoice    extends ChannelType(2)
  case GroupDM       extends ChannelType(3)
  case GuildCategory extends ChannelType(4)
  case GuildNews     extends ChannelType(5)
  case GuildForum    extends ChannelType(15)
  case Unknown       extends ChannelType(-1)

object ChannelType:
  def fromInt(i: Int): ChannelType =
    values.find(_.value == i).getOrElse(Unknown)

  given Decoder[ChannelType] = Decoder.decodeInt.map(fromInt)
  given Encoder[ChannelType] = Encoder.encodeInt.contramap(_.value)

final case class Channel(
    id: Snowflake,
    `type`: ChannelType,
    guildId: Option[Snowflake],
    name: Option[String],
    topic: Option[String],
    nsfw: Option[Boolean],
    lastMessageId: Option[Snowflake],
    position: Option[Int],
    parentId: Option[Snowflake],
)

object Channel:
  private given Configuration = Configuration.default.withSnakeCaseMemberNames
  given Decoder[Channel] = ConfiguredDecoder.derived
  given Encoder[Channel] = ConfiguredEncoder.derived

/** A permission overwrite for a channel.
  *
  * @param id   The role or user Snowflake ID.
  * @param `type` 0 = role, 1 = member.
  * @param allow Permission bits to allow, as a decimal string.
  * @param deny  Permission bits to deny, as a decimal string.
  */
final case class PermissionOverwrite(
    id: String,
    `type`: Int,
    allow: String,
    deny: String,
)

object PermissionOverwrite:
  given Encoder[PermissionOverwrite] =
    Encoder.forProduct4("id", "type", "allow", "deny")(o => (o.id, o.`type`, o.allow, o.deny))
