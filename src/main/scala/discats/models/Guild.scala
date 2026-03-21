package discats.models

import io.circe.generic.semiauto.*
import io.circe.{Decoder, Encoder}
import io.circe.derivation.{Configuration, ConfiguredDecoder, ConfiguredEncoder}

final case class GuildMember(
    user: Option[User],
    nick: Option[String],
    roles: List[Snowflake],
    joinedAt: String,
    deaf: Boolean,
    mute: Boolean,
)

object GuildMember:
  private given Configuration = Configuration.default.withSnakeCaseMemberNames
  given Decoder[GuildMember] = ConfiguredDecoder.derived
  given Encoder[GuildMember] = ConfiguredEncoder.derived

final case class Guild(
    id: Snowflake,
    name: String,
    icon: Option[String],
    splash: Option[String],
    ownerId: Snowflake,
    afkChannelId: Option[Snowflake],
    afkTimeout: Int,
    verificationLevel: Int,
    defaultMessageNotifications: Int,
    explicitContentFilter: Int,
    roles: List[io.circe.Json],
    emojis: List[io.circe.Json],
    features: List[String],
    memberCount: Option[Int],
    channels: Option[List[Channel]],
    members: Option[List[GuildMember]],
)

object Guild:
  private given Configuration = Configuration.default.withSnakeCaseMemberNames
  given Decoder[Guild] = ConfiguredDecoder.derived
  given Encoder[Guild] = ConfiguredEncoder.derived
