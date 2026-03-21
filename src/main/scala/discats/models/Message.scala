package discats.models

import io.circe.generic.semiauto.*
import io.circe.{Decoder, Encoder}
import io.circe.derivation.{Configuration, ConfiguredDecoder, ConfiguredEncoder}

final case class Message(
    id: Snowflake,
    channelId: Snowflake,
    guildId: Option[Snowflake],
    author: User,
    content: String,
    timestamp: String,
    editedTimestamp: Option[String],
    tts: Boolean,
    mentionEveryone: Boolean,
    pinned: Boolean,
    embeds: List[Embed],
    `type`: Int,
)

object Message:
  private given Configuration = Configuration.default.withSnakeCaseMemberNames
  given Decoder[Message] = ConfiguredDecoder.derived
  given Encoder[Message] = ConfiguredEncoder.derived

final case class MessageCreate(
    content: String,
    tts: Option[Boolean] = None,
    embeds: Option[List[Embed]] = None,
    replyToMessageId: Option[Snowflake] = None,
)

object MessageCreate:
  private given Configuration = Configuration.default.withSnakeCaseMemberNames
  given Encoder[MessageCreate] = ConfiguredEncoder.derived
