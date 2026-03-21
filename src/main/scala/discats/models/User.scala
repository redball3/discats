package discats.models

import io.circe.generic.semiauto.*
import io.circe.{Decoder, Encoder}
import io.circe.derivation.{Configuration, ConfiguredDecoder, ConfiguredEncoder}

final case class User(
    id: Snowflake,
    username: String,
    discriminator: String,
    globalName: Option[String],
    avatar: Option[String],
    bot: Option[Boolean],
    system: Option[Boolean],
)

object User:
  private given Configuration = Configuration.default.withSnakeCaseMemberNames
  given Decoder[User] = ConfiguredDecoder.derived
  given Encoder[User] = ConfiguredEncoder.derived
