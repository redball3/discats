package discats.models

import io.circe.{Decoder, Encoder}

opaque type Snowflake = Long

object Snowflake:
  def apply(value: Long): Snowflake = value
  def fromString(s: String): Either[String, Snowflake] =
    s.toLongOption.toRight(s"Invalid Snowflake: $s")

  extension (s: Snowflake)
    def value: Long    = s
    def asString: String = s.toString

  given Encoder[Snowflake] = Encoder.encodeLong.contramap(_.value)
  given Decoder[Snowflake] =
    Decoder.decodeString.emap(fromString)
      .or(Decoder.decodeLong.map(apply))
