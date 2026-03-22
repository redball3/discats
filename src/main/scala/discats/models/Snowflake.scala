package discats.models

import io.circe.{Decoder, Encoder}

/** A Discord Snowflake ID — an opaque wrapper around a [[Long]].
  *
  * Snowflakes are Discord's unique identifiers for entities such as users,
  * messages, channels, and guilds. They encode a timestamp, worker ID, and
  * sequence number in a 64-bit integer.
  *
  * Use [[Snowflake.apply]] to construct from a `Long`, or
  * [[Snowflake.fromString]] to parse from a string (Discord often serialises
  * IDs as strings in JSON).
  */
opaque type Snowflake = Long

object Snowflake:
  /** Wrap a raw [[Long]] as a [[Snowflake]]. */
  def apply(value: Long): Snowflake = value

  /** Parse a [[Snowflake]] from a decimal string.
    * Returns `Left` with an error message if the string is not a valid long.
    */
  def fromString(s: String): Either[String, Snowflake] =
    s.toLongOption.toRight(s"Invalid Snowflake: $s")

  extension (s: Snowflake)
    /** The underlying [[Long]] value. */
    def value: Long    = s
    /** The [[Snowflake]] rendered as a decimal string, as Discord returns in JSON. */
    def asString: String = s.toString

  given Encoder[Snowflake] = Encoder.encodeLong.contramap(_.value)
  given Decoder[Snowflake] =
    Decoder.decodeString.emap(fromString)
      .or(Decoder.decodeLong.map(apply))
