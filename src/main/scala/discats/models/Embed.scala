package discats.models

import io.circe.generic.semiauto.*
import io.circe.{Decoder, Encoder}

final case class EmbedFooter(text: String, iconUrl: Option[String])
final case class EmbedImage(url: String, height: Option[Int], width: Option[Int])
final case class EmbedThumbnail(url: String, height: Option[Int], width: Option[Int])
final case class EmbedAuthor(name: String, url: Option[String], iconUrl: Option[String])
final case class EmbedField(name: String, value: String, inline: Option[Boolean])

final case class Embed(
    title: Option[String],
    description: Option[String],
    url: Option[String],
    color: Option[Int],
    footer: Option[EmbedFooter],
    image: Option[EmbedImage],
    thumbnail: Option[EmbedThumbnail],
    author: Option[EmbedAuthor],
    fields: Option[List[EmbedField]],
)

object Embed:
  given Decoder[EmbedFooter]    = deriveDecoder
  given Encoder[EmbedFooter]    = deriveEncoder
  given Decoder[EmbedImage]     = deriveDecoder
  given Encoder[EmbedImage]     = deriveEncoder
  given Decoder[EmbedThumbnail] = deriveDecoder
  given Encoder[EmbedThumbnail] = deriveEncoder
  given Decoder[EmbedAuthor]    = deriveDecoder
  given Encoder[EmbedAuthor]    = deriveEncoder
  given Decoder[EmbedField]     = deriveDecoder
  given Encoder[EmbedField]     = deriveEncoder
  given Decoder[Embed]          = deriveDecoder
  given Encoder[Embed]          = deriveEncoder
