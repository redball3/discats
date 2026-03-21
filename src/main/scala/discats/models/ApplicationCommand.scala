package discats.models

import io.circe.*
import io.circe.generic.semiauto.*
import io.circe.syntax.*
import io.circe.derivation.{Configuration, ConfiguredDecoder, ConfiguredEncoder}

/** Discord command option types. */
enum CommandOptionType(val value: Int):
  case SubCommand      extends CommandOptionType(1)
  case SubCommandGroup extends CommandOptionType(2)
  case String          extends CommandOptionType(3)
  case Integer         extends CommandOptionType(4)
  case Boolean         extends CommandOptionType(5)
  case User            extends CommandOptionType(6)
  case Channel         extends CommandOptionType(7)
  case Role            extends CommandOptionType(8)
  case Mentionable     extends CommandOptionType(9)
  case Number          extends CommandOptionType(10)
  case Attachment      extends CommandOptionType(11)
  case Unknown         extends CommandOptionType(-1)

object CommandOptionType:
  def fromInt(i: Int): CommandOptionType = values.find(_.value == i).getOrElse(Unknown)
  given Encoder[CommandOptionType] = Encoder.encodeInt.contramap(_.value)
  given Decoder[CommandOptionType] = Decoder.decodeInt.map(fromInt)

/** A choice within a command option. */
final case class CommandChoice(name: String, value: Json)

object CommandChoice:
  given Encoder[CommandChoice] = deriveEncoder
  given Decoder[CommandChoice] = deriveDecoder

/** An option (parameter) for a slash command. */
final case class CommandOption(
    `type`: CommandOptionType,
    name: String,
    description: String,
    required: Option[Boolean] = None,
    choices: Option[List[CommandChoice]] = None,
    options: Option[List[CommandOption]] = None,
    channelTypes: Option[List[Int]] = None,
    minValue: Option[Double] = None,
    maxValue: Option[Double] = None,
    autocomplete: Option[Boolean] = None,
)

object CommandOption:
  private given Configuration = Configuration.default.withSnakeCaseMemberNames
  given Encoder[CommandOption] = ConfiguredEncoder.derived
  given Decoder[CommandOption] = ConfiguredDecoder.derived

/** A command definition to register with Discord. */
final case class ApplicationCommand(
    name: String,
    description: String,
    options: Option[List[CommandOption]] = None,
    defaultMemberPermissions: Option[String] = None,
    dmPermission: Option[Boolean] = None,
) {
  def withOption(opt: CommandOption): ApplicationCommand =
    copy(options = Some(options.getOrElse(Nil) :+ opt))

  def string(name: String, description: String, required: Boolean = false): ApplicationCommand =
    withOption(CommandOption(CommandOptionType.String, name, description, required = Some(required)))

  def integer(name: String, description: String, required: Boolean = false): ApplicationCommand =
    withOption(CommandOption(CommandOptionType.Integer, name, description, required = Some(required)))

  def bool(name: String, description: String, required: Boolean = false): ApplicationCommand =
    withOption(CommandOption(CommandOptionType.Boolean, name, description, required = Some(required)))

  def user(name: String, description: String, required: Boolean = false): ApplicationCommand =
    withOption(CommandOption(CommandOptionType.User, name, description, required = Some(required)))
}

object ApplicationCommand:
  private given Configuration = Configuration.default.withSnakeCaseMemberNames
  given Encoder[ApplicationCommand] = ConfiguredEncoder.derived
  given Decoder[ApplicationCommand] = ConfiguredDecoder.derived

/** A registered application command returned from Discord. */
final case class RegisteredCommand(
    id: Snowflake,
    applicationId: Snowflake,
    name: String,
    description: String,
    options: Option[List[CommandOption]],
)

object RegisteredCommand:
  private given Configuration = Configuration.default.withSnakeCaseMemberNames
  given Decoder[RegisteredCommand] = ConfiguredDecoder.derived
  given Encoder[RegisteredCommand] = ConfiguredEncoder.derived
