package discats.models

import io.circe.*
import io.circe.syntax.*

/** An option within a string select menu. */
final case class SelectOption(
    label: String,
    value: String,
    description: Option[String] = None,
    default: Option[Boolean] = None,
)

object SelectOption:
  given Encoder[SelectOption] = (s: SelectOption) =>
    Json
      .obj(
        "label"       -> Json.fromString(s.label),
        "value"       -> Json.fromString(s.value),
        "description" -> s.description.fold(Json.Null)(Json.fromString),
        "default"     -> s.default.fold(Json.Null)(Json.fromBoolean),
      )
      .dropNullValues

  given Decoder[SelectOption] = (c: HCursor) =>
    for
      label       <- c.downField("label").as[String]
      value       <- c.downField("value").as[String]
      description <- c.downField("description").as[Option[String]]
      default     <- c.downField("default").as[Option[Boolean]]
    yield SelectOption(label, value, description, default)

/** A string select menu component (Discord component type 3). */
final case class StringSelectMenu(
    customId: String,
    options: List[SelectOption],
    placeholder: Option[String] = None,
    minValues: Option[Int] = None,
    maxValues: Option[Int] = None,
    disabled: Option[Boolean] = None,
)

object StringSelectMenu:
  given Encoder[StringSelectMenu] = (m: StringSelectMenu) =>
    Json
      .obj(
        "type"        -> Json.fromInt(3),
        "custom_id"   -> Json.fromString(m.customId),
        "options"     -> m.options.asJson,
        "placeholder" -> m.placeholder.fold(Json.Null)(Json.fromString),
        "min_values"  -> m.minValues.fold(Json.Null)(Json.fromInt),
        "max_values"  -> m.maxValues.fold(Json.Null)(Json.fromInt),
        "disabled"    -> m.disabled.fold(Json.Null)(Json.fromBoolean),
      )
      .dropNullValues

  given Decoder[StringSelectMenu] = (c: HCursor) =>
    for
      customId    <- c.downField("custom_id").as[String]
      options     <- c.downField("options").as[List[SelectOption]]
      placeholder <- c.downField("placeholder").as[Option[String]]
      minValues   <- c.downField("min_values").as[Option[Int]]
      maxValues   <- c.downField("max_values").as[Option[Int]]
      disabled    <- c.downField("disabled").as[Option[Boolean]]
    yield StringSelectMenu(customId, options, placeholder, minValues, maxValues, disabled)

/** An action row (Discord component type 1) containing a single select menu.
  *
  * Discord requires select menus to be wrapped in an action row.
  */
final case class ActionRow(menu: StringSelectMenu)

object ActionRow:
  given Encoder[ActionRow] = (a: ActionRow) =>
    Json.obj(
      "type"       -> Json.fromInt(1),
      "components" -> Json.arr(a.menu.asJson),
    )

  given Decoder[ActionRow] = (c: HCursor) =>
    c.downField("components").downArray.as[StringSelectMenu].map(ActionRow.apply)
