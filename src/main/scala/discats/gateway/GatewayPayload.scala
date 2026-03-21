package discats.gateway

import io.circe.*
import io.circe.syntax.*

/** Raw Gateway payload as received over WebSocket.
  * @param op  opcode
  * @param d   event data (varies by opcode/event)
  * @param s   sequence number (only for Dispatch)
  * @param t   event name (only for Dispatch)
  */
final case class GatewayPayload(
    op: Opcode,
    d: Option[Json],
    s: Option[Int],
    t: Option[String],
)

object GatewayPayload:
  given Decoder[GatewayPayload] = (c: HCursor) =>
    for
      op <- c.downField("op").as[Opcode]
      d  <- c.downField("d").as[Option[Json]]
      s  <- c.downField("s").as[Option[Int]]
      t  <- c.downField("t").as[Option[String]]
    yield GatewayPayload(op, d, s, t)

  given Encoder[GatewayPayload] = (p: GatewayPayload) =>
    Json.obj(
      "op" -> p.op.asJson,
      "d"  -> p.d.asJson,
      "s"  -> p.s.asJson,
      "t"  -> p.t.asJson,
    )

  def heartbeat(seq: Option[Int]): GatewayPayload =
    GatewayPayload(Opcode.Heartbeat, seq.map(Json.fromInt), None, None)

  def identify(token: String, intents: Int, properties: IdentifyProperties): GatewayPayload =
    GatewayPayload(
      op = Opcode.Identify,
      d  = Some(Json.obj(
        "token"      -> Json.fromString(token),
        "intents"    -> Json.fromInt(intents),
        "properties" -> Json.obj(
          "os"      -> Json.fromString(properties.os),
          "browser" -> Json.fromString(properties.browser),
          "device"  -> Json.fromString(properties.device),
        ),
      )),
      s = None,
      t = None,
    )

  def resume(token: String, sessionId: String, seq: Int): GatewayPayload =
    GatewayPayload(
      op = Opcode.Resume,
      d  = Some(Json.obj(
        "token"      -> Json.fromString(token),
        "session_id" -> Json.fromString(sessionId),
        "seq"        -> Json.fromInt(seq),
      )),
      s = None,
      t = None,
    )

final case class IdentifyProperties(
    os: String      = "linux",
    browser: String = "discats",
    device: String  = "discats",
)
