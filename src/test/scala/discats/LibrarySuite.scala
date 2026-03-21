package discats

import cats.effect.*
import discats.gateway.*
import discats.models.*
import io.circe.*
import io.circe.syntax.*
import io.circe.jawn.decode as jsonDecode
import munit.CatsEffectSuite

class SnowflakeSuite extends munit.FunSuite:

  test("Snowflake encodes to number") {
    val sf = Snowflake(1234567890123456789L)
    assertEquals(sf.asJson, Json.fromLong(1234567890123456789L))
  }

  test("Snowflake decodes from string") {
    val json = Json.fromString("1234567890123456789")
    assertEquals(json.as[Snowflake], Right(Snowflake(1234567890123456789L)))
  }

  test("Snowflake decodes from number") {
    val json = Json.fromLong(42L)
    assertEquals(json.as[Snowflake], Right(Snowflake(42L)))
  }

  test("Snowflake.asString") {
    val sf = Snowflake(999L)
    assertEquals(sf.asString, "999")
  }

  test("Snowflake codec round-trip") {
    val sf  = Snowflake(123L)
    val enc = sf.asJson
    assertEquals(enc.as[Snowflake], Right(sf))
  }

class GatewayPayloadSuite extends munit.FunSuite:

  test("decode Hello payload") {
    val raw    = """{"op":10,"d":{"heartbeat_interval":41250},"s":null,"t":null}"""
    val result = jsonDecode[GatewayPayload](raw)
    assert(result.isRight, s"expected Right but got $result")
    val p = result.toOption.get
    assertEquals(p.op, Opcode.Hello)
    assertEquals(
      p.d.flatMap(_.hcursor.downField("heartbeat_interval").as[Long].toOption),
      Some(41250L),
    )
  }

  test("decode Dispatch payload") {
    val raw    = """{"op":0,"d":{"content":"hello"},"s":1,"t":"MESSAGE_CREATE"}"""
    val result = jsonDecode[GatewayPayload](raw)
    assert(result.isRight, s"expected Right but got $result")
    val p = result.toOption.get
    assertEquals(p.op, Opcode.Dispatch)
    assertEquals(p.s, Some(1))
    assertEquals(p.t, Some("MESSAGE_CREATE"))
  }

  test("GatewayPayload heartbeat encodes correctly") {
    val p    = GatewayPayload.heartbeat(Some(42))
    val json = p.asJson
    assertEquals((json \\ "op").headOption, Some(Json.fromInt(1)))
    assertEquals((json \\ "d").headOption, Some(Json.fromInt(42)))
  }

  test("GatewayPayload codec round-trip") {
    val p   = GatewayPayload(Opcode.HeartbeatAck, None, None, None)
    val enc = p.asJson
    val dec = enc.as[GatewayPayload]
    assertEquals(dec.map(_.op), Right(Opcode.HeartbeatAck))
  }

class GatewayEventSuite extends munit.FunSuite:

  test("fromPayload returns None for non-Dispatch") {
    val p = GatewayPayload(Opcode.Hello, None, None, None)
    assertEquals(GatewayEvent.fromPayload(p), None)
  }

  test("fromPayload returns Unknown for unhandled event") {
    val p = GatewayPayload(Opcode.Dispatch, Some(Json.obj()), None, Some("SOME_UNKNOWN_EVENT"))
    assertEquals(
      GatewayEvent.fromPayload(p),
      Some(GatewayEvent.Unknown("SOME_UNKNOWN_EVENT", Some(Json.obj()))),
    )
  }

  test("fromPayload handles RESUMED") {
    val p = GatewayPayload(Opcode.Dispatch, None, Some(1), Some("RESUMED"))
    assertEquals(GatewayEvent.fromPayload(p), Some(GatewayEvent.Resumed))
  }

class ModelCodecSuite extends munit.FunSuite:

  test("Channel round-trips JSON") {
    val channel = Channel(
      id            = Snowflake(1L),
      `type`        = ChannelType.GuildText,
      guildId       = Some(Snowflake(2L)),
      name          = Some("general"),
      topic         = None,
      nsfw          = Some(false),
      lastMessageId = None,
      position      = Some(0),
      parentId      = None,
    )
    val encoded = channel.asJson
    val decoded = encoded.as[Channel]
    assertEquals(decoded, Right(channel))
  }

  test("MessageCreate encodes content") {
    val mc  = MessageCreate(content = "hello!")
    val enc = mc.asJson
    assertEquals((enc \\ "content").headOption, Some(Json.fromString("hello!")))
  }
