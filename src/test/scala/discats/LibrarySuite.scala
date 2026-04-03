package discats

import cats.effect.*
import discats.gateway.*
import discats.models.*
import io.circe.*
import io.circe.syntax.*
import io.circe.jawn.decode as jsonDecode
import weaver.SimpleIOSuite

object SnowflakeSuite extends SimpleIOSuite:

  pureTest("Snowflake encodes to number") {
    val sf = Snowflake(1234567890123456789L)
    expect(sf.asJson == Json.fromLong(1234567890123456789L))
  }

  pureTest("Snowflake decodes from string") {
    val json = Json.fromString("1234567890123456789")
    expect(json.as[Snowflake] == Right(Snowflake(1234567890123456789L)))
  }

  pureTest("Snowflake decodes from number") {
    val json = Json.fromLong(42L)
    expect(json.as[Snowflake] == Right(Snowflake(42L)))
  }

  pureTest("Snowflake.asString") {
    val sf = Snowflake(999L)
    expect(sf.asString == "999")
  }

  pureTest("Snowflake codec round-trip") {
    val sf  = Snowflake(123L)
    val enc = sf.asJson
    expect(enc.as[Snowflake] == Right(sf))
  }

object GatewayPayloadSuite extends SimpleIOSuite:

  pureTest("decode Hello payload") {
    val raw = """{"op":10,"d":{"heartbeat_interval":41250},"s":null,"t":null}"""
    jsonDecode[GatewayPayload](raw) match
      case Left(err) => failure(s"expected Right but got Left($err)")
      case Right(p) =>
        expect(p.op == Opcode.Hello) and
          expect(p.d.flatMap(_.hcursor.downField("heartbeat_interval").as[Long].toOption) == Some(41250L))
  }

  pureTest("decode Dispatch payload") {
    val raw = """{"op":0,"d":{"content":"hello"},"s":1,"t":"MESSAGE_CREATE"}"""
    jsonDecode[GatewayPayload](raw) match
      case Left(err) => failure(s"expected Right but got Left($err)")
      case Right(p) =>
        expect(p.op == Opcode.Dispatch) and
          expect(p.s == Some(1)) and
          expect(p.t == Some("MESSAGE_CREATE"))
  }

  pureTest("GatewayPayload heartbeat encodes correctly") {
    val p    = GatewayPayload.heartbeat(Some(42))
    val json = p.asJson
    expect((json \\ "op").headOption == Some(Json.fromInt(1))) and
      expect((json \\ "d").headOption == Some(Json.fromInt(42)))
  }

  pureTest("GatewayPayload codec round-trip") {
    val p   = GatewayPayload(Opcode.HeartbeatAck, None, None, None)
    val enc = p.asJson
    val dec = enc.as[GatewayPayload]
    expect(dec.map(_.op) == Right(Opcode.HeartbeatAck))
  }

object GatewayEventSuite extends SimpleIOSuite:

  pureTest("fromPayload returns None for non-Dispatch") {
    val p = GatewayPayload(Opcode.Hello, None, None, None)
    expect(GatewayEvent.fromPayload(p) == None)
  }

  pureTest("fromPayload returns Unknown for unhandled event") {
    val p = GatewayPayload(Opcode.Dispatch, Some(Json.obj()), None, Some("SOME_UNKNOWN_EVENT"))
    expect(GatewayEvent.fromPayload(p) == Some(GatewayEvent.Unknown("SOME_UNKNOWN_EVENT", Some(Json.obj()))))
  }

  pureTest("fromPayload handles RESUMED") {
    val p = GatewayPayload(Opcode.Dispatch, None, Some(1), Some("RESUMED"))
    expect(GatewayEvent.fromPayload(p) == Some(GatewayEvent.Resumed))
  }

object ModelCodecSuite extends SimpleIOSuite:

  pureTest("Channel round-trips JSON") {
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
    expect(encoded.as[Channel] == Right(channel))
  }

  pureTest("MessageCreate encodes content") {
    val mc  = MessageCreate(content = "hello!")
    val enc = mc.asJson
    expect((enc \\ "content").headOption == Some(Json.fromString("hello!")))
  }

  pureTest("PermissionOverwrite encodes with correct field names") {
    val ow  = PermissionOverwrite(id = "123", `type` = 0, allow = "8", deny = "0")
    val enc = ow.asJson
    expect((enc \\ "id").headOption == Some(Json.fromString("123"))) and
      expect((enc \\ "type").headOption == Some(Json.fromInt(0))) and
      expect((enc \\ "allow").headOption == Some(Json.fromString("8"))) and
      expect((enc \\ "deny").headOption == Some(Json.fromString("0")))
  }

  pureTest("PermissionOverwrite member type encodes as 1") {
    val ow  = PermissionOverwrite(id = "456", `type` = 1, allow = "0", deny = "4")
    val enc = ow.asJson
    expect((enc \\ "type").headOption == Some(Json.fromInt(1)))
  }
