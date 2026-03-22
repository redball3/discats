package discats

import discats.models.*
import io.circe.jawn.decode
import io.circe.syntax.*
import weaver.SimpleIOSuite

object InteractionSubtypeSuite extends SimpleIOSuite:

  // ── Fixtures ──────────────────────────────────────────────────────────────

  private val fakeUser   = User(Snowflake(10L), "testuser", "0000", None, None, None, None)
  private val fakeMember = GuildMember(Some(fakeUser), Some("tester"), Nil, "2021-01-01T00:00:00Z", deaf = false, mute = false)

  private val guildJson =
    """|{
       |  "id": "1", "application_id": "2", "type": 2,
       |  "guild_id": "99", "channel_id": "5",
       |  "member": {"roles":[],"joined_at":"2021-01-01T00:00:00Z","deaf":false,"mute":false},
       |  "token": "tok", "version": 1
       |}""".stripMargin

  private val channelJson =
    """|{
       |  "id": "1", "application_id": "2", "type": 2,
       |  "channel_id": "5",
       |  "user": {"id":"10","username":"testuser","discriminator":"0000"},
       |  "token": "tok", "version": 1
       |}""".stripMargin

  private val guildInteraction = GuildInteraction(
    id            = Snowflake(1L),
    applicationId = Snowflake(2L),
    `type`        = InteractionType.ApplicationCommand,
    data          = None,
    guildId       = Snowflake(99L),
    channelId     = Some(Snowflake(5L)),
    member        = fakeMember,
    token         = "tok",
    version       = 1,
  )

  private val channelInteraction = ChannelInteraction(
    id            = Snowflake(1L),
    applicationId = Snowflake(2L),
    `type`        = InteractionType.ApplicationCommand,
    data          = None,
    channelId     = Some(Snowflake(5L)),
    user          = fakeUser,
    token         = "tok",
    version       = 1,
  )

  // ── Decoder dispatch ──────────────────────────────────────────────────────

  pureTest("Interaction decoder produces GuildInteraction when guild_id is present") {
    val result = decode[Interaction](guildJson)
    expect(result.isRight) and expect(result.toOption.get.isInstanceOf[GuildInteraction])
  }

  pureTest("Interaction decoder produces ChannelInteraction when guild_id is absent") {
    val result = decode[Interaction](channelJson)
    expect(result.isRight) and expect(result.toOption.get.isInstanceOf[ChannelInteraction])
  }

  pureTest("GuildInteraction decoded fields are correct") {
    decode[Interaction](guildJson) match
      case Right(g: GuildInteraction) =>
        expect(g.guildId == Snowflake(99L)) and
          expect(g.channelId == Some(Snowflake(5L))) and
          expect(g.token == "tok")
      case other => failure(s"Expected GuildInteraction, got $other")
  }

  pureTest("ChannelInteraction decoded fields are correct") {
    decode[Interaction](channelJson) match
      case Right(c: ChannelInteraction) =>
        expect(c.user.username == "testuser") and
          expect(c.channelId == Some(Snowflake(5L))) and
          expect(c.token == "tok")
      case other => failure(s"Expected ChannelInteraction, got $other")
  }

  // ── Codec round-trips ─────────────────────────────────────────────────────

  pureTest("GuildInteraction codec round-trips") {
    val json   = guildInteraction.asJson
    val result = json.as[GuildInteraction]
    expect(result == Right(guildInteraction))
  }

  pureTest("ChannelInteraction codec round-trips") {
    val json   = channelInteraction.asJson
    val result = json.as[ChannelInteraction]
    expect(result == Right(channelInteraction))
  }

  pureTest("Interaction encoder round-trips via the sealed trait") {
    val asSealed: Interaction = guildInteraction
    val result = asSealed.asJson.as[Interaction]
    expect(result == Right(guildInteraction))
  }

  // ── invoker ───────────────────────────────────────────────────────────────

  pureTest("GuildInteraction.invoker returns member.user") {
    expect(guildInteraction.invoker == fakeMember.user)
  }

  pureTest("GuildInteraction.invoker is None when member has no user") {
    val g = guildInteraction.copy(member = fakeMember.copy(user = None))
    expect(g.invoker == None)
  }

  pureTest("ChannelInteraction.invoker returns the user directly") {
    expect(channelInteraction.invoker == fakeUser)
  }

  // ── Shared sealed-trait methods ───────────────────────────────────────────

  pureTest("commandName works on GuildInteraction") {
    val g = guildInteraction.copy(data = Some(ApplicationCommandData(Snowflake(3L), "ping", 1, None, None, None, None)))
    expect(g.commandName == Some("ping"))
  }

  pureTest("commandName works on ChannelInteraction") {
    val c = channelInteraction.copy(data = Some(ApplicationCommandData(Snowflake(3L), "ping", 1, None, None, None, None)))
    expect(c.commandName == Some("ping"))
  }

  pureTest("commandName is None for non-ApplicationCommand interactions") {
    val g = guildInteraction.copy(`type` = InteractionType.Ping, data = Some(ApplicationCommandData(Snowflake(3L), "x", 1, None, None, None, None)))
    expect(g.commandName == None)
  }

  pureTest("stringOption extracts option from ChannelInteraction") {
    import io.circe.Json
    val opt = InteractionOption("msg", CommandOptionType.String, Some(Json.fromString("hello")), None, None)
    val c   = channelInteraction.copy(data = Some(ApplicationCommandData(Snowflake(3L), "echo", 1, None, Some(List(opt)), None, None)))
    expect(c.stringOption("msg") == Some("hello")) and expect(c.stringOption("nope") == None)
  }

  // ── Pattern matching ──────────────────────────────────────────────────────

  pureTest("sealed trait can be pattern-matched exhaustively") {
    def label(i: Interaction): String = i match
      case _: GuildInteraction   => "guild"
      case _: ChannelInteraction => "channel"

    expect(label(guildInteraction) == "guild") and
      expect(label(channelInteraction) == "channel")
  }
