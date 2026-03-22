package discats

import cats.effect.*
import cats.effect.kernel.Ref
import discats.gateway.{GatewayEvent, Opcode}
import discats.models.*
import io.circe.*
import io.circe.jawn.decode as jsonDecode
import io.circe.syntax.*
import weaver.SimpleIOSuite

object InteractionCodecSuite extends SimpleIOSuite:

  private val fakeMember = GuildMember(None, None, Nil, "2021-01-01T00:00:00Z", deaf = false, mute = false)

  private def makeInteraction(name: String, options: List[InteractionOption] = Nil): GuildInteraction =
    GuildInteraction(
      id            = Snowflake(1L),
      applicationId = Snowflake(2L),
      `type`        = InteractionType.ApplicationCommand,
      data          = Some(ApplicationCommandData(
        id       = Snowflake(3L),
        name     = name,
        `type`   = 1,
        resolved = None,
        options  = if options.isEmpty then None else Some(options),
        guildId  = None,
        targetId = None,
      )),
      guildId   = Snowflake(4L),
      channelId = Some(Snowflake(5L)),
      member    = fakeMember,
      token     = "test-token",
      version   = 1,
    )

  pureTest("Interaction.commandName extracts command name") {
    val i = makeInteraction("ping")
    expect(i.commandName == Some("ping"))
  }

  pureTest("Interaction.stringOption extracts string value") {
    val opt = InteractionOption("text", CommandOptionType.String, Some(Json.fromString("hello")), None, None)
    val i   = makeInteraction("echo", List(opt))
    expect(i.stringOption("text") == Some("hello")) and
      expect(i.stringOption("missing") == None)
  }

  pureTest("Interaction.intOption extracts integer value") {
    val opt = InteractionOption("count", CommandOptionType.Integer, Some(Json.fromInt(42)), None, None)
    val i   = makeInteraction("repeat", List(opt))
    expect(i.intOption("count") == Some(42L))
  }

  pureTest("Interaction.boolOption extracts boolean value") {
    val opt = InteractionOption("flag", CommandOptionType.Boolean, Some(Json.fromBoolean(true)), None, None)
    val i   = makeInteraction("cmd", List(opt))
    expect(i.boolOption("flag") == Some(true))
  }

  pureTest("InteractionResponse.reply encodes correctly") {
    val resp = InteractionResponse.reply("Pong!")
    val enc  = resp.asJson
    val content = enc.hcursor.downField("data").downField("content").as[String]
    expect((enc \\ "type").headOption == Some(Json.fromInt(4))) and
      expect(content == Right("Pong!"))
  }

  pureTest("InteractionResponse.replyEphemeral sets ephemeral flag") {
    val resp  = InteractionResponse.replyEphemeral("Secret!")
    val flags = resp.asJson.hcursor.downField("data").downField("flags").as[Int]
    expect(flags == Right(64)) // 1 << 6
  }

  pureTest("InteractionResponse.defer encodes deferred type") {
    val resp = InteractionResponse.defer()
    expect(resp.asJson.hcursor.downField("type").as[Int] == Right(5))
  }

  pureTest("ApplicationCommand encodes with options") {
    val cmd = ApplicationCommand("greet", "Say hello")
      .string("name", "Who to greet", required = true)
    val enc     = cmd.asJson
    val options = enc.hcursor.downField("options").as[List[Json]]
    expect((enc \\ "name").headOption.flatMap(_.asString) == Some("greet")) and
      expect(options.isRight && options.toOption.get.size == 1)
  }

  pureTest("InteractionCallbackData.asEphemeral sets ephemeral flag") {
    val d = InteractionCallbackData.text("hi").asEphemeral
    expect(d.flags == Some(64))
  }

object SlashCommandRouterSuite extends SimpleIOSuite:

  private val fakeUser = User(Snowflake(0L), "testuser", "0000", None, None, None, None)

  private def makeInteraction(name: String): ChannelInteraction =
    ChannelInteraction(
      id            = Snowflake(100L),
      applicationId = Snowflake(200L),
      `type`        = InteractionType.ApplicationCommand,
      data          = Some(ApplicationCommandData(Snowflake(300L), name, 1, None, None, None, None)),
      channelId     = None,
      user          = fakeUser,
      token         = "tok",
      version       = 1,
    )

  test("router routes to correct handler") {
    for
      called <- IO.ref(false)
      client  = StubRestClient(called)
      router  = SlashCommandRouter[IO].on("ping")(_ => IO.pure(InteractionResponse.reply("Pong!")))
      _      <- router.dispatch(makeInteraction("ping"), client)
      r      <- called.get
    yield expect(r)
  }

  test("router calls fallback for unknown commands") {
    for
      called <- IO.ref(false)
      client  = StubRestClient(called)
      router  = SlashCommandRouter[IO].withFallback(_ => IO.pure(InteractionResponse.reply("fallback")))
      _      <- router.dispatch(makeInteraction("unknown"), client)
      r      <- called.get
    yield expect(r)
  }

  test("router responds with error message for unknown command without fallback") {
    for
      called <- IO.ref(false)
      client  = StubRestClient(called)
      router  = SlashCommandRouter[IO]
      _      <- router.dispatch(makeInteraction("oops"), client)
      r      <- called.get
    yield expect(r)
  }

/** Minimal stub RestClient that records whether respondToInteraction was called. */
class StubRestClient(called: Ref[IO, Boolean]) extends discats.rest.RestClient[IO]:
  def respondToInteraction(id: Snowflake, token: String, r: InteractionResponse): IO[Unit] =
    called.set(true)
  // ── stubs for the rest ───────────────────────────────────────────────
  def getChannel(channelId: Snowflake): IO[Channel]                                              = IO.raiseError(notImpl)
  def getMessage(channelId: Snowflake, messageId: Snowflake): IO[discats.models.Message]         = IO.raiseError(notImpl)
  def getMessages(channelId: Snowflake, limit: Int): IO[List[discats.models.Message]]            = IO.raiseError(notImpl)
  def sendMessage(channelId: Snowflake, body: MessageCreate): IO[discats.models.Message]         = IO.raiseError(notImpl)
  def deleteMessage(channelId: Snowflake, messageId: Snowflake): IO[Unit]                        = IO.raiseError(notImpl)
  def editMessage(c: Snowflake, m: Snowflake, b: MessageCreate): IO[discats.models.Message]      = IO.raiseError(notImpl)
  def getGuild(guildId: Snowflake): IO[Guild]                                                    = IO.raiseError(notImpl)
  def getGuildChannels(guildId: Snowflake): IO[List[Channel]]                                    = IO.raiseError(notImpl)
  def registerGlobalCommand(appId: Snowflake, cmd: ApplicationCommand): IO[RegisteredCommand]   = IO.raiseError(notImpl)
  def registerGuildCommand(a: Snowflake, g: Snowflake, c: ApplicationCommand): IO[RegisteredCommand] = IO.raiseError(notImpl)
  def listGlobalCommands(appId: Snowflake): IO[List[RegisteredCommand]]                         = IO.raiseError(notImpl)
  def deleteGlobalCommand(a: Snowflake, c: Snowflake): IO[Unit]                                 = IO.raiseError(notImpl)
  def deleteGuildCommand(a: Snowflake, g: Snowflake, c: Snowflake): IO[Unit]                    = IO.raiseError(notImpl)
  def editInteractionResponse(a: Snowflake, t: String, b: MessageCreate): IO[discats.models.Message] = IO.raiseError(notImpl)
  def deleteInteractionResponse(a: Snowflake, t: String): IO[Unit]                              = IO.raiseError(notImpl)
  private val notImpl = new NotImplementedError("stub")
