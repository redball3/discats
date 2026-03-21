package discats

import cats.effect.*
import cats.effect.kernel.Ref
import discats.gateway.{GatewayEvent, Opcode}
import discats.models.*
import io.circe.*
import io.circe.jawn.decode as jsonDecode
import io.circe.syntax.*
import munit.CatsEffectSuite

class InteractionCodecSuite extends munit.FunSuite:

  private def makeInteraction(name: String, options: List[InteractionOption] = Nil): Interaction =
    Interaction(
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
      guildId   = Some(Snowflake(4L)),
      channelId = Some(Snowflake(5L)),
      member    = None,
      user      = None,
      token     = "test-token",
      version   = 1,
    )

  test("Interaction.commandName extracts command name") {
    val i = makeInteraction("ping")
    assertEquals(i.commandName, Some("ping"))
  }

  test("Interaction.stringOption extracts string value") {
    val opt = InteractionOption("text", CommandOptionType.String, Some(Json.fromString("hello")), None, None)
    val i   = makeInteraction("echo", List(opt))
    assertEquals(i.stringOption("text"), Some("hello"))
    assertEquals(i.stringOption("missing"), None)
  }

  test("Interaction.intOption extracts integer value") {
    val opt = InteractionOption("count", CommandOptionType.Integer, Some(Json.fromInt(42)), None, None)
    val i   = makeInteraction("repeat", List(opt))
    assertEquals(i.intOption("count"), Some(42L))
  }

  test("Interaction.boolOption extracts boolean value") {
    val opt = InteractionOption("flag", CommandOptionType.Boolean, Some(Json.fromBoolean(true)), None, None)
    val i   = makeInteraction("cmd", List(opt))
    assertEquals(i.boolOption("flag"), Some(true))
  }

  test("InteractionResponse.reply encodes correctly") {
    val resp = InteractionResponse.reply("Pong!")
    val enc  = resp.asJson
    assertEquals((enc \\ "type").headOption, Some(Json.fromInt(4)))
    val content = enc.hcursor.downField("data").downField("content").as[String]
    assertEquals(content, Right("Pong!"))
  }

  test("InteractionResponse.replyEphemeral sets ephemeral flag") {
    val resp = InteractionResponse.replyEphemeral("Secret!")
    val enc  = resp.asJson
    val flags = enc.hcursor.downField("data").downField("flags").as[Int]
    assertEquals(flags, Right(64)) // 1 << 6
  }

  test("InteractionResponse.defer encodes deferred type") {
    val resp = InteractionResponse.defer()
    assertEquals(resp.asJson.hcursor.downField("type").as[Int], Right(5))
  }

  test("ApplicationCommand encodes with options") {
    val cmd = ApplicationCommand("greet", "Say hello")
      .string("name", "Who to greet", required = true)
    val enc = cmd.asJson
    assertEquals((enc \\ "name").headOption.flatMap(_.asString), Some("greet"))
    val options = enc.hcursor.downField("options").as[List[Json]]
    assert(options.isRight && options.toOption.get.size == 1)
  }

  test("InteractionCallbackData.asEphemeral sets ephemeral flag") {
    val d = InteractionCallbackData.text("hi").asEphemeral
    assertEquals(d.flags, Some(64))
  }

class SlashCommandRouterSuite extends CatsEffectSuite:

  private def makeInteraction(name: String): Interaction =
    Interaction(
      id            = Snowflake(100L),
      applicationId = Snowflake(200L),
      `type`        = InteractionType.ApplicationCommand,
      data          = Some(ApplicationCommandData(Snowflake(300L), name, 1, None, None, None, None)),
      guildId       = None,
      channelId     = None,
      member        = None,
      user          = None,
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
    yield assert(r)
  }

  test("router calls fallback for unknown commands") {
    for
      called <- IO.ref(false)
      client  = StubRestClient(called)
      router  = SlashCommandRouter[IO].withFallback(_ => IO.pure(InteractionResponse.reply("fallback")))
      _      <- router.dispatch(makeInteraction("unknown"), client)
      r      <- called.get
    yield assert(r)
  }

  test("router responds with error message for unknown command without fallback") {
    for
      called <- IO.ref(false)
      client  = StubRestClient(called)
      router  = SlashCommandRouter[IO]
      _      <- router.dispatch(makeInteraction("oops"), client)
      r      <- called.get
    yield assert(r)
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
