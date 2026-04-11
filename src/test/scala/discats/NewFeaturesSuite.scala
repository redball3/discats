package discats

import cats.effect.*
import cats.effect.kernel.Ref
import cats.syntax.all.*
import discats.gateway.GatewayEvent
import discats.models.*
import io.circe.*
import io.circe.jawn.decode as jsonDecode
import io.circe.syntax.*
import weaver.SimpleIOSuite

// ── Attachment tests ─────────────────────────────────────────────────────────

object AttachmentSuite extends SimpleIOSuite:

  private val fakeAttachment = Attachment(
    id          = Snowflake(999L),
    filename    = "screenshot.png",
    url         = "https://cdn.discordapp.com/attachments/screenshot.png",
    proxyUrl    = "https://media.discordapp.net/attachments/screenshot.png",
    size        = 123456,
    contentType = Some("image/png"),
  )

  pureTest("Attachment codec round-trips") {
    val result = fakeAttachment.asJson.as[Attachment]
    expect(result == Right(fakeAttachment))
  }

  pureTest("Attachment decodes from Discord JSON (snake_case)") {
    val json =
      """|{
         |  "id": "999",
         |  "filename": "screenshot.png",
         |  "url": "https://cdn.discordapp.com/attachments/screenshot.png",
         |  "proxy_url": "https://media.discordapp.net/attachments/screenshot.png",
         |  "size": 123456,
         |  "content_type": "image/png"
         |}""".stripMargin
    val result = jsonDecode[Attachment](json)
    expect(result.isRight) and
      expect(result.toOption.get.filename == "screenshot.png") and
      expect(result.toOption.get.contentType == Some("image/png"))
  }

  pureTest("Attachment without content_type decodes with None") {
    val json =
      """|{
         |  "id": "999",
         |  "filename": "file.dat",
         |  "url": "https://cdn.discordapp.com/file.dat",
         |  "proxy_url": "https://media.discordapp.net/file.dat",
         |  "size": 42
         |}""".stripMargin
    val result = jsonDecode[Attachment](json)
    expect(result.isRight) and expect(result.toOption.get.contentType == None)
  }

  pureTest("attachmentOption resolves attachment from resolved data") {
    val attachmentId = "999"
    val resolved     = ResolvedData(None, None, None, Some(Map(attachmentId -> fakeAttachment)))
    val option       = InteractionOption("screenshot", CommandOptionType.Attachment, Some(Json.fromString(attachmentId)), None, None)
    val interaction  = ChannelInteraction(
      id            = Snowflake(1L),
      applicationId = Snowflake(2L),
      `type`        = InteractionType.ApplicationCommand,
      data          = Some(ApplicationCommandData(Snowflake(3L), "submit-task", 1, Some(resolved), Some(List(option)), None, None)),
      channelId     = None,
      user          = User(Snowflake(10L), "user", "0000", None, None, None, None),
      token         = "tok",
      version       = 1,
    )
    expect(interaction.attachmentOption("screenshot") == Some(fakeAttachment)) and
      expect(interaction.attachmentOption("missing") == None)
  }

  pureTest("ApplicationCommand.attachment() adds attachment option") {
    val cmd = ApplicationCommand("submit-task", "Submit task evidence")
      .attachment("screenshot", "Evidence screenshot", required = true)
    val enc     = cmd.asJson
    val options = enc.hcursor.downField("options").as[List[Json]]
    expect(options.isRight) and
      expect(options.toOption.get.size == 1) and
      expect(options.toOption.get.head.hcursor.downField("type").as[Int] == Right(11))
  }

// ── MessageComponentData tests ───────────────────────────────────────────────

object MessageComponentDataSuite extends SimpleIOSuite:

  pureTest("MessageComponentData codec round-trips") {
    val data = MessageComponentData("area_select", 3, Some(List("Asgarnia", "Kandarin")))
    val result = data.asJson.as[MessageComponentData]
    expect(result == Right(data))
  }

  pureTest("MessageComponentData decodes from Discord JSON (snake_case)") {
    val json =
      """|{
         |  "custom_id": "area_select",
         |  "component_type": 3,
         |  "values": ["Asgarnia"]
         |}""".stripMargin
    val result = jsonDecode[MessageComponentData](json)
    expect(result.isRight) and
      expect(result.toOption.get.customId == "area_select") and
      expect(result.toOption.get.values == Some(List("Asgarnia")))
  }

  pureTest("GuildInteraction decodes MessageComponent type and populates componentData") {
    val fakeUser   = User(Snowflake(10L), "tester", "0000", None, None, None, None)
    val fakeMember = GuildMember(Some(fakeUser), None, Nil, "2021-01-01T00:00:00Z", deaf = false, mute = false)
    val json =
      """|{
         |  "id": "1", "application_id": "2", "type": 3,
         |  "guild_id": "99", "channel_id": "5",
         |  "member": {"roles":[],"joined_at":"2021-01-01T00:00:00Z","deaf":false,"mute":false},
         |  "token": "tok", "version": 1,
         |  "data": {
         |    "custom_id": "submit_task:abc123",
         |    "component_type": 3,
         |    "values": ["Asgarnia"]
         |  }
         |}""".stripMargin
    val result = jsonDecode[Interaction](json)
    expect(result.isRight) and
      expect(result.toOption.get.`type` == InteractionType.MessageComponent) and
      expect(result.toOption.get.componentCustomId == Some("submit_task:abc123")) and
      expect(result.toOption.get.componentValues == Some(List("Asgarnia"))) and
      expect(result.toOption.get.data == None)
  }

  pureTest("ChannelInteraction decodes MessageComponent type and populates componentData") {
    val json =
      """|{
         |  "id": "1", "application_id": "2", "type": 3,
         |  "channel_id": "5",
         |  "user": {"id":"10","username":"testuser","discriminator":"0000"},
         |  "token": "tok", "version": 1,
         |  "data": {
         |    "custom_id": "confirm_action",
         |    "component_type": 2,
         |    "values": null
         |  }
         |}""".stripMargin
    val result = jsonDecode[Interaction](json)
    expect(result.isRight) and
      expect(result.toOption.get.componentCustomId == Some("confirm_action")) and
      expect(result.toOption.get.componentValues == None)
  }

  pureTest("ApplicationCommand interaction still populates data correctly") {
    val json =
      """|{
         |  "id": "1", "application_id": "2", "type": 2,
         |  "channel_id": "5",
         |  "user": {"id":"10","username":"testuser","discriminator":"0000"},
         |  "token": "tok", "version": 1,
         |  "data": {"id":"3","name":"ping","type":1}
         |}""".stripMargin
    val result = jsonDecode[Interaction](json)
    expect(result.isRight) and
      expect(result.toOption.get.commandName == Some("ping")) and
      expect(result.toOption.get.componentCustomId == None)
  }

// ── Component model tests ────────────────────────────────────────────────────

object ComponentSuite extends SimpleIOSuite:

  pureTest("SelectOption encodes correctly, dropping nulls") {
    val opt = SelectOption("Asgarnia", "asgarnia")
    val enc = opt.asJson
    expect(enc.hcursor.downField("label").as[String] == Right("Asgarnia")) and
      expect(enc.hcursor.downField("value").as[String] == Right("asgarnia")) and
      expect(!enc.hcursor.keys.exists(_.toList.contains("description")))
  }

  pureTest("SelectOption with description encodes description") {
    val opt = SelectOption("Asgarnia", "asgarnia", description = Some("Desert area"))
    val enc = opt.asJson
    expect(enc.hcursor.downField("description").as[String] == Right("Desert area"))
  }

  pureTest("SelectOption codec round-trips") {
    val opt    = SelectOption("Kandarin", "kandarin", Some("Fishing area"), None)
    val result = opt.asJson.as[SelectOption]
    expect(result == Right(opt))
  }

  pureTest("StringSelectMenu encodes with correct type=3 and custom_id") {
    val menu = StringSelectMenu(
      customId    = "area_select",
      options     = List(SelectOption("Asgarnia", "asgarnia"), SelectOption("Kandarin", "kandarin")),
      placeholder = Some("Choose an area"),
    )
    val enc = menu.asJson
    expect(enc.hcursor.downField("type").as[Int] == Right(3)) and
      expect(enc.hcursor.downField("custom_id").as[String] == Right("area_select")) and
      expect(enc.hcursor.downField("placeholder").as[String] == Right("Choose an area")) and
      expect(enc.hcursor.downField("options").as[List[Json]].map(_.size) == Right(2))
  }

  pureTest("ActionRow encodes with type=1 and wraps select menu in components array") {
    val menu = StringSelectMenu("sel", List(SelectOption("A", "a")))
    val row  = ActionRow(menu)
    val enc  = row.asJson
    expect(enc.hcursor.downField("type").as[Int] == Right(1)) and
      expect(enc.hcursor.downField("components").as[List[Json]].map(_.size) == Right(1)) and
      expect(enc.hcursor.downField("components").downArray.downField("type").as[Int] == Right(3))
  }

  pureTest("InteractionCallbackData.withSelectMenu encodes components in response") {
    val menu = StringSelectMenu("sel", List(SelectOption("Asgarnia", "asgarnia")))
    val data = InteractionCallbackData.withSelectMenu("Pick area:", menu)
    val enc  = data.asJson
    expect(enc.hcursor.downField("content").as[String] == Right("Pick area:")) and
      expect(enc.hcursor.downField("components").as[List[Json]].map(_.size) == Right(1))
  }

  pureTest("InteractionCallbackData encoder drops null fields") {
    val data = InteractionCallbackData(content = Some("hello"))
    val enc  = data.asJson
    expect(!enc.hcursor.keys.exists(_.toList.contains("embeds"))) and
      expect(!enc.hcursor.keys.exists(_.toList.contains("components")))
  }

  pureTest("InteractionResponse.replyWithSelectMenu wraps in ephemeral response") {
    val menu = StringSelectMenu("sel", List(SelectOption("A", "a")))
    val resp = InteractionResponse.replyWithSelectMenu("Pick:", menu)
    val enc  = resp.asJson
    expect(enc.hcursor.downField("type").as[Int] == Right(4)) and
      expect(enc.hcursor.downField("data").downField("flags").as[Int] == Right(64))
  }

// ── ComponentRouter tests ────────────────────────────────────────────────────

object ComponentRouterSuite extends SimpleIOSuite:

  private val fakeUser = User(Snowflake(0L), "testuser", "0000", None, None, None, None)

  private def makeComponentInteraction(customId: String, values: List[String] = Nil): ChannelInteraction =
    ChannelInteraction(
      id            = Snowflake(100L),
      applicationId = Snowflake(200L),
      `type`        = InteractionType.MessageComponent,
      data          = None,
      channelId     = None,
      user          = fakeUser,
      token         = "tok",
      version       = 1,
      componentData = Some(MessageComponentData(customId, 3, if values.isEmpty then None else Some(values))),
    )

  test("router routes to correct handler by exact customId") {
    for
      called <- IO.ref(false)
      client  = ComponentStubRestClient(called)
      router  = ComponentRouter[IO].on("area_select")(_ => IO.pure(InteractionResponse.reply("ok")))
      _      <- router.dispatch(makeComponentInteraction("area_select"), client)
      r      <- called.get
    yield expect(r)
  }

  test("router routes to prefix handler") {
    for
      called <- IO.ref(false)
      client  = ComponentStubRestClient(called)
      router  = ComponentRouter[IO].onPrefix("submit_task:")(_ => IO.pure(InteractionResponse.reply("ok")))
      _      <- router.dispatch(makeComponentInteraction("submit_task:abc123"), client)
      r      <- called.get
    yield expect(r)
  }

  test("prefix handler receives full customId") {
    for
      received <- IO.ref("")
      calledRef <- IO.ref(false)
      client    = ComponentStubRestClient(calledRef)
      router    = ComponentRouter[IO].onPrefix("submit_task:") { interaction =>
                    interaction.componentCustomId.traverse(id => received.set(id)) *>
                      IO.pure(InteractionResponse.reply("ok"))
                  }
      _        <- router.dispatch(makeComponentInteraction("submit_task:abc123"), client)
      r        <- received.get
    yield expect(r == "submit_task:abc123")
  }

  test("router ignores non-MessageComponent interactions") {
    for
      called <- IO.ref(false)
      client  = ComponentStubRestClient(called)
      router  = ComponentRouter[IO].on("area_select")(_ => IO.pure(InteractionResponse.reply("ok")))
      nonComponent = makeComponentInteraction("area_select").copy(`type` = InteractionType.ApplicationCommand)
      _      <- router.dispatch(nonComponent, client)
      r      <- called.get
    yield expect(!r)
  }

  test("router ignores unknown customIds silently") {
    for
      called <- IO.ref(false)
      client  = ComponentStubRestClient(called)
      router  = ComponentRouter[IO].on("area_select")(_ => IO.pure(InteractionResponse.reply("ok")))
      _      <- router.dispatch(makeComponentInteraction("unknown"), client)
      r      <- called.get
    yield expect(!r)
  }

  test("componentValues are accessible in handler") {
    for
      received  <- IO.ref(List.empty[String])
      calledRef <- IO.ref(false)
      client     = ComponentStubRestClient(calledRef)
      router     = ComponentRouter[IO].on("area_select") { interaction =>
                     interaction.componentValues.traverse(vs => received.set(vs)) *>
                       IO.pure(InteractionResponse.reply("ok"))
                   }
      _         <- router.dispatch(makeComponentInteraction("area_select", List("Asgarnia", "Kandarin")), client)
      r         <- received.get
    yield expect(r == List("Asgarnia", "Kandarin"))
  }

class ComponentStubRestClient(called: Ref[IO, Boolean]) extends discats.rest.RestClient[IO]:
  def respondToInteraction(id: Snowflake, token: String, r: InteractionResponse): IO[Unit] =
    called.set(true)
  def getChannel(channelId: Snowflake): IO[Channel]                                              = IO.raiseError(notImpl)
  def deleteChannel(channelId: Snowflake): IO[Unit]                                              = IO.raiseError(notImpl)
  def getMessage(channelId: Snowflake, messageId: Snowflake): IO[discats.models.Message]         = IO.raiseError(notImpl)
  def getMessages(channelId: Snowflake, limit: Int): IO[List[discats.models.Message]]            = IO.raiseError(notImpl)
  def sendMessage(channelId: Snowflake, body: MessageCreate): IO[discats.models.Message]         = IO.raiseError(notImpl)
  def deleteMessage(channelId: Snowflake, messageId: Snowflake): IO[Unit]                        = IO.raiseError(notImpl)
  def editMessage(c: Snowflake, m: Snowflake, b: MessageCreate): IO[discats.models.Message]      = IO.raiseError(notImpl)
  def getGuild(guildId: Snowflake): IO[Guild]                                                    = IO.raiseError(notImpl)
  def getGuildChannels(guildId: Snowflake): IO[List[Channel]]                                    = IO.raiseError(notImpl)
  def createGuildChannel(guildId: Snowflake, name: String): IO[Channel]                         = IO.raiseError(notImpl)
  def createGuildChannelWithOverwrites(guildId: Snowflake, name: String, overwrites: List[PermissionOverwrite], parentId: Option[Snowflake]): IO[Channel] = IO.raiseError(notImpl)
  def createGuildCategory(guildId: Snowflake, name: String): IO[Channel]                        = IO.raiseError(notImpl)
  def getUser(userId: Snowflake): IO[User]                                                       = IO.raiseError(notImpl)
  def addReaction(channelId: Snowflake, messageId: Snowflake, emoji: String): IO[Unit]           = IO.raiseError(notImpl)
  def registerGlobalCommand(appId: Snowflake, cmd: ApplicationCommand): IO[RegisteredCommand]   = IO.raiseError(notImpl)
  def registerGuildCommand(a: Snowflake, g: Snowflake, c: ApplicationCommand): IO[RegisteredCommand] = IO.raiseError(notImpl)
  def listGlobalCommands(appId: Snowflake): IO[List[RegisteredCommand]]                         = IO.raiseError(notImpl)
  def deleteGlobalCommand(a: Snowflake, c: Snowflake): IO[Unit]                                 = IO.raiseError(notImpl)
  def deleteGuildCommand(a: Snowflake, g: Snowflake, c: Snowflake): IO[Unit]                    = IO.raiseError(notImpl)
  def editInteractionResponse(a: Snowflake, t: String, b: MessageCreate): IO[discats.models.Message] = IO.raiseError(notImpl)
  def deleteInteractionResponse(a: Snowflake, t: String): IO[Unit]                              = IO.raiseError(notImpl)
  private val notImpl = new NotImplementedError("stub")
