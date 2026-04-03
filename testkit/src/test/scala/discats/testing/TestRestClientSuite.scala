package discats.testing

import cats.effect.IO
import discats.models.*
import weaver.SimpleIOSuite

object TestRestClientSuite extends SimpleIOSuite:

  // ── Fixtures ──────────────────────────────────────────────────────────────

  private val fakeUser = User(Snowflake(1L), "bot", "0000", None, None, Some(true), None)

  private val fakeMessage = Message(
    id              = Snowflake(100L),
    channelId       = Snowflake(200L),
    guildId         = None,
    author          = fakeUser,
    content         = "hello",
    timestamp       = "2021-01-01T00:00:00Z",
    editedTimestamp = None,
    tts             = false,
    mentionEveryone = false,
    pinned          = false,
    embeds          = Nil,
    `type`          = 0,
  )

  private val fakeChannel = Channel(
    id            = Snowflake(200L),
    `type`        = ChannelType.GuildText,
    guildId       = None,
    name          = Some("general"),
    topic         = None,
    nsfw          = None,
    lastMessageId = None,
    position      = Some(0),
    parentId      = None,
  )

  private val fakeCommand     = ApplicationCommand("ping", "Ping the bot")
  private val fakeRegistered  = RegisteredCommand(Snowflake(300L), Snowflake(400L), "ping", "Ping the bot", None)
  private val channelId       = Snowflake(200L)
  private val fakeInteraction = InteractionResponse.reply("Pong!")

  // ── NotStubbedException ───────────────────────────────────────────────────

  test("unstubbed sendMessage raises NotStubbedException") {
    for
      client <- TestRestClient.create[IO]
      result <- client.sendMessage(channelId, MessageCreate("hi")).attempt
    yield expect(result.left.exists(_.isInstanceOf[NotStubbedException]))
  }

  test("unstubbed getChannel raises NotStubbedException") {
    for
      client <- TestRestClient.create[IO]
      result <- client.getChannel(channelId).attempt
    yield expect(result.left.exists(_.isInstanceOf[NotStubbedException]))
  }

  test("NotStubbedException message names the method") {
    for
      client <- TestRestClient.create[IO]
      result <- client.getGuild(Snowflake(1L)).attempt
    yield expect(result.left.exists(_.getMessage.contains("getGuild")))
  }

  // ── sendMessage ───────────────────────────────────────────────────────────

  test("stubSendMessage returns the configured message") {
    for
      client <- TestRestClient.create[IO]
      _      <- client.stubSendMessage((_, _) => IO.pure(fakeMessage))
      msg    <- client.sendMessage(channelId, MessageCreate("hi"))
    yield expect(msg == fakeMessage)
  }

  test("stubSendMessage records the call") {
    for
      client <- TestRestClient.create[IO]
      _      <- client.stubSendMessage((_, _) => IO.pure(fakeMessage))
      _      <- client.sendMessage(channelId, MessageCreate("first"))
      _      <- client.sendMessage(channelId, MessageCreate("second"))
      calls  <- client.sentMessages
    yield expect(calls.map(_._2.content) == List("first", "second"))
  }

  test("sentMessages is empty before any calls") {
    for
      client <- TestRestClient.create[IO]
      calls  <- client.sentMessages
    yield expect(calls.isEmpty)
  }

  test("sentMessages records the channelId alongside the body") {
    val otherId = Snowflake(999L)
    for
      client <- TestRestClient.create[IO]
      _      <- client.stubSendMessage((_, _) => IO.pure(fakeMessage))
      _      <- client.sendMessage(otherId, MessageCreate("hi"))
      calls  <- client.sentMessages
    yield expect(calls.head._1 == otherId)
  }

  // ── respondToInteraction / acceptAllInteractions ──────────────────────────

  test("acceptAllInteractions stubs respondToInteraction to succeed silently") {
    for
      client <- TestRestClient.create[IO]
      _      <- client.acceptAllInteractions
      _      <- client.respondToInteraction(Snowflake(1L), "tok", fakeInteraction)
    yield success
  }

  test("interactionResponses records calls after acceptAllInteractions") {
    for
      client <- TestRestClient.create[IO]
      _      <- client.acceptAllInteractions
      _      <- client.respondToInteraction(Snowflake(1L), "tok-a", fakeInteraction)
      _      <- client.respondToInteraction(Snowflake(2L), "tok-b", fakeInteraction)
      calls  <- client.interactionResponses
    yield expect(calls.map(_._2) == List("tok-a", "tok-b"))
  }

  test("unstubbed respondToInteraction raises NotStubbedException") {
    for
      client <- TestRestClient.create[IO]
      result <- client.respondToInteraction(Snowflake(1L), "tok", fakeInteraction).attempt
    yield expect(result.left.exists(_.isInstanceOf[NotStubbedException]))
  }

  // ── deleteMessage ─────────────────────────────────────────────────────────

  test("stubDeleteMessage records the call") {
    val messageId = Snowflake(55L)
    for
      client <- TestRestClient.create[IO]
      _      <- client.stubDeleteMessage((_, _) => IO.unit)
      _      <- client.deleteMessage(channelId, messageId)
      calls  <- client.deletedMessages
    yield expect(calls == List((channelId, messageId)))
  }

  // ── registerGlobalCommand ─────────────────────────────────────────────────

  test("stubRegisterGlobalCommand records the command") {
    val appId = Snowflake(400L)
    for
      client <- TestRestClient.create[IO]
      _      <- client.stubRegisterGlobalCommand((_, _) => IO.pure(fakeRegistered))
      _      <- client.registerGlobalCommand(appId, fakeCommand)
      cmds   <- client.registeredCommands
    yield expect(cmds == List(fakeCommand))
  }

  test("registerGuildCommand also records the command") {
    val appId   = Snowflake(400L)
    val guildId = Snowflake(500L)
    for
      client <- TestRestClient.create[IO]
      _      <- client.stubRegisterGuildCommand((_, _, _) => IO.pure(fakeRegistered))
      _      <- client.registerGuildCommand(appId, guildId, fakeCommand)
      cmds   <- client.registeredCommands
    yield expect(cmds == List(fakeCommand))
  }

  // ── getChannel ────────────────────────────────────────────────────────────

  test("stubGetChannel returns configured channel") {
    for
      client <- TestRestClient.create[IO]
      _      <- client.stubGetChannel(_ => IO.pure(fakeChannel))
      ch     <- client.getChannel(channelId)
    yield expect(ch == fakeChannel)
  }

  // ── createGuildChannel ────────────────────────────────────────────────────

  test("stubCreateGuildChannel returns configured channel") {
    val guildId = Snowflake(10L)
    for
      client <- TestRestClient.create[IO]
      _      <- client.stubCreateGuildChannel((_, _) => IO.pure(fakeChannel))
      ch     <- client.createGuildChannel(guildId, "general")
    yield expect(ch == fakeChannel)
  }

  test("createGuildChannel records (guildId, name) pairs in order") {
    val guildId = Snowflake(10L)
    for
      client <- TestRestClient.create[IO]
      _      <- client.stubCreateGuildChannel((_, _) => IO.pure(fakeChannel))
      _      <- client.createGuildChannel(guildId, "alpha")
      _      <- client.createGuildChannel(guildId, "beta")
      calls  <- client.createdChannels
    yield expect(calls == List((guildId, "alpha"), (guildId, "beta")))
  }

  test("unstubbed createGuildChannel raises NotStubbedException") {
    for
      client <- TestRestClient.create[IO]
      result <- client.createGuildChannel(Snowflake(1L), "oops").attempt
    yield expect(result.left.exists(_.isInstanceOf[NotStubbedException]))
  }

  // ── createGuildChannelWithOverwrites ──────────────────────────────────────

  test("stubCreateGuildChannelWithOverwrites returns configured channel") {
    val guildId   = Snowflake(20L)
    val overwrite = PermissionOverwrite(id = "99", `type` = 0, allow = "8", deny = "0")
    for
      client <- TestRestClient.create[IO]
      _      <- client.stubCreateGuildChannelWithOverwrites((_, _, _) => IO.pure(fakeChannel))
      ch     <- client.createGuildChannelWithOverwrites(guildId, "secret", List(overwrite))
    yield expect(ch == fakeChannel)
  }

  test("createGuildChannelWithOverwrites records (guildId, name, overwrites) triples") {
    val guildId   = Snowflake(20L)
    val overwrite = PermissionOverwrite(id = "99", `type` = 0, allow = "8", deny = "0")
    for
      client <- TestRestClient.create[IO]
      _      <- client.stubCreateGuildChannelWithOverwrites((_, _, _) => IO.pure(fakeChannel))
      _      <- client.createGuildChannelWithOverwrites(guildId, "secret", List(overwrite))
      calls  <- client.createdPrivateChannels
    yield expect(calls == List((guildId, "secret", List(overwrite))))
  }

  test("unstubbed createGuildChannelWithOverwrites raises NotStubbedException") {
    for
      client <- TestRestClient.create[IO]
      result <- client.createGuildChannelWithOverwrites(Snowflake(1L), "x", Nil).attempt
    yield expect(result.left.exists(_.isInstanceOf[NotStubbedException]))
  }

  // ── getUser ───────────────────────────────────────────────────────────────

  test("stubGetUser returns configured user") {
    for
      client <- TestRestClient.create[IO]
      _      <- client.stubGetUser(_ => IO.pure(fakeUser))
      u      <- client.getUser(Snowflake(1L))
    yield expect(u == fakeUser)
  }

  test("unstubbed getUser raises NotStubbedException") {
    for
      client <- TestRestClient.create[IO]
      result <- client.getUser(Snowflake(1L)).attempt
    yield expect(result.left.exists(_.isInstanceOf[NotStubbedException]))
  }

  // ── addReaction ───────────────────────────────────────────────────────────

  test("stubAddReaction completes successfully") {
    val msgId = Snowflake(55L)
    for
      client <- TestRestClient.create[IO]
      _      <- client.stubAddReaction((_, _, _) => IO.unit)
      _      <- client.addReaction(channelId, msgId, "👍")
    yield success
  }

  test("addReaction records (channelId, messageId, emoji) triples in order") {
    val msgId1 = Snowflake(55L)
    val msgId2 = Snowflake(56L)
    for
      client <- TestRestClient.create[IO]
      _      <- client.stubAddReaction((_, _, _) => IO.unit)
      _      <- client.addReaction(channelId, msgId1, "👍")
      _      <- client.addReaction(channelId, msgId2, "✅")
      calls  <- client.addedReactions
    yield expect(calls == List((channelId, msgId1, "👍"), (channelId, msgId2, "✅")))
  }

  test("unstubbed addReaction raises NotStubbedException") {
    for
      client <- TestRestClient.create[IO]
      result <- client.addReaction(Snowflake(1L), Snowflake(2L), "👍").attempt
    yield expect(result.left.exists(_.isInstanceOf[NotStubbedException]))
  }

  // ── reset ─────────────────────────────────────────────────────────────────

  test("reset clears all recorded calls") {
    for
      client <- TestRestClient.create[IO]
      _      <- client.stubSendMessage((_, _) => IO.pure(fakeMessage))
      _      <- client.sendMessage(channelId, MessageCreate("hi"))
      _      <- client.reset
      calls  <- client.sentMessages
    yield expect(calls.isEmpty)
  }

  test("reset removes configured stubs") {
    for
      client <- TestRestClient.create[IO]
      _      <- client.stubSendMessage((_, _) => IO.pure(fakeMessage))
      _      <- client.reset
      result <- client.sendMessage(channelId, MessageCreate("hi")).attempt
    yield expect(result.left.exists(_.isInstanceOf[NotStubbedException]))
  }
