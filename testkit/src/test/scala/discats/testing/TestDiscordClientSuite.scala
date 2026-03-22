package discats.testing

import cats.effect.IO
import discats.DiscordClient
import discats.gateway.GatewayEvent
import discats.models.*
import weaver.SimpleIOSuite

object TestDiscordClientSuite extends SimpleIOSuite:

  private val fakeUser = User(Snowflake(1L), "bot", "0000", None, None, Some(true), None)
  private val fakeMsg  = Message(Snowflake(10L), Snowflake(20L), None, fakeUser, "hi", "2021-01-01T00:00:00Z", None, tts = false, mentionEveryone = false, pinned = false, Nil, 0)

  test("create gives a client that satisfies DiscordClient") {
    for
      client <- TestDiscordClient.create[IO]
    yield expect(client.isInstanceOf[DiscordClient[IO]])
  }

  test("REST calls delegate to the rest field") {
    for
      client <- TestDiscordClient.create[IO]
      _      <- client.rest.stubSendMessage((_, _) => IO.pure(fakeMsg))
      msg    <- client.sendMessage(Snowflake(20L), MessageCreate("hello"))
    yield expect(msg == fakeMsg)
  }

  test("sentMessages on rest reflects calls made through the DiscordClient interface") {
    for
      client <- TestDiscordClient.create[IO]
      _      <- client.rest.stubSendMessage((_, _) => IO.pure(fakeMsg))
      _      <- client.sendMessage(Snowflake(20L), MessageCreate("hello"))
      calls  <- client.rest.sentMessages
    yield expect(calls.size == 1)
  }

  test("events stream delegates to the gateway field") {
    val event = GatewayEvent.MessageCreate(fakeMsg)
    for
      client <- TestDiscordClient.create[IO]
      _      <- client.gateway.emit(event)
      _      <- client.gateway.complete
      events <- client.events.compile.toList
    yield expect(events == List(event))
  }

  test("rest and gateway are independently controllable") {
    for
      client <- TestDiscordClient.create[IO]
      _      <- client.rest.acceptAllInteractions
      _      <- client.gateway.emit(GatewayEvent.Resumed)
      _      <- client.gateway.complete
      events <- client.events.compile.toList
      _      <- client.rest.respondToInteraction(Snowflake(1L), "tok", InteractionResponse.reply("ok"))
      calls  <- client.rest.interactionResponses
    yield expect(events == List(GatewayEvent.Resumed)) and expect(calls.size == 1)
  }
