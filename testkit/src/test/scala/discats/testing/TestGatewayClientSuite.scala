package discats.testing

import cats.effect.IO
import discats.gateway.GatewayEvent
import discats.models.*
import weaver.SimpleIOSuite

object TestGatewayClientSuite extends SimpleIOSuite:

  private val fakeUser = User(Snowflake(1L), "user", "0000", None, None, None, None)
  private val fakeMsg  = Message(Snowflake(10L), Snowflake(20L), None, fakeUser, "hi", "2021-01-01T00:00:00Z", None, tts = false, mentionEveryone = false, pinned = false, Nil, 0)

  test("emit + complete produces a single-event stream") {
    for
      gateway <- TestGatewayClient.create[IO]
      _       <- gateway.emit(GatewayEvent.MessageCreate(fakeMsg))
      _       <- gateway.complete
      events  <- gateway.events.compile.toList
    yield expect(events == List(GatewayEvent.MessageCreate(fakeMsg)))
  }

  test("multiple emits preserve order") {
    val event1 = GatewayEvent.MessageCreate(fakeMsg)
    val event2 = GatewayEvent.MessageCreate(fakeMsg.copy(id = Snowflake(11L)))
    val event3 = GatewayEvent.MessageCreate(fakeMsg.copy(id = Snowflake(12L)))
    for
      gateway <- TestGatewayClient.create[IO]
      _       <- gateway.emit(event1)
      _       <- gateway.emit(event2)
      _       <- gateway.emit(event3)
      _       <- gateway.complete
      events  <- gateway.events.compile.toList
    yield expect(events == List(event1, event2, event3))
  }

  test("complete with no events produces an empty stream") {
    for
      gateway <- TestGatewayClient.create[IO]
      _       <- gateway.complete
      events  <- gateway.events.compile.toList
    yield expect(events.isEmpty)
  }

  test("stream of different event types") {
    val msgEvent  = GatewayEvent.MessageCreate(fakeMsg)
    val delEvent  = GatewayEvent.MessageDelete(Snowflake(10L), Snowflake(20L), None)
    for
      gateway <- TestGatewayClient.create[IO]
      _       <- gateway.emit(msgEvent)
      _       <- gateway.emit(delEvent)
      _       <- gateway.complete
      events  <- gateway.events.compile.toList
    yield expect(events == List(msgEvent, delEvent))
  }
