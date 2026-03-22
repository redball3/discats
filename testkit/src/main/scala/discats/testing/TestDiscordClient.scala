package discats.testing

import cats.effect.kernel.Concurrent
import cats.syntax.all.*
import discats.DiscordClient
import discats.gateway.GatewayClient

/** A fully-controllable in-memory [[DiscordClient]] for use in tests.
  *
  * Combines a [[TestRestClient]] and a [[TestGatewayClient]] behind the
  * standard [[DiscordClient]] interface so bot code under test needs no
  * changes. All REST calls are recorded and configurable via `rest`, and
  * Gateway events can be injected via `gateway`.
  *
  * {{{
  * for
  *   client <- TestDiscordClient.create[IO]
  *   _      <- client.rest.acceptAllInteractions
  *   _      <- client.gateway.emit(GatewayEvent.InteractionCreate(pingInteraction))
  *   _      <- client.gateway.complete
  *   _      <- client.events.through(router.pipe(client)).compile.drain
  *   calls  <- client.rest.interactionResponses
  * yield expect(calls.head._3 == InteractionResponse.reply("Pong!"))
  * }}}
  *
  * Obtain an instance via [[TestDiscordClient.create]].
  */
final class TestDiscordClient[F[_]] private (
    val rest: TestRestClient[F],
    val gateway: TestGatewayClient[F],
)(using Concurrent[F])
    extends DiscordClient[F]:
  export rest.*

object TestDiscordClient:

  /** Create a [[TestDiscordClient]] with fresh [[TestRestClient]] and [[TestGatewayClient]] instances. */
  def create[F[_]: Concurrent]: F[TestDiscordClient[F]] =
    for
      rest    <- TestRestClient.create[F]
      gateway <- TestGatewayClient.create[F]
    yield new TestDiscordClient(rest, gateway)
