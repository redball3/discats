package discats

import cats.effect.*
import cats.syntax.all.*
import discats.gateway.{GatewayClient, GatewayEvent, Intent}
import discats.models.*
import discats.rest.{RateLimiter, RestClient}
import fs2.Stream
import org.http4s.ember.client.EmberClientBuilder

/** Combined Discord client: REST + Gateway.
  *
  * Obtain an instance via [[DiscordClient.resource]].
  */
trait DiscordClient[F[_]] extends RestClient[F]:
  def gateway: GatewayClient[F]

  /** Convenience: the gateway event stream. */
  def events: Stream[F, GatewayEvent] = gateway.events

object DiscordClient:

  /** Allocate a [[DiscordClient]] as a [[Resource]].
    *
    * The resource manages the lifecycle of the underlying HTTP client;
    * releasing it closes all connections.
    *
    * @param config  Bot token, intents, API version.
    */
  def resource[F[_]: Async](config: DiscordConfig): Resource[F, DiscordClient[F]] =
    EmberClientBuilder
      .default[F]
      .build
      .evalMap { httpClient =>
        RateLimiter.create[F].map { rl =>
          val rest    = RestClient(httpClient, config, rl)
          val gateway = GatewayClient(config)
          new Impl(rest, gateway)
        }
      }

  private class Impl[F[_]](
      rest: RestClient[F],
      val gateway: GatewayClient[F],
  ) extends DiscordClient[F]:
    export rest.*
