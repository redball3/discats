package discats.gateway

import cats.effect.*

/** Internal mutable state for a Gateway connection. */
final case class GatewayState(
    sessionId: Option[String],
    resumeUrl: Option[String],
    seq: Option[Int],
    heartbeatInterval: Option[Long],
)

object GatewayState:
  val empty: GatewayState = GatewayState(None, None, None, None)

  def ref[F[_]: Sync]: F[Ref[F, GatewayState]] =
    Ref.of[F, GatewayState](empty)
