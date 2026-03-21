package discats.rest

import cats.effect.*
import cats.syntax.all.*
import org.http4s.Headers

import scala.concurrent.duration.*

/** Per-bucket rate limit state. */
private[rest] final case class RateLimitBucket(
    remaining: Int,
    resetAfter: FiniteDuration,
)

/** Tracks Discord REST rate limits on a per-route basis.
  *
  * Discord sends:
  *   X-RateLimit-Remaining   — requests left in this window
  *   X-RateLimit-Reset-After — seconds until bucket resets
  *   X-RateLimit-Bucket      — opaque bucket ID
  */
trait RateLimiter[F[_]]:
  /** Block until the given bucket allows a request, then run `fa`. */
  def apply[A](bucketKey: String)(fa: F[A]): F[A]

  /** Update bucket state from Discord response headers. */
  def updateFromHeaders(bucketKey: String, headers: Headers): F[Unit]

object RateLimiter:
  def create[F[_]: Async]: F[RateLimiter[F]] =
    Ref.of[F, Map[String, RateLimitBucket]](Map.empty).map(new Impl(_))

  private class Impl[F[_]](
      buckets: Ref[F, Map[String, RateLimitBucket]],
  )(using F: Async[F])
      extends RateLimiter[F]:

    def apply[A](bucketKey: String)(fa: F[A]): F[A] =
      buckets.get.flatMap { state =>
        state.get(bucketKey) match
          case Some(b) if b.remaining <= 0 => F.sleep(b.resetAfter) >> fa
          case _                           => fa
      }

    def updateFromHeaders(bucketKey: String, headers: Headers): F[Unit] =
      val remaining = headers.headers
        .find(_.name.toString.equalsIgnoreCase("x-ratelimit-remaining"))
        .flatMap(_.value.toIntOption)
      val resetAfter = headers.headers
        .find(_.name.toString.equalsIgnoreCase("x-ratelimit-reset-after"))
        .flatMap(_.value.toDoubleOption)
        .map(d => (d * 1000).toLong.millis)
      (remaining, resetAfter) match
        case (Some(r), Some(ra)) =>
          buckets.update(_.updated(bucketKey, RateLimitBucket(r, ra)))
        case _ =>
          F.unit
