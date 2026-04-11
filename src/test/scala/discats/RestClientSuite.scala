package discats

import cats.effect.IO
import cats.effect.kernel.Resource
import discats.gateway.Intent
import discats.models.*
import discats.rest.{RateLimiter, RestClient}
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.*
import org.http4s.client.Client
import weaver.SimpleIOSuite

object RestClientSuite extends SimpleIOSuite:

  private val config = DiscordConfig(token = "test-token", intents = Intent.Guilds)

  private def makeClient(handler: Request[IO] => IO[Response[IO]]): IO[RestClient[IO]] =
    val httpClient = Client.apply[IO](req => Resource.eval(handler(req)))
    RateLimiter.create[IO].map(rl => RestClient(httpClient, config, rl))

  private def jsonResp(json: Json): IO[Response[IO]] =
    IO.pure(Response[IO](Status.Ok).withEntity(json.noSpaces))

  private def noContentResp: IO[Response[IO]] =
    IO.pure(Response[IO](Status.NoContent))

  // ── getUser ───────────────────────────────────────────────────────────

  test("getUser sends GET to /users/:id and returns decoded User") {
    val userId   = Snowflake(99L)
    val userJson = Json.obj(
      "id"            -> "99".asJson,
      "username"      -> "tester".asJson,
      "discriminator" -> "0001".asJson,
      "global_name"   -> "Tester".asJson,
      "avatar"        -> Json.Null,
    )
    for
      capturedPath <- IO.ref("")
      client <- makeClient { req =>
        capturedPath.set(req.uri.path.renderString) >> jsonResp(userJson)
      }
      user <- client.getUser(userId)
      path <- capturedPath.get
    yield expect(user.id == userId) and
      expect(user.username == "tester") and
      expect(user.globalName == Some("Tester")) and
      expect(path == "/api/v10/users/99")
  }

  // ── createGuildChannel ────────────────────────────────────────────────

  test("createGuildChannel sends POST to /guilds/:id/channels and returns Channel") {
    val guildId     = Snowflake(42L)
    val channelJson = Json.obj(
      "id"              -> "7".asJson,
      "type"            -> 0.asJson,
      "guild_id"        -> "42".asJson,
      "name"            -> "leagueslite".asJson,
      "topic"           -> Json.Null,
      "nsfw"            -> false.asJson,
      "last_message_id" -> Json.Null,
      "position"        -> 0.asJson,
      "parent_id"       -> Json.Null,
    )
    for
      capturedMethod <- IO.ref("")
      capturedPath   <- IO.ref("")
      capturedBody   <- IO.ref(Json.Null)
      client <- makeClient { req =>
        for
          body <- req.as[Json]
          _    <- capturedMethod.set(req.method.name)
          _    <- capturedPath.set(req.uri.path.renderString)
          _    <- capturedBody.set(body)
          resp <- jsonResp(channelJson)
        yield resp
      }
      channel <- client.createGuildChannel(guildId, "leagueslite")
      method  <- capturedMethod.get
      path    <- capturedPath.get
      body    <- capturedBody.get
    yield expect(channel.id == Snowflake(7L)) and
      expect(channel.name == Some("leagueslite")) and
      expect(method == "POST") and
      expect(path == "/api/v10/guilds/42/channels") and
      expect((body \\ "name").headOption == Some(Json.fromString("leagueslite"))) and
      expect((body \\ "type").headOption == Some(Json.fromInt(0)))
  }

  // ── addReaction ───────────────────────────────────────────────────────

  test("addReaction sends PUT to reactions endpoint with URL-encoded emoji") {
    val channelId = Snowflake(10L)
    val messageId = Snowflake(20L)
    for
      capturedMethod <- IO.ref("")
      capturedPath   <- IO.ref("")
      client <- makeClient { req =>
        capturedMethod.set(req.method.name) >>
          capturedPath.set(req.uri.path.renderString) >>
          noContentResp
      }
      _ <- client.addReaction(channelId, messageId, "✅")
      method <- capturedMethod.get
      path   <- capturedPath.get
    yield expect(method == "PUT") and
      expect(path.startsWith("/api/v10/channels/10/messages/20/reactions/")) and
      expect(path.endsWith("/@me")) and
      expect(path.contains("%E2%9C%85") || path.contains("%e2%9c%85"))
  }

  test("addReaction with plain ASCII emoji encodes correctly") {
    val channelId = Snowflake(1L)
    val messageId = Snowflake(2L)
    for
      capturedPath <- IO.ref("")
      client <- makeClient { req =>
        capturedPath.set(req.uri.path.renderString) >> noContentResp
      }
      _    <- client.addReaction(channelId, messageId, "👍")
      path <- capturedPath.get
    yield expect(path.endsWith("/@me")) and
      expect(!path.contains("👍"))
  }

  // ── createGuildChannelWithOverwrites ──────────────────────────────────

  test("createGuildChannelWithOverwrites sends permission_overwrites in body") {
    val guildId = Snowflake(42L)
    val overwrite = PermissionOverwrite(id = "99", `type` = 0, allow = "8", deny = "0")
    val channelJson = Json.obj(
      "id"              -> "7".asJson,
      "type"            -> 0.asJson,
      "guild_id"        -> "42".asJson,
      "name"            -> "secret".asJson,
      "topic"           -> Json.Null,
      "nsfw"            -> false.asJson,
      "last_message_id" -> Json.Null,
      "position"        -> 0.asJson,
      "parent_id"       -> Json.Null,
    )
    for
      capturedMethod <- IO.ref("")
      capturedPath   <- IO.ref("")
      capturedBody   <- IO.ref(Json.Null)
      client <- makeClient { req =>
        for
          body <- req.as[Json]
          _    <- capturedMethod.set(req.method.name)
          _    <- capturedPath.set(req.uri.path.renderString)
          _    <- capturedBody.set(body)
          resp <- jsonResp(channelJson)
        yield resp
      }
      channel <- client.createGuildChannelWithOverwrites(guildId, "secret", List(overwrite))
      method  <- capturedMethod.get
      path    <- capturedPath.get
      body    <- capturedBody.get
    yield expect(channel.id == Snowflake(7L)) and
      expect(method == "POST") and
      expect(path == "/api/v10/guilds/42/channels") and
      expect((body \\ "name").headOption == Some(Json.fromString("secret"))) and
      expect((body \\ "type").headOption == Some(Json.fromInt(0))) and
      expect((body \\ "permission_overwrites").headOption.exists(_.isArray))
  }

  test("createGuildChannelWithOverwrites sends empty overwrites list when none provided") {
    val guildId = Snowflake(1L)
    val channelJson = Json.obj(
      "id" -> "5".asJson, "type" -> 0.asJson, "guild_id" -> "1".asJson,
      "name" -> "open".asJson, "topic" -> Json.Null, "nsfw" -> false.asJson,
      "last_message_id" -> Json.Null, "position" -> 0.asJson, "parent_id" -> Json.Null,
    )
    for
      capturedBody <- IO.ref(Json.Null)
      client <- makeClient { req =>
        req.as[Json].flatMap(b => capturedBody.set(b) >> jsonResp(channelJson))
      }
      _ <- client.createGuildChannelWithOverwrites(guildId, "open", Nil)
      body <- capturedBody.get
    yield expect((body \\ "permission_overwrites").headOption == Some(Json.arr()))
  }

  test("createGuildCategory sends category type in body") {
    val guildId = Snowflake(55L)
    val channelJson = Json.obj(
      "id" -> "9".asJson, "type" -> 4.asJson, "guild_id" -> "55".asJson,
      "name" -> "bingo".asJson, "topic" -> Json.Null, "nsfw" -> false.asJson,
      "last_message_id" -> Json.Null, "position" -> 0.asJson, "parent_id" -> Json.Null,
    )
    for
      capturedBody <- IO.ref(Json.Null)
      client <- makeClient { req =>
        req.as[Json].flatMap(b => capturedBody.set(b) >> jsonResp(channelJson))
      }
      _    <- client.createGuildCategory(guildId, "bingo")
      body <- capturedBody.get
    yield expect((body \\ "name").headOption == Some(Json.fromString("bingo"))) and
      expect((body \\ "type").headOption == Some(Json.fromInt(4)))
  }
