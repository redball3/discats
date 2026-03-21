# discats

A functional Scala library for building Discord bots, built on [Cats Effect](https://typelevel.org/cats-effect/), [FS2](https://fs2.io/), and [http4s](https://http4s.org/).

## Features

- **Typed Gateway events** — receive Discord events as a typed ADT stream
- **REST client** — send messages, manage channels, and interact with guilds
- **Slash command routing** — fluent builder for handling slash command interactions
- **Rate limiting** — automatic per-bucket rate limit tracking
- **Session resumption** — automatic reconnect and session resume on disconnect
- **Effect-polymorphic** — works with any `cats.effect.Async` effect type

## Installation

Add to your `build.sbt`:

```scala
libraryDependencies += "io.github.discats" %% "discats" % "<version>"
```

## Quick Start

```scala
import cats.effect.*
import discats.*
import discats.models.*

object MyBot extends IOApp.Simple:
  def run: IO[Unit] =
    val config = DiscordConfig(
      token   = sys.env("DISCORD_TOKEN"),
      intents = Intent.GuildMessages | Intent.MessageContent,
    )

    DiscordClient.resource[IO](config).use { client =>
      client.events
        .collect { case GatewayEvent.MessageCreate(msg) => msg }
        .filter(_.content == "!ping")
        .evalMap { msg =>
          client.sendMessage(msg.channelId, MessageCreate("Pong!"))
        }
        .compile
        .drain
    }
```

## Slash Commands

Register commands with Discord and handle them using `SlashCommandRouter`:

```scala
val router = SlashCommandRouter[IO]
  .on("ping") { _ =>
    IO.pure(InteractionResponse.reply("Pong!"))
  }
  .on("echo") { interaction =>
    val text = interaction.stringOption("text").getOrElse("(nothing)")
    IO.pure(InteractionResponse.reply(text))
  }

// Register with Discord
client.registerGlobalCommand(
  applicationId,
  ApplicationCommand("ping", "Replies with Pong!")
    .string("text", "Text to echo"),
)

// Wire the router into the event stream
client.events
  .through(router.pipe(client))
  .compile
  .drain
```

## API Overview

### `DiscordConfig`

```scala
DiscordConfig(
  token      = "Bot <your-token>",
  intents    = Intent.AllNonPrivileged,
  apiVersion = 10,           // default
)
```

### `DiscordClient`

```scala
DiscordClient.resource[IO](config).use { client =>
  // REST operations
  client.sendMessage(channelId, MessageCreate("Hello!"))
  client.getGuild(guildId)
  client.getMessages(channelId, limit = 50)

  // Gateway event stream
  client.events  // Stream[IO, GatewayEvent]
}
```

### Gateway Events

```scala
client.events.evalMap {
  case GatewayEvent.Ready(_, user, _, _, _)    => IO.println(s"Logged in as ${user.username}")
  case GatewayEvent.MessageCreate(msg)         => IO.println(s"${msg.author.username}: ${msg.content}")
  case GatewayEvent.InteractionCreate(i)       => router.dispatch(i, client)
  case GatewayEvent.GuildCreate(guild)         => IO.println(s"Joined ${guild.name}")
  case _                                       => IO.unit
}
```

### Intents

```scala
Intent.GuildMessages | Intent.MessageContent | Intent.DirectMessages

// Or use the convenience constant
Intent.AllNonPrivileged
```

### `InteractionResponse`

```scala
InteractionResponse.reply("Hello!")                  // Public reply
InteractionResponse.replyEphemeral("Only you!")      // Ephemeral reply
InteractionResponse.defer(ephemeral = false)         // Deferred response
```

## License

MIT — see [LICENSE](LICENSE).
