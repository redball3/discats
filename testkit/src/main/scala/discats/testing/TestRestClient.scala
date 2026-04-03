package discats.testing

import cats.effect.kernel.{Concurrent, Ref}
import cats.syntax.all.*
import discats.models.*
import discats.rest.RestClient

/** An in-memory, fully-controllable implementation of [[RestClient]] for use in tests.
  *
  * Every call is recorded and dispatched to a configurable stub function. Methods
  * without a configured stub raise [[NotStubbedException]], making oversight visible
  * immediately. Use [[acceptAllInteractions]] as a quick opt-out for
  * [[respondToInteraction]] when you only care about other behaviour.
  *
  * {{{
  * // Example — verify that a ping command replies "Pong!"
  * for
  *   client <- TestRestClient.create[IO]
  *   _      <- client.acceptAllInteractions
  *   router  = SlashCommandRouter[IO].on("ping")(_ => IO.pure(InteractionResponse.reply("Pong!")))
  *   _      <- router.dispatch(pingInteraction, client)
  *   calls  <- client.interactionResponses
  * yield expect(calls.head._3 == InteractionResponse.reply("Pong!"))
  * }}}
  *
  * Obtain an instance via [[TestRestClient.create]].
  */
final class TestRestClient[F[_]] private (state: Ref[F, TestRestClient.State[F]])(using F: Concurrent[F])
    extends RestClient[F]:

  // ── Stub configuration ────────────────────────────────────────────────────

  /** Configure the response for [[getChannel]]. */
  def stubGetChannel(f: Snowflake => F[Channel]): F[Unit] =
    state.update(_.copy(getChannelFn = Some(f)))

  /** Configure the response for [[getMessage]]. */
  def stubGetMessage(f: (Snowflake, Snowflake) => F[Message]): F[Unit] =
    state.update(_.copy(getMessageFn = Some(f)))

  /** Configure the response for [[getMessages]]. */
  def stubGetMessages(f: (Snowflake, Int) => F[List[Message]]): F[Unit] =
    state.update(_.copy(getMessagesFn = Some(f)))

  /** Configure the response for [[sendMessage]]. */
  def stubSendMessage(f: (Snowflake, MessageCreate) => F[Message]): F[Unit] =
    state.update(_.copy(sendMessageFn = Some(f)))

  /** Configure the response for [[deleteMessage]]. */
  def stubDeleteMessage(f: (Snowflake, Snowflake) => F[Unit]): F[Unit] =
    state.update(_.copy(deleteMessageFn = Some(f)))

  /** Configure the response for [[editMessage]]. */
  def stubEditMessage(f: (Snowflake, Snowflake, MessageCreate) => F[Message]): F[Unit] =
    state.update(_.copy(editMessageFn = Some(f)))

  /** Configure the response for [[getGuild]]. */
  def stubGetGuild(f: Snowflake => F[Guild]): F[Unit] =
    state.update(_.copy(getGuildFn = Some(f)))

  /** Configure the response for [[getGuildChannels]]. */
  def stubGetGuildChannels(f: Snowflake => F[List[Channel]]): F[Unit] =
    state.update(_.copy(getGuildChannelsFn = Some(f)))

  /** Configure the response for [[createGuildChannel]]. */
  def stubCreateGuildChannel(f: (Snowflake, String) => F[Channel]): F[Unit] =
    state.update(_.copy(createGuildChannelFn = Some(f)))

  /** Configure the response for [[createGuildChannelWithOverwrites]]. */
  def stubCreateGuildChannelWithOverwrites(f: (Snowflake, String, List[PermissionOverwrite]) => F[Channel]): F[Unit] =
    state.update(_.copy(createGuildChannelWithOverwritesFn = Some(f)))

  /** Configure the response for [[getUser]]. */
  def stubGetUser(f: Snowflake => F[User]): F[Unit] =
    state.update(_.copy(getUserFn = Some(f)))

  /** Configure the response for [[addReaction]]. */
  def stubAddReaction(f: (Snowflake, Snowflake, String) => F[Unit]): F[Unit] =
    state.update(_.copy(addReactionFn = Some(f)))

  /** Configure the response for [[registerGlobalCommand]]. */
  def stubRegisterGlobalCommand(f: (Snowflake, ApplicationCommand) => F[RegisteredCommand]): F[Unit] =
    state.update(_.copy(registerGlobalCommandFn = Some(f)))

  /** Configure the response for [[registerGuildCommand]]. */
  def stubRegisterGuildCommand(f: (Snowflake, Snowflake, ApplicationCommand) => F[RegisteredCommand]): F[Unit] =
    state.update(_.copy(registerGuildCommandFn = Some(f)))

  /** Configure the response for [[listGlobalCommands]]. */
  def stubListGlobalCommands(f: Snowflake => F[List[RegisteredCommand]]): F[Unit] =
    state.update(_.copy(listGlobalCommandsFn = Some(f)))

  /** Configure the response for [[deleteGlobalCommand]]. */
  def stubDeleteGlobalCommand(f: (Snowflake, Snowflake) => F[Unit]): F[Unit] =
    state.update(_.copy(deleteGlobalCommandFn = Some(f)))

  /** Configure the response for [[deleteGuildCommand]]. */
  def stubDeleteGuildCommand(f: (Snowflake, Snowflake, Snowflake) => F[Unit]): F[Unit] =
    state.update(_.copy(deleteGuildCommandFn = Some(f)))

  /** Configure the response for [[respondToInteraction]]. */
  def stubRespondToInteraction(f: (Snowflake, String, InteractionResponse) => F[Unit]): F[Unit] =
    state.update(_.copy(respondToInteractionFn = Some(f)))

  /** Configure the response for [[editInteractionResponse]]. */
  def stubEditInteractionResponse(f: (Snowflake, String, MessageCreate) => F[Message]): F[Unit] =
    state.update(_.copy(editInteractionResponseFn = Some(f)))

  /** Configure the response for [[deleteInteractionResponse]]. */
  def stubDeleteInteractionResponse(f: (Snowflake, String) => F[Unit]): F[Unit] =
    state.update(_.copy(deleteInteractionResponseFn = Some(f)))

  /** Convenience: stub [[respondToInteraction]] to succeed silently.
    * Use this when you want to verify other behaviour and don't care about interaction replies.
    */
  def acceptAllInteractions: F[Unit] =
    stubRespondToInteraction((_, _, _) => F.unit)

  // ── Recorded calls ────────────────────────────────────────────────────────

  /** All `(channelId, body)` pairs passed to [[sendMessage]], in call order. */
  def sentMessages: F[List[(Snowflake, MessageCreate)]] =
    state.get.map(_.sentMessages)

  /** All `(interactionId, token, response)` triples passed to [[respondToInteraction]], in call order. */
  def interactionResponses: F[List[(Snowflake, String, InteractionResponse)]] =
    state.get.map(_.interactionResponses)

  /** All `(channelId, messageId)` pairs passed to [[deleteMessage]], in call order. */
  def deletedMessages: F[List[(Snowflake, Snowflake)]] =
    state.get.map(_.deletedMessages)

  /** All commands passed to [[registerGlobalCommand]] or [[registerGuildCommand]], in call order. */
  def registeredCommands: F[List[ApplicationCommand]] =
    state.get.map(_.registeredCommands)

  /** All `(guildId, name)` pairs passed to [[createGuildChannel]], in call order. */
  def createdChannels: F[List[(Snowflake, String)]] =
    state.get.map(_.createdChannels)

  /** All `(guildId, name, overwrites)` triples passed to [[createGuildChannelWithOverwrites]], in call order. */
  def createdPrivateChannels: F[List[(Snowflake, String, List[PermissionOverwrite])]] =
    state.get.map(_.createdPrivateChannels)

  /** All `(channelId, messageId, emoji)` triples passed to [[addReaction]], in call order. */
  def addedReactions: F[List[(Snowflake, Snowflake, String)]] =
    state.get.map(_.addedReactions)

  /** Reset all recorded calls and remove all configured stubs, returning to a clean state. */
  def reset: F[Unit] = state.set(TestRestClient.State())

  // ── RestClient implementation ─────────────────────────────────────────────

  private def notStubbed[A](method: String): F[A] =
    F.raiseError(NotStubbedException(method))

  def getChannel(channelId: Snowflake): F[Channel] =
    state.get.flatMap(_.getChannelFn.fold(notStubbed("getChannel"))(_(channelId)))

  def getMessage(channelId: Snowflake, messageId: Snowflake): F[Message] =
    state.get.flatMap(_.getMessageFn.fold(notStubbed("getMessage"))(_(channelId, messageId)))

  def getMessages(channelId: Snowflake, limit: Int): F[List[Message]] =
    state.get.flatMap(_.getMessagesFn.fold(notStubbed("getMessages"))(_(channelId, limit)))

  def sendMessage(channelId: Snowflake, body: MessageCreate): F[Message] =
    state.flatModify { s =>
      val next = s.copy(sentMessages = s.sentMessages :+ (channelId, body))
      next -> s.sendMessageFn.fold(notStubbed("sendMessage"))(_(channelId, body))
    }

  def deleteMessage(channelId: Snowflake, messageId: Snowflake): F[Unit] =
    state.flatModify { s =>
      val next = s.copy(deletedMessages = s.deletedMessages :+ (channelId, messageId))
      next -> s.deleteMessageFn.fold(notStubbed("deleteMessage"))(_(channelId, messageId))
    }

  def editMessage(channelId: Snowflake, messageId: Snowflake, body: MessageCreate): F[Message] =
    state.get.flatMap(_.editMessageFn.fold(notStubbed("editMessage"))(_(channelId, messageId, body)))

  def getGuild(guildId: Snowflake): F[Guild] =
    state.get.flatMap(_.getGuildFn.fold(notStubbed("getGuild"))(_(guildId)))

  def getGuildChannels(guildId: Snowflake): F[List[Channel]] =
    state.get.flatMap(_.getGuildChannelsFn.fold(notStubbed("getGuildChannels"))(_(guildId)))

  def createGuildChannel(guildId: Snowflake, name: String): F[Channel] =
    state.flatModify { s =>
      val next = s.copy(createdChannels = s.createdChannels :+ (guildId, name))
      next -> s.createGuildChannelFn.fold(notStubbed("createGuildChannel"))(_(guildId, name))
    }

  def createGuildChannelWithOverwrites(
      guildId: Snowflake,
      name: String,
      overwrites: List[PermissionOverwrite],
  ): F[Channel] =
    state.flatModify { s =>
      val next = s.copy(createdPrivateChannels = s.createdPrivateChannels :+ (guildId, name, overwrites))
      next -> s.createGuildChannelWithOverwritesFn.fold(notStubbed("createGuildChannelWithOverwrites"))(_(guildId, name, overwrites))
    }

  def getUser(userId: Snowflake): F[User] =
    state.get.flatMap(_.getUserFn.fold(notStubbed("getUser"))(_(userId)))

  def addReaction(channelId: Snowflake, messageId: Snowflake, emoji: String): F[Unit] =
    state.flatModify { s =>
      val next = s.copy(addedReactions = s.addedReactions :+ (channelId, messageId, emoji))
      next -> s.addReactionFn.fold(notStubbed("addReaction"))(_(channelId, messageId, emoji))
    }

  def registerGlobalCommand(applicationId: Snowflake, command: ApplicationCommand): F[RegisteredCommand] =
    state.flatModify { s =>
      val next = s.copy(registeredCommands = s.registeredCommands :+ command)
      next -> s.registerGlobalCommandFn.fold(notStubbed("registerGlobalCommand"))(_(applicationId, command))
    }

  def registerGuildCommand(applicationId: Snowflake, guildId: Snowflake, command: ApplicationCommand): F[RegisteredCommand] =
    state.flatModify { s =>
      val next = s.copy(registeredCommands = s.registeredCommands :+ command)
      next -> s.registerGuildCommandFn.fold(notStubbed("registerGuildCommand"))(_(applicationId, guildId, command))
    }

  def listGlobalCommands(applicationId: Snowflake): F[List[RegisteredCommand]] =
    state.get.flatMap(_.listGlobalCommandsFn.fold(notStubbed("listGlobalCommands"))(_(applicationId)))

  def deleteGlobalCommand(applicationId: Snowflake, commandId: Snowflake): F[Unit] =
    state.get.flatMap(_.deleteGlobalCommandFn.fold(notStubbed("deleteGlobalCommand"))(_(applicationId, commandId)))

  def deleteGuildCommand(applicationId: Snowflake, guildId: Snowflake, commandId: Snowflake): F[Unit] =
    state.get.flatMap(_.deleteGuildCommandFn.fold(notStubbed("deleteGuildCommand"))(_(applicationId, guildId, commandId)))

  def respondToInteraction(interactionId: Snowflake, token: String, response: InteractionResponse): F[Unit] =
    state.flatModify { s =>
      val next = s.copy(interactionResponses = s.interactionResponses :+ (interactionId, token, response))
      next -> s.respondToInteractionFn.fold(notStubbed("respondToInteraction"))(_(interactionId, token, response))
    }

  def editInteractionResponse(applicationId: Snowflake, token: String, body: MessageCreate): F[Message] =
    state.get.flatMap(_.editInteractionResponseFn.fold(notStubbed("editInteractionResponse"))(_(applicationId, token, body)))

  def deleteInteractionResponse(applicationId: Snowflake, token: String): F[Unit] =
    state.get.flatMap(_.deleteInteractionResponseFn.fold(notStubbed("deleteInteractionResponse"))(_(applicationId, token)))

object TestRestClient:

  private[testing] case class State[F[_]](
    // stubs
    getChannelFn:                Option[Snowflake => F[Channel]]                                               = None,
    getMessageFn:                Option[(Snowflake, Snowflake) => F[Message]]                                  = None,
    getMessagesFn:               Option[(Snowflake, Int) => F[List[Message]]]                                  = None,
    sendMessageFn:               Option[(Snowflake, MessageCreate) => F[Message]]                              = None,
    deleteMessageFn:             Option[(Snowflake, Snowflake) => F[Unit]]                                     = None,
    editMessageFn:               Option[(Snowflake, Snowflake, MessageCreate) => F[Message]]                   = None,
    getGuildFn:                  Option[Snowflake => F[Guild]]                                                 = None,
    getGuildChannelsFn:          Option[Snowflake => F[List[Channel]]]                                         = None,
    createGuildChannelFn:        Option[(Snowflake, String) => F[Channel]]                                     = None,
    createGuildChannelWithOverwritesFn: Option[(Snowflake, String, List[PermissionOverwrite]) => F[Channel]]   = None,
    getUserFn:                   Option[Snowflake => F[User]]                                                  = None,
    addReactionFn:               Option[(Snowflake, Snowflake, String) => F[Unit]]                             = None,
    registerGlobalCommandFn:     Option[(Snowflake, ApplicationCommand) => F[RegisteredCommand]]               = None,
    registerGuildCommandFn:      Option[(Snowflake, Snowflake, ApplicationCommand) => F[RegisteredCommand]]    = None,
    listGlobalCommandsFn:        Option[Snowflake => F[List[RegisteredCommand]]]                               = None,
    deleteGlobalCommandFn:       Option[(Snowflake, Snowflake) => F[Unit]]                                     = None,
    deleteGuildCommandFn:        Option[(Snowflake, Snowflake, Snowflake) => F[Unit]]                          = None,
    respondToInteractionFn:      Option[(Snowflake, String, InteractionResponse) => F[Unit]]                   = None,
    editInteractionResponseFn:   Option[(Snowflake, String, MessageCreate) => F[Message]]                      = None,
    deleteInteractionResponseFn: Option[(Snowflake, String) => F[Unit]]                                        = None,
    // recordings
    sentMessages:          List[(Snowflake, MessageCreate)]                   = Nil,
    interactionResponses:  List[(Snowflake, String, InteractionResponse)]     = Nil,
    deletedMessages:       List[(Snowflake, Snowflake)]                       = Nil,
    registeredCommands:    List[ApplicationCommand]                           = Nil,
    createdChannels:       List[(Snowflake, String)]                          = Nil,
    createdPrivateChannels: List[(Snowflake, String, List[PermissionOverwrite])] = Nil,
    addedReactions:        List[(Snowflake, Snowflake, String)]               = Nil,
  )

  /** Create a fresh [[TestRestClient]] with no stubs configured. */
  def create[F[_]: Concurrent]: F[TestRestClient[F]] =
    Ref[F].of(State[F]()).map(new TestRestClient(_))
