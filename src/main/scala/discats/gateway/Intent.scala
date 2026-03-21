package discats.gateway

/** Gateway intents bitfield. Combine with `|`. */
opaque type Intent = Int

object Intent:
  def apply(bits: Int): Intent = bits

  val Guilds: Intent                       = 1 << 0
  val GuildMembers: Intent                 = 1 << 1
  val GuildModeration: Intent              = 1 << 2
  val GuildEmojisAndStickers: Intent       = 1 << 3
  val GuildIntegrations: Intent            = 1 << 4
  val GuildWebhooks: Intent                = 1 << 5
  val GuildInvites: Intent                 = 1 << 6
  val GuildVoiceStates: Intent             = 1 << 7
  val GuildPresences: Intent               = 1 << 8
  val GuildMessages: Intent                = 1 << 9
  val GuildMessageReactions: Intent        = 1 << 10
  val GuildMessageTyping: Intent           = 1 << 11
  val DirectMessages: Intent               = 1 << 12
  val DirectMessageReactions: Intent       = 1 << 13
  val DirectMessageTyping: Intent          = 1 << 14
  val MessageContent: Intent               = 1 << 15
  val GuildScheduledEvents: Intent         = 1 << 16
  val AutoModerationConfiguration: Intent  = 1 << 20
  val AutoModerationExecution: Intent      = 1 << 21

  /** Convenience: all non-privileged intents. */
  val AllNonPrivileged: Intent =
    Guilds | GuildModeration | GuildEmojisAndStickers | GuildIntegrations |
    GuildWebhooks | GuildInvites | GuildVoiceStates | GuildMessages |
    GuildMessageReactions | GuildMessageTyping | DirectMessages |
    DirectMessageReactions | DirectMessageTyping | GuildScheduledEvents |
    AutoModerationConfiguration | AutoModerationExecution

  extension (a: Intent)
    def |(b: Intent): Intent  = Intent(a | b)
    def &(b: Intent): Intent  = Intent(a & b)
    def value: Int            = a
