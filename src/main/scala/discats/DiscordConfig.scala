package discats

import discats.gateway.Intent

/** Configuration for a Discord client.
  *
  * @param token    Bot token (without "Bot " prefix — the library adds it)
  * @param intents  Bitfield of Gateway intents to subscribe to
  * @param apiVersion  Discord API version (default: 10)
  */
final case class DiscordConfig(
    token: String,
    intents: Intent,
    apiVersion: Int = 10,
):
  def authHeader: String       = s"Bot $token"
  def apiBase: String          = s"https://discord.com/api/v$apiVersion"
  def gatewayUrl: String       = s"wss://gateway.discord.gg/?v=$apiVersion&encoding=json"
