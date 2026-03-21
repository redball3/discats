package discats.gateway

import io.circe.{Decoder, Encoder}

enum Opcode(val value: Int):
  case Dispatch            extends Opcode(0)
  case Heartbeat           extends Opcode(1)
  case Identify            extends Opcode(2)
  case PresenceUpdate      extends Opcode(3)
  case VoiceStateUpdate    extends Opcode(4)
  case Resume              extends Opcode(6)
  case Reconnect           extends Opcode(7)
  case RequestGuildMembers extends Opcode(8)
  case InvalidSession      extends Opcode(9)
  case Hello               extends Opcode(10)
  case HeartbeatAck        extends Opcode(11)
  case Unknown             extends Opcode(-1)

object Opcode:
  def fromInt(i: Int): Opcode = values.find(_.value == i).getOrElse(Unknown)

  given Decoder[Opcode] = Decoder.decodeInt.map(fromInt)
  given Encoder[Opcode] = Encoder.encodeInt.contramap(_.value)
