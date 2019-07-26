package Simulation

import java.util.UUID

abstract class Message(){
  val senderId:AgentId
  val receiverId:AgentId
  val session:UUID = UUID.randomUUID()
}

case class MarketBuyOrderMessage(override val senderId:AgentId, override val receiverId:AgentId, units: Int, override val session: UUID) extends Message
case class MarketSellMessage(override val senderId:AgentId, override val receiverId:AgentId, units: Int, price:Double) extends Message

case class PersonSendRegards(override val senderId:AgentId, override val receiverId:AgentId, msg: String) extends Message

case class PersonInfoRequest(override val senderId:AgentId, override val receiverId: AgentId) extends Message
case class PersonInfo(override val senderId:AgentId, override val receiverId:AgentId, personId: AgentId, override val session: UUID) extends Message {
  def this(message:PersonInfoRequest, personId: AgentId) = this(message.receiverId, message.senderId, personId, session = message.session)
}
