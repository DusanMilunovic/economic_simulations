package Simulation
import java.util.UUID

import code._

trait PersonInterface {
  def sendRegards(msg: String): Any
}

class PersonProxy(
  override val id: AgentId,
  val simo: SimO
) extends Proxy with PersonInterface {
  override def sendRegards(msg: String): Unit = {
    val sentMessage = PersonSendRegards(simo.id, id, msg)
    simo.outputMessages = sentMessage :: simo.outputMessages
    //sentMessage.session
  }
}

class Person(
  val shared: Simulation,
  val active: Boolean,
  var happiness : Int = 0, // pursuit of it
  var log : List[String] = List(),
  override val id: AgentId
) extends SimO(shared) with PersonInterface {

  var contact: PersonProxy = _

  def mycopy(_shared: Simulation,
             _substitution: collection.mutable.Map[SimO, SimO]) = {
    val p = new Person(_shared, active, happiness, log, id);
    copy_state_to(p);
    p
  }

  def messages = List("Lanisters", "Starks", "Baratheons", "Bros", "Gamers", "Ladies", "Elephants", "Sea horses",
  "Unicorns", "Narwhals", "Saints", "Unil students", "Donkeys", "Ze Germans", "Frogs")
  def names = List("Poirot", "Sbeve", "The Milk Man", "Star Lord", "Donkey Kong", "Super Mario", "Morgan Freeman",
  "Prince Charles", "Willian", "Herr Gustavo", "The One Who Knocks", "Mitar")


  protected def algo = __forever(
    __interact(() => sharedProxy.getPerson(), this),
    //__do{
    //  shared.getPerson(this.id)
    //},
    //__dowhile (
    //  __wait(1)
    //)(!(inputMessages exists {case a: PersonInfo => true case _ => false})),
    __do {
      for (msg <- inputMessages) if (msg.isInstanceOf[PersonSendRegards]) sendRegards(msg.asInstanceOf[PersonSendRegards].msg)
      contact = new PersonProxy((inputMessages filter {case a: PersonInfo => true case _ => false}).head.asInstanceOf[PersonInfo].personId, this)
      contact.sendRegards(messages(GLOBAL.rnd.nextInt(messages.length - 1)) + " send their regards by " + names(id.toInt))
      contact = null
      inputMessages = List()
    },
    __wait(1)
  );

  override def stat {
    print("(Person@" + happiness + " "  + ")  ");
  }

  override def sendRegards(msg: String): Unit = println(msg)
}


