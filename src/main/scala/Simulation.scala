package Simulation
import java.util.UUID

import Owner._
import Securities._

trait SimulationInterface {
  def getPerson(): Any
}

class SimulationProxy(simo: SimO) extends SimulationInterface {
  override def getPerson(): UUID = {
    val sentMessage = PersonInfoRequest(simo.id, ENVIRONMENT_ID)
    simo.outputMessages = sentMessage :: simo.outputMessages
    sentMessage.session
  }
}


class Simulation extends SimulationInterface {

  var timer = 0;

  var arbeitsmarkt = collection.mutable.Stack[SimO](); // all Persons

  /** TODO: We should have a registry of sims here, which can be looked up by
      id. This eliminates the need for substitution when copying a simulation,
      which is a mess.
  */
  var sims = List[SimO]()
  var simsWithProxies : List[(AgentId, SimO)] = List[(AgentId, SimO)]()
  var myMessages: List[Message] = List[Message]()
  /** This is not a constructor since we first need to create the Simulation
      to hand it over to the sims, and then hand
      the sims to the Simulation (via init).

      init() accepts the list of sims `_sims`,
      enters persons into the labor market,
      and output the status of each sim.
  */
  def init(_sims: List[SimO]) {
    assert(timer == 0);
    println("INIT Simulation " + this);
    sims = _sims;
    for(s <- sims) if(s.isInstanceOf[Person]) arbeitsmarkt.push(s);
    for (s <- sims) {
      val id = Generator.getNextAgentId
      simsWithProxies = (id, s) :: simsWithProxies
    }
    if(! GLOBAL.silent) {
      for(s <- sims) { s.stat; }
      println; println;
    }

    println("INIT Simulation complete " + this);
  }

  override def getPerson(): AgentId = {
    GLOBAL.rnd.nextInt(simsWithProxies.length).toLong
    //myMessages = new PersonInfo(inputMessage, id) :: myMessages
  }

  /** TODO: Object ids (owners) in logs don't get substituted yet.
      This will become necessary when we want to compute supply by _other_
      sellers.
  */
  def mycopy() = {
    val s2 = new Simulation;
    val old2new = collection.mutable.Map[SimO, SimO]();

    // this separation would not be needed if we had a central map from sim ids
    // to sims.
    for(s <- sims) {
      if(s.isInstanceOf[Person]) {
        val cp = s.mycopy(s2, old2new).asInstanceOf[SimO];
        old2new += (s -> cp);
      }
    }

    s2.sims = sims.map((s: SimO) => {
      old2new.getOrElse(s, {
        val cp = s.mycopy(s2, old2new).asInstanceOf[SimO];
        old2new += (s -> cp);
        cp
      })
    });

    s2.arbeitsmarkt =
      arbeitsmarkt.map((x: SimO) => old2new(x.asInstanceOf[SimO]));

    s2.timer = timer;

    (s2, old2new)
  }

  def handleMessage(msg: Message) = msg match {
    case pir: PersonInfoRequest => myMessages = new PersonInfo(pir, getPerson()) :: myMessages
  }

  /** run the simulation. Must init() first! */
  def run_until(until: Int) {
    println("RESUME Simulation " + this);
    while(timer <= until)
    {
      if(! GLOBAL.silent) println("timer = " + timer);
      sims.map(s => s.run_until(timer));
      val agentMessages = sims.flatMap {case s: Person => s.outputMessages} groupBy (msg => msg.receiverId)
      //handle all of my messages first
      val envMessages = agentMessages.getOrElse(ENVIRONMENT_ID, List())
      envMessages foreach (m => handleMessage(m))
      //after that union allmessages with myMessages so I can have new messages
      val allMessages = sims.flatMap {case s: Person => s.outputMessages} union myMessages groupBy (msg => msg.receiverId)

      sims.foreach {case p: Person => {
        p.inputMessages = allMessages.getOrElse(p.id, List()) ++ p.inputMessages
        p.outputMessages = List()
      } }
      myMessages = List()
      if (! GLOBAL.silent) {
        for(s <- sims) s.stat;
        println(); println();
      }
      timer += 1;
    }
    println("STOP Simulation " + this);
  }
  def run(steps: Int) {
    run_until(timer + steps - 1);
  }

  /** To be used to start a nested simulation. Callable from the sims.

      Returns a mapping from the sims of the old simulation to those of the
      new.
  */
  def run_sim(it: Int) : collection.mutable.Map[SimO, SimO] = {
    val (new_sim, old2new) = this.mycopy;

    new_sim.run(it);
    old2new
  }
}



