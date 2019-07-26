package Simulation


object MainExample {
  val s = new Simulation;

  val people = for(x <- 0 to 3) yield new Person(s, true, id=x);

  s.init(List(
  ) ++ people.toList);

  def main(argv: Array[String]) {
    if((argv.length != 1) || (argv(0).toInt < 1))
      println("Exactly one integer >0 argument needed!");
    else
      s.run(argv(0).toInt);
  }
}

object TradingExample {
  import Owner._;

  val simu = new Simulation;

  simu.init(List());
  simu.run(4);
/* After 4 steps we have
(BalanceSheet(4000,4000,4000,0,0),ArrayBuffer(getreide -> 0@0))
(BalanceSheet(50,50,50,0,0),ArrayBuffer(getreide -> 0@1000))
(BalanceSheet(0,4050,0,-4050,0),ArrayBuffer(getreide -> 4@1012))
*/
}



