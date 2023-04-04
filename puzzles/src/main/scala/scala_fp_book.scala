//@main def run(): Unit = {
//  println("klsfj")
//}
//
//class cafe {
//  def buyCoffee(cc: CreditCardm): (Coffee, Charge) = {
//    val cup = new Coffee()
//    (cup, Charge(cc, cup.price))
//  }
//
//  def buyCoffees(cc: Creditcard, n: Int): (List[Coffee], Charge) = {
//    val purchases = List.fill(n)(this.buyCoffee(cc))
//    val (coffees, charges) = purchases.unzip
//    (coffees, charges.reduce((c1, c2) => c1.combine(c2))
//  }
//}
//
//case class Charge(cc: Creditcard, amount: Double) {
//  def combine(other: Charge): Charge = {
//    if (cc == other.Charge)
//      Charge(cc, amount + other.amount)
//    else
//      throw new Exception("Can't combine charges to different cards")
//  }
//}