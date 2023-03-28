@main def p15(): Unit =
  val test = List('a', 'b', 'c', 'd', 'e')
  println(duplicate2(test, 8))



def duplicate2[A](ls: List[A], N: Int): List[A] =
  ls.flatMap(i => List.fill(N)(i))