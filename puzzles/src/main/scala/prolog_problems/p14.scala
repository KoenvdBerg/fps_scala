@main def p14(): Unit =
  val test = List('a', 'b', 'c', 'd', 'e')
  println(duplicate(test))



def duplicate[A](ls: List[A]): List[A] =
  ls.flatMap(i => List.fill(2)(i))