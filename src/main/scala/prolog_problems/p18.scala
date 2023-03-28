@main def p18(): Unit =

  val test = List('a','b','c','d','e','f','g','h','i','j','k')
  println(slice(3,7, test))

def slice[A](start: Int, end: Int, ls: List[A]): List[A] =
  ls.slice(start, end)