@main def p16(): Unit =

  val test = List('a','b','c','d','e','f','g','h','i','j','k')
  println(dropN(3, test))

def dropN[A](N: Int, ls: List[A]): List[A] =
  if ls.isEmpty then Nil
  else
    val (taken, tail) = ls.splitAt(N)
    taken.take(N-1) ::: dropN(N, tail)
