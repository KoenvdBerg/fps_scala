@main def p21(): Unit =
  val test = List('a', 'b', 'c', 'd')
  println(insertAt('n', 2, test))

def insertAt[A](item: A, pos: Int, ls: List[A]): List[A] =
  ls.splitAt(pos) match
    case (cons, tail) => cons ::: item :: tail