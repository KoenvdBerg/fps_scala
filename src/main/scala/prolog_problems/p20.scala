@main def p20(): Unit =
  val test = List('a', 'b', 'c', 'd')
  println(remove(1, test))
  println(remove2(1, test))

def remove[A](pos: Int, ls: List[A]): (List[A], A) =
  val lsWithoutPos = ls.slice(0, pos) ::: ls.slice(pos+1, ls.length)
  (lsWithoutPos, ls(pos))


def remove2[A](pos: Int, ls: List[A]): (List[A], A) = ls.splitAt(pos) match
  case (cons, e :: tail) => (cons ::: tail, e)