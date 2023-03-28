@main def p23(): Unit =
  val test = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')
  println(randomSelect(10, test))


def randomSelect[A](N: Int, ls: List[A]): List[A] =
  def helperRandomSelect[A](ls: List[A], result: List[A]): List[A] =
    if result.length == N | ls.isEmpty then result
    else
      val randInt = scala.util.Random.nextInt(ls.length)
      val (remainder, item) = remove3(randInt, ls)
      helperRandomSelect(remainder, item :: result)
  helperRandomSelect(ls, Nil)

def remove3[A](pos: Int, ls: List[A]): (List[A], A) = ls.splitAt(pos) match
  case (cons, e :: tail) => (cons ::: tail, e)