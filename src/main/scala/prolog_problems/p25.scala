@main def p25(): Unit =
  val test = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')
  println(randomPermute(test))

def randomPermute[A](ls: List[A]): List[A] =
  randomSelect3(ls.length, ls)

def randomSelect3[A](N: Int, ls: List[A]): List[A] =
  def helperRandomSelect[A](ls: List[A], result: List[A]): List[A] =
    if result.length == N | ls.isEmpty then result
    else
      val randInt = scala.util.Random.nextInt(ls.length)
      val (remainder, item) = remove5(randInt, ls)
      helperRandomSelect(remainder, item :: result)
  helperRandomSelect(ls, Nil)

def remove5[A](pos: Int, ls: List[A]): (List[A], A) = ls.splitAt(pos) match
  case (cons, e :: tail) => (cons ::: tail, e)