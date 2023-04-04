@main def p24(): Unit =
  println(lotto(6, 49))


def lotto(n: Int, set: Int): List[Int] =
  val range = List.range(1, set+1)
  randomSelect2(n, range)

def randomSelect2[A](N: Int, ls: List[A]): List[A] =
  def helperRandomSelect[A](ls: List[A], result: List[A]): List[A] =
    if result.length == N | ls.isEmpty then result
    else
      val randInt = scala.util.Random.nextInt(ls.length)
      val (remainder, item) = remove4(randInt, ls)
      helperRandomSelect(remainder, item :: result)
  helperRandomSelect(ls, Nil)

def remove4[A](pos: Int, ls: List[A]): (List[A], A) = ls.splitAt(pos) match
  case (cons, e :: tail) => (cons ::: tail, e)