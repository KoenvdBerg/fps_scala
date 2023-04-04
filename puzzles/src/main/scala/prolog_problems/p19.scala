@main def p19(): Unit =
  val test = List('a','b','c','d','e','f','g','h','i','j','k')
  println(rotate(3, test))
  println(rotate(8, test))

  println(P19.rotate(-100, test))
  println(rotate2(-100, test))


def rotate[A](N: Int, ls: List[A]): List[A] =

  def rotateHelper[A](N: Int, ls: List[A]): List[A] =
    (N, ls) match
      case (0, ms: List[A]) => ls
      case (x, cons :: tail) if x > 0 =>
        rotateHelper(x-1, tail ::: List(cons))
      case (x, cons :: tail) if x < 0 => rotateHelper(x+1, List(tail.last) ::: List(cons) ::: tail.take(tail.length-1))
      case _ => throw new NoSuchElementException()
  rotateHelper(N, ls)

def rotate2[A](N: Int, ls: List[A]): List[A] =
  val nbound = N % ls.length  // skipping the full rotation rounds
  if nbound < 0 then rotate2(nbound + ls.length, ls)
  else ls.drop(nbound) ::: ls.take(nbound)


object P19 {
  def rotate[A](n: Int, ls: List[A]): List[A] = {
    val nBounded = if (ls.isEmpty) 0 else n % ls.length
    if (nBounded < 0) rotate(nBounded + ls.length, ls)
    else (ls drop nBounded) ::: (ls take nBounded)
  }
}