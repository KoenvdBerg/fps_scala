@main def po3(): Unit =
  val test = List(1,2,3,4,5,6,7)
  val res = nth(test, 3)
  println(res)

  println(nthRecur(test, 3))

def nth[A](ls: List[A], n: Int): A =
  ls.take(n+1).last

def nthRecur[A](ls: List[A], n: Int): A = (ls, n) match
  case (x :: _, 0) => x
  case (_ :: tail, _) =>
    println(tail)
    nthRecur(tail, n-1)
  case _ => throw new NoSuchElementException()