@main def po8(): Unit =
  val test = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
  println(compress(test))
  println(compress2(test))

//  println(palinRecur(test))

def compress(ls: List[Any]): List[Any] = ls match
  case cons :: Nil => List(cons)
  case a :: b :: tail =>
    println(s"$a, $b, $tail")
    if a == b then compress(List(b) ::: tail) else List(a) ::: compress(List(b) ::: tail)
  case _ => throw new NoSuchElementException()

def compress2[A](ls: List[A]): List[A] = ls match
  case Nil => Nil
  case a :: tail => a :: compress2(tail.dropWhile(_ == a))