@main def po5(): Unit =
  val test = List(1,2,3,4,5,6,7)
  println(rev(test))

  println(revRecur(test))

def rev[A](ls: List[A]): List[A] =
  ls.reverse

def revRecur[A](ls: List[A]): List[A] = ls match
  case cons :: Nil => List(cons)
  case cons :: tail =>
    revRecur(tail) ::: List(cons)
  case _ => throw new NoSuchElementException()