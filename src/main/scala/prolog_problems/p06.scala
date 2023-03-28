@main def po6(): Unit =
  val test = List(1,2,3,3,2,1)
  println(palin(test))

  println(palinRecur(test))

def palin[A](ls: List[A]): Boolean =
  ls.reverse == ls

def revRecurw[A](ls: List[A]): List[A] = ls match
  case cons :: Nil => List(cons)
  case cons :: tail =>
    revRecur(tail) ::: List(cons)
  case _ => throw new NoSuchElementException()

def palinRecur[A](ls: List[A]): Boolean =
  val reverse = revRecurw(ls)
  reverse == ls