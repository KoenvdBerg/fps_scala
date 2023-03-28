@main def po4(): Unit =
  val test = List(1,2,3,4,5,6,7)
  println(length(test))

  println(lengthRecur(test))

def length[A](ls: List[A]): Int =
  ls.length

def lengthRecur[A](ls: List[A], len: Int = 1): Int = ls match
  case _ :: Nil => len
  case _ :: tail =>
    println(tail)
    lengthRecur(tail, len+1)
  case _ => throw new NoSuchElementException()