@main def po2(): Unit =
  val test = List(1,2,3,4,5,6,7)
  val res = semiLastRecursive(test)
  print(res)

def semiLastRecursive[A](ls: List[A]): A = ls match
  case h :: _ :: Nil => h
  case _ :: tail =>
    println(tail)
    semiLastRecursive(tail)
  case _ => throw new NoSuchElementException()
