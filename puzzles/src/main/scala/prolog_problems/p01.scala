@main def po1(): Unit =
  val test = List(1,2,3,4,5,6,7)
  val res = lastRecursive(test)
  print(res)


def Last[A](x: List[A]): A =
  x.last

def lastRecursive[A](ls: List[A]): A = ls match
  case h :: Nil => h
  case _ :: tail =>
    println(tail)
    lastRecursive(tail)
  case _ => throw new NoSuchElementException()
