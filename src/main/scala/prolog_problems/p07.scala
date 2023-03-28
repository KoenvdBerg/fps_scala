@main def po7(): Unit =
  val test = List(List(1, 1), 2, List(3, List(5, 8)))
  println(flattenRecur(test))

//  println(palinRecur(test))

def flattenRecur(ls: List[Any]): List[Any] = ls flatMap {
  case ms: List[_] => flattenRecur(ms)
  case e => List(e)

}

