@main def p17(): Unit =

  val test = List('a','b','c','d','e','f','g','h','i','j','k')
  println(split(3, test))

def split[A](N: Int, ls: List[A]): (List[A], List[A]) =
  ls.splitAt(N)