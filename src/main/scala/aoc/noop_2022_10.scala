@main def noop(): Unit =
  println("lksdjf")


def interpretProgram(s: String, c: Int, x: Int): List[(Int, Int)] =
  if s.contains("noop") then List((c+1, x+0))
  else
    val value = s.split(" ").last.toInt
    List((c+1, x), (c+2, x+value))

def readAllProgram(s: List[String]): List[(Int, Int)] =
  def loop(s: List[String], acc: List[(Int, Int)], c: Int, x: Int): List[(Int, Int)] = s match
    case Nil => acc
    case h :: tail =>
      val res = interpretProgram(h, c, x)
      loop(tail, acc ::: res, res.last.head, res.last.last)

  loop(s, Nil, 1, 1)

def score(r: List[(Int, Int)]): Int =
  val interesting = List(20, 60, 100, 140, 180, 220)
  interesting.map(i => r(i-2)).map(x => x(0)*x(1)).sum