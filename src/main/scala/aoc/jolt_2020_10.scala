package p.jolt

@main def jolt(): Unit = {
  val input = """28
                |33
                |18
                |42
                |31
                |14
                |46
                |20
                |48
                |47
                |24
                |23
                |49
                |45
                |19
                |38
                |39
                |11
                |1
                |32
                |25
                |35
                |8
                |17
                |7
                |9
                |4
                |2
                |34
                |10
                |3
                |
                |""".stripMargin

  val inputrefined = input.split("\n").toList.map(_.toInt)

  val x = countJolts(inputrefined, computeDiff)

  val res = (x.count(_ == 3)+1) * (x.count(_ == 1)+1)

}

def countJolts(js: List[Int], f: (Int, Int) => Int): List[Int] = {
  val jss = js.sorted
  def loop(n: Int, acc: List[Int]): List[Int] = {
    if n + 1 == jss.length then acc
    else loop(n+1, acc ::: List(f(jss(n), jss(n+1))))
  }
  loop(0, Nil)

}

def computeDiff(a: Int, b: Int): Int = {
  b - a
}

