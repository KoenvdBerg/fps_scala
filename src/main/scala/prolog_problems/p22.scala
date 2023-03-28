@main def p22(): Unit =
  println(range(4, 9))
  println(rangeCur(4,9))

def range(start: Int, end: Int): List[Int] =
  List.range(start, end+1)

def rangeCur(start: Int, end: Int): List[Int] =
  def helperRange(start: Int, end: Int, result: List[Int]): List[Int] =
    if start == end+1 then result
    else
      helperRange(start + 1, end, result ::: List(start))
  helperRange(start, end, Nil)