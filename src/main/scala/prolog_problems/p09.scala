@main def p09(): Unit =

  //https://alvinalexander.com/scala/how-to-split-sequences-subsets-groupby-partition-scala-cookbook/

  val test = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
  val intermediate = pack(test)
  println(intermediate)

def pack[A](ls: List[A]): List[List[A]] =
  if ls.isEmpty then List(List())
  else
    val (packed, next) = ls.span(_ == ls.head)
    if next == Nil then List(packed)
    else packed :: pack(next)