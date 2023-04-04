@main def p12(): Unit =

  //https://alvinalexander.com/scala/how-to-split-sequences-subsets-groupby-partition-scala-cookbook/

  val test = List((4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e'))
  println(encodeFlat(test))
//  println(modifiedLengthEncode(intermediate))


def encodeFlat[A](ls: List[(Int, A)]): List[A] =
  ls.flatMap(t =>
    for
      i <- 0 to t(0)
    yield t(1))