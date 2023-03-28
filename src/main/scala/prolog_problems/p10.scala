@main def p10(): Unit =

  //https://alvinalexander.com/scala/how-to-split-sequences-subsets-groupby-partition-scala-cookbook/

  val test = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
  val intermediate = packz(test)
  println(lengthEncode(intermediate))

def packz[A](ls: List[A]): List[List[A]] =
  if ls.isEmpty then List(List())
  else
    val (packed, next) = ls.span(_ == ls.head)
    if next == Nil then List(packed)
    else packed :: packz(next)

def lengthEncode[A](ls: List[List[A]]): List[(Int, A)] =
  ls.map(lm => (lm.length, lm.head))