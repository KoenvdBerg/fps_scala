@main def p11(): Unit =

  //https://alvinalexander.com/scala/how-to-split-sequences-subsets-groupby-partition-scala-cookbook/

  val test = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
  val lol = List(1,1,1,1,8,2,2,2,2,4,4,4,3,3,3,4,4,4,5)
  val intermediate = lengthEncodeq(packq(lol))
  println(modifiedLengthEncode(intermediate))
//  println(modifiedLengthEncode(intermediate))

def packq[A](ls: List[A]): List[List[A]] =
  if ls.isEmpty then List(List())
  else
    val (packed, next) = ls.span(_ == ls.head)
    if next == Nil then List(packed)
    else packed :: packq(next)

def lengthEncodeq[A](ls: List[List[A]]): List[(Int, A)] =
  ls.map(lm => (lm.length, lm.head))

def modifiedLengthEncode[B](ls: List[(Int, B)]): List[Any] =
  ls.map(t => if t(0) == 1 then t(1) else t)