@main def p13(): Unit =
  val test = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
  println(lengthEncode2(test))



def lengthEncode2[A](ls: List[A]): List[(Int, A)] =
  if ls.isEmpty then Nil
  else
    val (packed, next) = ls.span(_ == ls.head)
    (packed.length, packed.head) :: lengthEncode(next)


//def lengthEncode[A](ls: List[A]): List[(Int, A)] =
//  if ls.isEmpty then Nil
//  else
//    val (packed, next) = ls.span(_ == ls.head)
//    if  next == Nil then List(packed).map(t => (t.length, t.head))
//    else
//      val encoded = List(packed).map(t => (t.length, t.head))
//      encoded ::: lengthEncode(next)