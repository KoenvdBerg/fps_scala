@main def p26(): Unit =
  val test = List('a', 'b', 'c', 'd', 'e', 'f')
//  val combs = combinations(3, test)
//
//  println(combs.length)
//  println(combs)

  println(P26.combinations(3, test))

def combinations[A](n: Int, ls: List[A]): List[List[A]] =
  // 1- slice base
  // 2- compute for base
  // 3- leave out first letter and repeat
  def helper[A](ls: List[A]): List[List[A]] =
    val (base, rest) = ls.splitAt(n - 1)
    if rest.isEmpty then Nil
    else
      val hits = for i <- rest yield base ::: List(i)
      val (pre, _) = remove10(n - (n-1), ls)
      hits ::: helper(pre)

  val result = helper(ls)
  if ls.length < n then Nil
  else
    val (pre, _) = remove10(0, ls)
    result ::: combinations(n, pre)

def remove10[A](pos: Int, ls: List[A]): (List[A], A) = ls.splitAt(pos) match
  case (cons, e :: tail) => (cons ::: tail, e)


object P26 {
  // flatMapSublists is like list.flatMap, but instead of passing each element
  // to the function, it passes successive sublists of L.
  def flatMapSublists[A,B](ls: List[A])(f: (List[A]) => List[B]): List[B] =
    ls match {
      case Nil => Nil
      case sublist@(_ :: tail) => f(sublist) ::: flatMapSublists(tail)(f)
    }

  def combinations[A](n: Int, ls: List[A]): List[List[A]] =
    if (n == 0) List(Nil)
    else flatMapSublists(ls) { sl =>
      combinations(n - 1, sl.tail) map {sl.head :: _}
    }
}