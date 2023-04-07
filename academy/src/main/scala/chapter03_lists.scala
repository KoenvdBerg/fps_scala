
// 3.1
// case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
// 3
// this is the one because it's earlier in the cases then the next one, which would also work.

object L {
  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x,xs) => x + sum(xs)
    }
    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(d, ds) => d * product(ds)
    }
    def apply[A](as: A*): List[A] = {
      if as.isEmpty then Nil
      else Cons(as.head, apply(as.tail: _*))
    }
  }

  // 3.2
  def tail[A](as: List[A]): List[A] = as match
    case Cons(_, as) => as
    case _ => Nil

  // 3.3
  def setHead[A](as: List[A], r: A): List[A] = as match
    case Cons(_, as) => Cons(r, as)
    case _ => Nil

  // 3.4
  def drop[A](l: List[A], n: Int): List[A] = (l, n) match
    case (l, 0) => l
    case (Cons(_, as), n) => drop(as, n-1)
    case (Nil, _) => Nil

  // 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match
    case Cons(a, as) =>
      if f(a) then dropWhile(as, f) else Cons(a, as)
    case _ => Nil

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))

  // 3.6
  def init[A](l: List[A]): List[A] =
    l match
      case Cons(_, Nil) => Nil
      case Cons(a, as) => Cons(a, init(as))
      case _ => Nil

  def drowWhile2[A](as: List[A])(f: A => Boolean): List[A] =
    as match
      case Cons(h,t) if f(h) => drowWhile2(t)(f)
      case _ => as

  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))

//  def sum2(ns: List[Int]) =
//    foldRight(ns, 0)((x,y) => x + y)
//
//  def product2(ns: List[Double]) =
//    foldRight(ns, 1.0)(_ * _)

  // 3.7
  // No it's not possible because foldright is not tail-recursive and thus no sudden stop can be accomplished without
  // first having to complete the stack trace

  // 3.8
  // scala> L.foldRight(L.List(1,2,3), L.Nil: L.List[Int])(L.Cons(_,_))
  // val res5: L.List[Int] = Cons(1,Cons(2,Cons(3,Nil)))
  //
  // What happens is that the Nil, just like 0 for sum or 1 for product, acts as the starting building block for
  // "folding" the list using the function f. In this case that function f constructs new Cons() for each found value,
  // and thus this call returns the same as the input.

  // 3.9
  //  L.length(L.List(1,2,3))
  // foldRight(Cons(1, Cons(2, Cons(3, Nil))), 0)((_, y) => 1 + y)
  // 1 + foldRight(Cons(2, Cons(3, Nil))), 0)((_, y) => 1 + y)
  // 1 + 1 + foldRight(Cons(3, Nil))), 0)((_, y) => 1 + y)
  // 1 + 1 + 1 + 0
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, y) => 1 + y)

  //  scala > val x = L.List("foo", "bar", "bac", "baz", "foobar", "joost")
  //  val x: L.List[String] = Cons(foo, Cons(bar, Cons(bac, Cons(baz, Cons(foobar, Cons(joost, Nil))))))
  //
  //  scala > L.length(x)
  //  val res1: Int = 6

  // 3.10
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    case Nil => z

  // 3.11
  def sum2(ns: List[Int]): Int =
    foldLeft(ns, 0)((x,y) => x + y)

  def product2(ns: List[Int]): Int =
    foldLeft(ns, 1)((x, y) => x * y)

  def length2[A](as: List[A]): Int =
    foldLeft(as, 0)((x, _) => x + 1)

  // 3.12
  // foldLeft(Cons(1, Cons(2, Cons(3, Nil))), 0)((_, y) => 1 + y)
  // foldLeft(Cons(2, Cons(3, Nil))), 0 + 1)((_, y) => 1 + y)
  // foldLeft(Cons(3, Nil))), 0 + 1 + 1)((_, y) => 1 + y)
  // foldLeft(0 + 1 + 1 + 1)
  // 3

  // foldLeft(Cons(1, Cons(2, Cons(3, Nil))), Nil)((x, y) => Cons(y, x))
  // foldLeft(Cons(1, Cons(2, Cons(3, Nil))), Cons(1, Nil))((x, y) => Cons(y, x))
  def reverse[A](as: List[A]): List[A] =
    foldLeft[A, List[A]](as, Nil)((y, x) =>
      Cons(x, y)
    )

  // 3.13
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    val asr = reverse(as)
    foldLeft(asr, z)((b, a) => f(a, b))


  // 3.14
  // foldLeft(Cons(1, Cons(2, Nil)
  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft[A, List[A]](a2, a1)((x, y) => Cons(y, x)
    )

  // 3.15
  def concatenate[A](lol: List[List[A]]): List[A] =
    reverse(foldLeft[List[A], List[A]](lol, Nil)(append2))

  def concatenate2[A](lol: List[List[A]]): List[A] =
    foldRight[List[A], List[A]](lol, Nil)(append2)


  // 3.16
  def addOneList(is: List[Int]): List[Int] = is match
    case Cons(x, xs) => Cons(x + 1, addOneList(xs))
    case Nil => Nil

  def addOneList2(is: List[Int]): List[Int] =
    foldRight[Int, List[Int]](is, Nil)((h, acc) => Cons(h+1, acc))

  // 3.17
  def doubleToString(ds: List[Double]): List[String] = ds match
    case Cons(x, xs) => Cons(x.toString, doubleToString(xs))
    case _ => Nil

  def doubleToString2(ds: List[Double]): List[String] =
    foldRight[Double, List[String]](ds, Nil)((h, t) => Cons(h.toString, t))

  // 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] = as match
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
    case _ => Nil

  def map2[A, B](as: List[A])(f: A => B): List[B] =
    foldRight[A, List[B]](as, Nil)((h, t) => Cons(f(h), t))

  // 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match
    case Cons(x, xs) if f(x) => Cons(x, filter(xs)(f))
    case Cons(x, xs) if !f(x) => filter(xs)(f)
    case _ => Nil

  def filter3[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight[A, List[A]](as, Nil)((h, t) => if f(h) then Cons(h, t) else t)

  // 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    def go(ass: List[A])(fs: A => List[B]): List[List[B]] =
      ass match
        case Cons(x, xs) => Cons(f(x), go(xs)(fs))
        case _ => Nil

    concatenate(go(as)(f))

  def flatMap2[A, B](as: List[A])(f: A => List[B]): List[B] =
    concatenate(map(as)(f))

  // 3.21
  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(x => if f(x) then List(x) else Nil)

  // 3.22
  def addLists(a: List[Int], b: List[Int]): List[Int] =
    if length2(a) != length2(b) then Nil
    else
      (a, b) match
        case (Cons(x, xs), Cons(y, ys)) => Cons(x+y, addLists(xs, ys))
        case (_, _) => Nil

  // 3.23
  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] =
    if length2(a) != length2(b) then Nil
    else
      (a, b) match
        case (Cons(x, xs), Cons(y, ys)) => Cons(f(x,y), zipWith(xs, ys)(f))
        case (_, _) => Nil

  // 3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    def go[A](supp: List[A], subb: List[A]): Boolean = subb match
      case Cons(x, xs) =>
        val dropped = drowWhile2(supp)(y => x != y)
        if length2(supp) - length2(dropped) == 1 then
          dropped match
            case Cons(_, _) => go(dropped, xs)
            case Nil => false
        else false
      case _ => true

    sub match
      case Nil => true
      case Cons(x, Nil) =>
        val dropped = drowWhile2(sup)(u => u != x)
        if length2(dropped) > 0 then true else false
      case Cons(x, xs) => go(drowWhile2(sup)(y => x != y), xs)
  }


@main def runlist(): Unit = {
  val x = L.List(1,2,3,4)
  val res = L.foldLeft(x, 1)((a, b) => a + a*b)
  println(res)

  println(L.length(x))

  println(L.reverse(x))

  println("######")
  println(L.foldRight(x, 0)((x,y) => x + y))

  println("#### 3.14 ####")
  println(L.append(x, L.List(99,100)))

  println("#### 3.15 ####")
  println(L.concatenate(L.List(x, L.List(6,7,8,9), L.List(943409,349034,3490))))
  println(L.concatenate2(L.List(x, L.List(6,7,8,9), L.List(943409,349034,3490))))

  println("#### 3.16 ####")
  println(L.addOneList(x))
  println(L.addOneList2(x))

  println("#### 3.17 ####")
  println(L.doubleToString(L.List(1.1,2.2,3.3)))
  println(L.doubleToString2(L.List(1.1,2.2,3.3)))

  println("#### 3.18 ####")
  println(L.map(x)(i => i*10))
  println(L.map(L.List("foo", "bar", "baz"))(i => s"Hello my friend named: ${i}"))
  println(L.map2(x)(i => i*10))
  println(L.map2(L.List("foo", "bar", "baz"))(i => s"Hello my friend named: ${i}"))

  println("#### 3.19 ####")
  println(L.filter(L.List(1,2,3,4,5,6,7,8,9,10))(i => i % 2 == 0))
  println(L.filter3(L.List(1,2,3,4,5,6,7,8,9,10))(i => i % 2 == 0))

  println("#### 3.20 ####")
  val bob = L.flatMap(L.List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))(i => L.List(i,i))
  println(bob)
  val bob2 = L.flatMap2(L.List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))(i => L.List(i,i))
  println(bob2)

  println("#### 3.21 ####")
  println(L.filter2(L.List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))(i => i % 2 == 0))

  println("#### 3.22 ####")
  println(L.addLists(L.List(1,2,3), L.List(4,5,6)))

  println("#### 3.23 ####")
  println(L.zipWith(L.List(1, 2, 3), L.List(4, 5, 6))((i, j) => i + j))
  println(L.zipWith(L.List("foo", "bar", "baz"), L.List("koen", "joost", "marco"))((i, j) => s"${i}_${j}"))

  println("#### 3.24 ####")
  println(L.hasSubsequence(L.List(1,2,3,4), L.List(3,2)))
  println(L.hasSubsequence(L.List(1,2,3,4), L.List(1,4)))
  println(L.hasSubsequence(L.List(1,2,3,4), L.List(100)))
  println(L.hasSubsequence(L.List(1,2,3,4), L.List(1,2)))
  println(L.hasSubsequence(L.List(1,2,3,4), L.List(2,3,4)))
  println(L.hasSubsequence(L.List(1,2,3,4), L.List(2)))
  println(L.hasSubsequence(L.List(1,2,3,4), L.List(L.Nil)))

}




