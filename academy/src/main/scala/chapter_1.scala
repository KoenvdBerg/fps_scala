
object test {
  def abs(n: Int): Int = {
    if (n < 0) then -n
    else n
  }

  def formatResult(name: String, x: Int, f: Int => Int): String = {
    val msg = s"The $name of ${x} is ${f(x)}"
    msg
  }

  def main(args: Array[String]): Unit = {
    println(formatResult("abs", -42, abs))
  }

  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int = {
      if n <= 0 then acc
      else go(n-1, acc*n)
    }
    go(n, 1)
  }

  // 2.1
  def fibbonaci(n: Int): Int = {
    def go(a: Int, b: Int, n: Int): Int = {
      if n == 0 then a
      else
        go(b, a+b, n-1)
    }
    go(0, 1, n)
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    def loop(n: Int): Int = {
      if n >= as.length then -1
      else if p(as(n)) then n
      else loop(n + 1)
    }
    loop(0)
  }

  // 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
      if n+1 >= as.length then true
      else if !ordered(as(n), as(n + 1)) then false
      else
        loop(n+1)
    }
    loop(0)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
    (b: B) => f(a,b)
  }

  // 2.3
  def curry[A, B, C](f: (A, B) => C): A => B => C = {
    (a: A) => (b: B) => f(a,b)
  }

  // 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  // 2.5
    def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }
  //val f = compose((y: Int) => y * 2, (x: Int) => x + 1)
}


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
  def init1[A](l: List[A]): List[A] =
    def go(las: List[A], acc: List[A]): List[A] =
      las match
        case Nil => acc
        case Cons(a, as) => go(as, Cons(a, as))
    go(l, Nil)

  def init[A](l: List[A]): List[A] =
    l match
      case Cons(_, Nil) => Nil
      case Cons(a, as) => Cons(a, init(as))
      case _ => Nil

  def drowWhile2[A](as: List[A])(f: A => Boolean): List[A] =
    as match
      case Cons(h,t) if f(h) => drowWhile2(t)(f)
      case _ => as

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

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
}

// 3.1
// case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
// 3
// this is the one because it's earlier in the cases then the next one, which would also work.

