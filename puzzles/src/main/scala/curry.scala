def curry[A, B, C](f: (A, B) => C): A => B => C = (a: A) => (b: B) => f(a,b)

def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

sealed trait List1[+A]
case object Nil extends List1[Nothing]
case class Cons[+A](head: A, tail: List1[A]) extends List1[A]

object List1 {

  def tail[A](as: List1[A]): List1[A] = as match {
    case Cons(x, xs) => xs
    case Nil => Nil
  }

  def sum(ints: List1[Int]): Int = {
    def go(ints: List1[Int], acc: Int): Int = ints match {
      case Nil => acc
      case Cons(x, xs) => go(xs, acc+x)
    }
    go(ints, 0)
  }

  def apply[A](as: A*): List1[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}