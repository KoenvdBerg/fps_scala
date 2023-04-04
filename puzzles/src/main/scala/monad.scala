// MONADS:

class Lazy[+A](value: => A) {
  private lazy val internal: A = value
  def flatMap[B](f: (=> A) => Lazy[B]): Lazy[B] = f(internal)
  def map[B](f: A => B): Lazy[B] = flatMap(x => Lazy(f(x)))
  def get: A = internal
}

object Lazy {
  def apply[A](value: => A): Lazy[A] = new Lazy(value)
}

val lazyInt: Lazy[Int] = Lazy {
  println("The response to everything is 42")
  42
}

def arc(m: Int, n: Int): Int = {
  if m == 0 then 1
  else if n == 0 then arc(m-1, n)
  else {
    arc(m-1, arc(0, n - 1))
  }
}
