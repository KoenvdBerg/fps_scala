@main def main_ack(): Unit =
  println("ksdjfkl")
//  println(ack(4,1).compute)
  println(even((1 to 10001).toList).compute)

//  val x = Pure(10).compute  ---> 10


def even[A](ls: List[A]): Pure[Boolean] = {
  import Pure.*
  if ls.isEmpty then done(true)
  else call(odd(ls.tail))
}
def odd[A](ls: List[A]): Pure[Boolean] = {
  import Pure.*
  if ls.isEmpty then done(false)
  else call(even(ls.tail))
}



def ack(m: Int, n: Int): Pure[Int] =
  import Pure.*
  (m, n) match
    case (0, _) => done(n + 1)
    case (_, 0) => call(ack(m-1, 1))
    case (_, _) =>
      for {
        inner <- call(ack(m, n-1))
        outer <- call(ack(m-1, inner))
      } yield outer

// hint: niet een fucntie schrijven in stack space maar juist een functie die een programma maakt in heapspace die geinterpreteerd wordt.

// encapsuleren van side-effects (bv. Option)


// functie database: Int -----> Int

// Int -------> Exception (404)


// Int -------> Option (Some, None)

// sealed is for writing libraries
sealed trait Pure[A]:
  final def compute: A = this match
    case Done(a) => a
    case Call(t) => t().compute
    case Cont(p, f) => p match
      case Done(a) => f(a).compute
      case Call(t) => t().flatMap(f).compute
      case Cont(pp, ff) => pp.flatMap(a => ff(a).flatMap(f)).compute
  def flatMap[B](f: A => Pure[B]): Pure[B] = this match
    case Done(a) => f(a)
    case c@Call(_) => Cont(c, f)
    case c@Cont(_,_) => Cont(c.p, (a) => c.f(a).flatMap(f))
  def map[B](f: A => B): Pure[B] = flatMap(x => Done(f(x)))

object Pure:
  def done[A](a: A): Pure[A] = Done(a)
  def call[A](p: => Pure[A]): Pure[A] = Call(() => p)
//  def cont[A,B](p: Pure[A], f: A => Pure[B]): Pure[B] = Cont(p, f)

private case class Done[A](a: A) extends Pure[A]

private case class Call[A](t: () => Pure[A]) extends Pure[A]

private case class Cont[A, B](p: Pure[A], f: A => Pure[B]) extends Pure[B]


