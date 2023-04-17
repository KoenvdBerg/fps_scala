object chapter05 {
  def if22[A](cond: Boolean, onTrue: () => A, onFalse: () => A): A =
    if cond then onTrue() else onFalse()

  def if2[A](cond: Boolean, onTrue: => A, onFalse: => A): A =
    if cond then onTrue else onFalse

  def maybeTwice(b: Boolean, i: => Int): Int =
    lazy val j = i
    if b then j+j else 0


  sealed trait Stream[+A]:

    def headOption: Option[A] = this match
      case Empty => None
      case Cons(h, _) => Some(h())

    // 5.1
    def toList: List[A] = this match
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
//      case Cons(h, t) => List(h()) ++ t().toList

    // 5.2
    def take(n: Int): Stream[A] = this match
      case Cons(h, t) if n > 0 => Stream.cons(h(), t().take(n-1))
      case Cons(h, _) if n == 1 => Stream.cons(h(), Empty)
      case _ => Stream.empty

    def drop(n: Int): Stream[A] = this match
      case Cons(_, t) if n > 0 => t().drop(n-1)
      case _ => this

    // 5.3
    def takeWhile1(p: A => Boolean): Stream[A] = this match
      case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
      case _ => Stream.empty

    def exists2(p: A => Boolean): Boolean = this match
      case Cons(h, t) => p(h()) || t().exists(p)
      case _ => false

    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      this match
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z

    def exists(p: A => Boolean): Boolean =
      this.foldRight(false)((a, b) => p(a) || b)

    // 5.4
    def forAll(p: A => Boolean): Boolean =
      foldRight(true)((a, b) => p(a) && b)

    // 5.5
    def takeWhile(p: A => Boolean): Stream[A] =
      this.foldRight(Stream.empty)((a, b) => if p(a) then Stream.cons(a, b) else Stream.empty)

    // 5.6
    def headOption2: Option[A] = this.foldRight(None: Option[A])((a: A, _) => Some(a))

    // 5.7
    def map[B](f: A => B): Stream[B] =
      this.foldRight(Stream.empty)((h, t) => Stream.cons(f(h), t))

    def filter(f: A => Boolean): Stream[A] =
      this.foldRight(Stream.empty[A])((h, t) => if f(h) then Stream.cons(h, t) else t)

    def append[A2 >: A](a2: => Stream[A2]): Stream[A2] =
      this.foldRight(a2)((h, t) => Stream.cons(h, t))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      this.foldRight(Stream.empty[B])((h, t) => f(h).append(t))

    def find(p: A => Boolean): Option[A] =
      this.filter(p).headOption


  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if as.isEmpty then empty else cons(as.head, apply(as.tail: _*))

    // 5.8
    def constant[A](a: A): Stream[A] =
      Stream.cons(a, constant(a))

    // 5.9
    def from(n: Int): Stream[Int] =
      Stream.cons(n, from(n + 1))

    // 5.10
    def fibs: Stream[Int] =
      def fibbonaci(n: Int): Int = {
        def go(a: Int, b: Int, n: Int): Int = {
          if n == 0 then a
          else
            go(b, a + b, n - 1)
        }

        go(0, 1, n)
      }
      Stream.from(0).map(fibbonaci)

    def fibs2: Stream[Int] =
      def fibStream(a: Int = 0, b: Int = 1): Stream[Int] =
        Stream.cons(a, fibStream(b, a + b))
      fibStream()

    // fibs = 0 #:: 1 #:: fibs(a) + fibs(b)

    // 5.11
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
      Stream.cons(z, unfold(f(z)(f)))
  }

}


val x = chapter05.Stream(1,2,3,4,5,6,7,8,9)


@main def C05(): Unit =
  val x = chapter05.if2(false, sys.error("fail"),  43)

  val y = chapter05.maybeTwice(true, {println("Hello there"); 1 + 41})


  println("### 5.7 ###")
  val t = chapter05.Stream(1,2,3,4,5,6,7,8,9)
  println(t.map(_ * 100).toList)
  println(t.filter(_ < 5).toList)
  println(t.append(chapter05.Stream(100,200,300)).toList)
  println(t.flatMap(i => chapter05.Stream(i + i*i)).toList)

  println(t.map(_*100).map(_ % 26).take(2).toList)

  println(t.find(_ == 5))

  println("### 5.8 ###")
  val e = chapter05.Stream.constant(1)
  println(e.take(5).toList)

  println("### 5.9 ###")
  val f = chapter05.Stream.from(2)
  println(f.take(10).toList)

  println("### 5.10 ###")
  println(chapter05.Stream.fibs.take(10).toList)
  println(chapter05.Stream.fibs2.take(10).toList)

  println("### 5.11 ###")
  val r = chapter05.Stream.unfold(0)(f => Some(f + 3))
  println(r.take(10).toList)
