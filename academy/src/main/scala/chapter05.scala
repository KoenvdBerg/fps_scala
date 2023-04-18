import chapter05.Stream.{empty, unfold}

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

    // 5.13
    def map2[B](f: A => B): Stream[B] =
      Stream.unfold(this) {
        case Cons(h, t) => Some((f(h()), t()))
        case Empty => None
      }

    def take2(n: Int): Stream[A] =
      Stream.unfold((this, n)){
        case (Cons(h, _), 1) => Some(h(), (Stream.empty, 0))
        case (Cons(h, t), n) if n > 1 => Some(h(), (t(), n-1))
        case _ => None
      }

    def takeWhile2(p: A => Boolean): Stream[A] =
      Stream.unfold(this){
        case Cons(h, t) if p(h()) => Some(h(), t())
        case _ => None
      }

    def zipWith2[B, C](b: Stream[B])(f: (A, B) => C): Stream[C] =
      Stream.unfold((this, b)) {
        case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
        case _ => None
      }

    def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
      Stream.unfold((this, s2)){
        case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
        case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
        case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
        case (Empty, Empty) => Some((None, None), (Empty, Empty))
        case _ => None
      }

    // 5.14
    def startsWith[A](s: Stream[A]): Boolean =
      this.zipAll(s).takeWhile {
        case (Some(a), Some(b)) => a == b
        case (Some(_), None) => false
        case (None, Some(_)) => false
        case (None, None) => false
      }.forAll(p => p._1.isDefined  && p._2.isDefined)


    def startsWith2[A](s: Stream[A]): Boolean =
      this.zipAll(s).takeWhile(p => p._2.isDefined).forAll((a, b) => a == b)

    // 5.15
    def tails: Stream[Stream[A]] =
      unfold(this){
        case Cons(h, t) => Some(Stream.cons(h(), t()), Stream.cons(h(), t()).drop(1))
        case Empty => None
      }

    def hasSubsequence[A](s: Stream[A]): Boolean =
      this.tails.exists(p => p.startsWith2(s))


    // 5.16
    //    def foldRight[B](z: => B)(f: (A, => B) => B): B =
    //      this match
    //        case Cons(h, t) => f(h(), t().foldRight(z)(f))
    //        case _ => z
    def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
      unfold(this) {
        case Cons(h, t) => Some(Stream.cons(h(), t()).foldRight(z)(f), Stream.cons(h(), t()).drop(1))
        case Empty => None
      }


    def scanRight2[B](z: => B)(f: (A, => B) => B): Stream[B] =
      this.tails.map(p => p.foldRight(z)(f))





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

    def constantCorrect[A](a: A): Stream[A] =
      lazy val c: Stream[A] = Stream.cons(a, c)
      c

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
      f(z) match
        case None => Stream.empty
        case Some((a: A, s: S)) => Stream.cons(a, unfold(s)(f))

    // 5.12
    def fibs3: Stream[Int] =
      unfold((0, 1))(f => Some(f._1, (f._2, f._1 + f._2)))

    def from3: Stream[Int] =
      unfold(0)(f => Some(f, f+1))

    def from3Correct(n: Int): Stream[Int] =
      unfold(n)(f => Some(f, f +1))

    def constant3[A](n: A): Stream[A] =
      unfold(n)(f => Some(f, f))

    def ones3: Stream[Int] =
       unfold(1)(f => Some(f, f))
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
//  val r = chapter05.Stream.unfold(0)(f => Some((None, f+100)))
  val r = chapter05.Stream.unfold(0)(f => Some(f, f+1))
  println(r.take(10).toList)

  println("### 5.12 ###")
  println(chapter05.Stream.fibs3.take(10).toList)
  println(chapter05.Stream.from3.take(10).toList)
  println(chapter05.Stream.constant3(9).take(10).toList)
  println(chapter05.Stream.ones3.take(10).toList)

  println("### 5.13  ###")
  println(t.map2(f => f * 100).take(10).toList)
  println(t.take2(n=3).toList)
  println(t.takeWhile2(p => p < 6).toList)
  val f1 = chapter05.Stream.fibs3
  val f2 = chapter05.Stream.from3
  println(f1.zipWith2(f2)((a, b) => a + b).take2(10).toList)
  println(f1.zipAll(f2).take2(10).toList)

  println("### 5.14 ###")
  val sw1 = chapter05.Stream(1,2,3,4,5)
  val sw2 = chapter05.Stream(1,2,3)
  println(sw1.startsWith2(sw2))
  println(sw2.startsWith2(sw1))

  println("### 5.15 ###")
  println(sw2.tails.toList.map(_.toList))

  println(sw1.hasSubsequence(chapter05.Stream(2,3,4)))

  println("### 5.16 ###")
  println(chapter05.Stream(1,2,3).scanRight(0)((a, b) => a + b).toList)
  println(chapter05.Stream(1,2,3).scanRight2(0)((a, b) => a + b).toList)
