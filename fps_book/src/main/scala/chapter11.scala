import testing.Gen
import chapter07.Par
import Combinator.{Parser, P}

object chapter11:

  trait Functor[F[_]]:
    def map[A, B] (fa: F[A])(f: A => B): F[B]
    def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
      (map(fab)(_._1), map(fab)(_._2))

  val listFunctor = new Functor[List]:
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)

  trait Monad[F[_]] extends Functor[F]:
    def unit[A](a: A): F[A]
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
    def map[A, B](fa: F[A])(f: A => B): F[B] =
      flatMap(fa)((a: A) => unit(f(a)))
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      flatMap(fa)((a: A) => map(fb)((b: B) => f(a, b)))

    def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

    // 11.3
    def sequence[A](lma: List[F[A]]): F[List[A]] =
      traverse(lma)(identity)
    def traversePattern[A, B](la: List[A])(f: A => F[B]): F[List[B]] = la match
      case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
      case Nil    => unit(Nil)

    def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
      la.foldRight(unit(List.empty[B]))((a: A, b: F[List[B]]) => map2(f(a), b)(_ :: _))


    // 11.4
    def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
      sequence(List.fill(n)(ma))

    // 11.5

    /**
     * List monad --> it will replicate the list N times like this: replicateM(3, List(1,2)) --> List(List(1,2), List(1,2), List(1,2))
     * Option monad --> replicateM(3, Some(3)) --> List(Some(3), Some(3), Some(3))
     *
     * It behaves by replicating the input monad N times and storing these replications into a list.
     */

    // 11.6
    def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = ms match
        case h :: t => flatMap(f(h))((b: Boolean) =>
          if b then map(filterM(t)(f))(h :: _)
          else filterM(t)(f))
        case Nil    => unit(List.empty[A])

    // 11.7
    def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
      (a: A) => flatMap(f(a))((b: B) => g(b))

    // 11.8
    def flatMapCompose[A, B](fa: F[A])(f: A => F[B]): F[B] =
      val c: Unit => F[B] = compose((_: Unit) => fa, f)
      c(())

    /**
     * 11.10
     *
     * // first identity law from compose to flatMap
     * (1) compose(f, unit)(v) == f(v)
     * (2) a => flatMap(f(a))(unit)(v) == f(v)
     * (3) flatMap(f(v))(unit) == f(v)
     * (4) let f(v) be x then: flatMap(x)(unit) == x
     *
     * // second identity law from compose to flatMap
     * (1) compose(unit, f)(v) == f(v)
     * (2) a => flatMap(unit)(f)(v) == f(v)
     * (3) flatMap(unit(v))(f) == f(v)
     * (4) let v be y then: flatMap(unit(y))(f) == f(y)
     *
     */

    // 11.12
    def join[A](mma: F[F[A]]): F[A] =
      flatMap(mma)((fa: F[A]) => fa)

    // 11.13
    def flatMapJoin[A, B](fa: F[A])(f: A => F[B]): F[B] =
      join(map(fa)(f))


  object Monad:

    val genMonad: Monad[Gen] = new Monad[Gen]:
      override def unit[A](a: A): Gen[A] = Gen.unit(a)
      override def flatMap[A, B](fa: Gen[A])(f: A => Gen[B]): Gen[B] = fa.flatMap(f)

    // 11.1
    val parMonad: Monad[Par] = new Monad[Par]:
      override def unit[A](a: A): Par[A] = Par.unit(a)
      override def flatMap[A, B](fa: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(fa)(f)

    val parserMonad: Monad[Parser] = new Monad[Parser]:
      override def unit[A](a: A): Parser[A] = P.succeed(a)
      override def flatMap[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] = P.flatMap(fa)(f)

    val listMonad: Monad[List] = new Monad[List]:
      override def unit[A](a: A): List[A] = List(a)
      override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)

    val lazylistMonad: Monad[LazyList] = new Monad[LazyList]:
      override def unit[A](a: A): LazyList[A] = LazyList(a)
      override def flatMap[A, B](fa: LazyList[A])(f: A => LazyList[B]): LazyList[B] = fa.flatMap(f)

    val optionMonad: Monad[Option] = new Monad[Option]:
      override def unit[A](a: A): Option[A] = Some(a)
      override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

    def stateMonad[S] = new Monad[[x] =>> State[S, x]]:
      override def unit[A](a: A): State[S, A] = State(s => (a, s))
      override def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] = fa.flatMap(f)
      def getState[A](fa: State[S, A]): State[S, S] = fa.get
      def setState[A](fa: State[S, A], s: => S): State[S, Unit] = fa.set(s)


    // 11.17
    case class Id[A](value: A)
    val idMonad: Monad[Id] = new Monad[Id]:
      override def unit[A](a: A): Id[A] = Id(a)
      override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] =
        f(fa.value)

    // 11.18
    case class Reader[R, A](run: R => A)
    object Reader:
      def unit[R, A](a: A): Reader[R, A] = Reader((_: R) => a)
      def flatMap[R, A, B](ra: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
        Reader(
          (r: R) =>
            val a: A = ra.run(r)
            f(a).run(r)
        )
      def getR[R, A](ra: Reader[R, A]): Reader[R, R] = Reader((r: R) => r)
      def readerMonad[R] = new Monad[[x] =>> Reader[R, x]]:
        override def unit[A](a: A): Reader[R, A] = Reader.unit(a)
        override def flatMap[A, B](fa: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = Reader.flatMap(fa)(f)


@main def run_chapter11: Unit =

  val x = List(1,2,3,4)
  val y = List(5,6,7,8)

  println(chapter11.Monad.listMonad.product(x, y))

  println(chapter11.Monad.listMonad.filterM(x)((i: Int) => if i <= 2 then List(true) else List(false)))

  // 11.11 for Gen monad
  val g: Gen[Int] = Gen.choose(0, 10)
  println(g.flatMap(Gen.unit))
  println(g)
  val f: Int => Gen[Int] = (i: Int) => Gen.choose(i * 2, i * 4)
  println(Gen.unit(3).flatMap(f))
  println(f(3))

  // 11.18
  val s: State[Int, Int] = State((i: Int) => (i*100, i + 2))
  val s2: State[Int, String] = State((i: Int) => (s"$i is lol", i + 20))
  val t1 = chapter11.Monad.stateMonad.replicateM(10, s)
  println(t1.run(4))
  println(chapter11.Monad.stateMonad.map2(s, s2)((a: Int, b: String) => s"$a + $b").run(4))
  println(chapter11.Monad.stateMonad.sequence(List(s)).run(5))

  // 11.19
  println(s.get.flatMap(s.set).run(100))
  println(s.set(100).flatMap(_ => s.get).run(199))

  val F = chapter11.Monad.stateMonad[Int]
  def zipWithIndex[A](as: List[A]): List[(Int, A)] =
    as.foldLeft(F.unit(List.empty[(Int, A)]))((acc, a) => for {
      xs <- acc
      n  <- acc.get
      _  <- acc.set(n + 1)
    } yield (n, a) :: xs).run(0)._1.reverse

  println(zipWithIndex(List("foo", "bar", "baz")))

