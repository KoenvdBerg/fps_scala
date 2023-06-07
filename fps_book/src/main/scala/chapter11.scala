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


@main def run_chapter11: Unit =
  println("DKLSJF")