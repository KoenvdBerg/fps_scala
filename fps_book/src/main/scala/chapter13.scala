import scala.io.StdIn.readLine
import chapter11.Monad
import chapter13_A.IO.{FlatMap, Suspend}

import scala.annotation.tailrec

object chapter13_A:
  
  enum IO[A]:
    case Return(a: A)
    case Suspend(resume: () => A)
    case FlatMap[A, B](sub: IO[A], k: A => IO[B]) extends IO[B]
    def map[B](f: A => B): IO[B] = flatMap((a: A) => Return(f(a)))
    def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)
    def run[AA >: A]: A = IO.run(this)
    
  
  object IO extends Monad[IO]:
    override def unit[A](a: => A): IO[A] = Return(a)
    override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa.flatMap(f)
    def PrintLine(s: String): IO[Unit] = Suspend(() => println(s))
    def ReadLine: IO[String] = Suspend(() => readLine)
    @tailrec
    def run[A](io: IO[A]): A = io match
      case Return(a)     => a
      case Suspend(r)    => r()
      case FlatMap(x, f) => x match
        case Return(a)       => run(f(a))
        case Suspend(r)      => run(f(r()))
        case FlatMap(xx, ff) => run(xx.flatMap(a => ff(a).flatMap(f)))
        
        
object chapter13_B: 
  enum Free[+F[_], A]:
    case Return(a: A) extends Free[Nothing, A]
    case Suspend(s: F[A])
    case FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

    def flatMap[F2[x] >: F[x], B](f: A => Free[F2, B]): Free[F2, B] = FlatMap(this, f)
    def map[B](f: A => B): Free[F, B] = flatMap((a: A) => Return(f(a)))

    @tailrec
    final def step: Free[F, A] = this match
      case FlatMap(FlatMap(x, f), g) => x.flatMap(a => f(a).flatMap(g)).step
      case FlatMap(Return(x), f) => f(x).step
      case _ => this


    def run[F2[x] >: F[x]](using F: Monad[F2]): F2[A] = this.step match
      case Return(a)  => F.unit(a)
      case Suspend(r) => r
      case FlatMap(x, f) => x match
        case Suspend(r) => F.flatMap(r)(a => f(a).run)
        case _          => sys.error("Impossible no se puede realizar esta categoria")
      
    
        
    
  object Free:
    // 13.1
    def freeMonad[F[_]]: Monad[[x] =>> Free[F, x]] = new Monad[[x] =>> Free[F, x]]:
      override def unit[A](a: => A): Free[F, A] = Return(a)
      override def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] = fa.flatMap(f)


    // 13.2
    extension [A](fa: Free[Function0, A])
      @tailrec
      def runTrampoline: A = fa match
        case Return(v) => v
        case Suspend(r) => r()
        case FlatMap(x, f) => x match
          case Return(a) => f(a).runTrampoline
          case Suspend(r) => f(r()).runTrampoline
          case FlatMap(xx, ff) => xx.flatMap(a => ff(a).flatMap(f)).runTrampoline
    
@main def run_chapter13: Unit =
  import chapter13_A.*
  
  val x: IO[Unit] = IO.PrintLine("My name is Koen")
  
  def converter: IO[Unit] = for {
    _ <- IO.PrintLine("what is your name?")
    n <- IO.ReadLine
    _ <- IO.PrintLine(s"your name is: $n")
  } yield ()
  
  converter.run
  
  val f: Int => IO[Int] = (x: Int) => chapter13_A.IO.Return(x)
  val g: Int => IO[Int] = List.fill(100000)(f).foldLeft(f) {
    (a, b) => x => chapter13_A.IO.Suspend(() => ()).flatMap { _ => a(x).flatMap(b)}
  }
  
  println(g(42).run)
  
  
//  val nextC: IO[Unit] = converter.flatMap(a => IO.PrintLine("HAHAHAH"))
//  val echo: IO[Unit] = IO.ReadLine.flatMap(IO.PrintLine)
//  val readInt: IO[Int] = IO.ReadLine.map(_.toInt)
//  val readInts: IO[(Int, Int)] = readInt ** readInt
//  val multiple: IO[List[String]] = IO.replicateM(3, IO.ReadLine)
//
//  val f = IO.forever(x)
//  println(f.unsafeRun())
  

//sealed trait IO[A]:
//  private val self = this
//
//  def run: A
//
//  def map[AA >: A, B](f: A => B): IO[B] = new IO[B] {
//    override def run: B = f(self.run)
//  }
//
//  def flatMap[AA >: A, B](f: A => IO[B]): IO[B] = new IO[B] {
//    override def run: B = f(self.run).run
//  }
//
//object IO:
//  def unit[A](a: => A): IO[A] = new IO {
//    override def run: A = a
//  }
//
//  def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa.flatMap(f)
//
//  def ReadLine: IO[String] = IO
// 
// NR 2: 
//case class IO[A](unsafeRun: () => A):
//  def map[B](f: A => B): IO[B] = IO.map(this)(f)
//
//  def flatMap[B](f: A => IO[B]): IO[B] = IO.flatMap(this)(f)
//
//  def **[AA >: A, B](iob: IO[B]): IO[(A, B)] = IO.product(this, iob)
//
//object IO extends Monad[IO]:
//  override def unit[A](a: => A): IO[A] = IO(() => a)
//
//  override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = IO(() => f(fa.unsafeRun()).unsafeRun())
//
//  def PrintLine(msg: String): IO[Unit] = unit(println(msg))
//
//  def ReadLine: IO[String] = unit(readLine)

//sealed trait Free[F[_], A]:
//  def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(this, f)
//
//  def map[B](f: A => B): Free[F, B] = flatMap(f.andThen(Return.apply))
//
//case class Return[F[_], A](a: A) extends Free[F, A]
//
//case class Suspend[F[_], A](s: F[A]) extends Free[F, A]
//
//case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]