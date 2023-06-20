import scala.io.StdIn.readLine
import chapter11.Monad
import chapter07.Par
import scala.annotation.tailrec
import scala.util.Try

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
    
    def covary[F2[x] >: F[x]]: Free[F2, A] = this
    
    // 13.3
    def run[F2[x] >: F[x]](using F: Monad[F2]): F2[A] = this match
      case Return(a) => F.unit(a)
      case Suspend(r) => r
      case FlatMap(x, f) => x match
        case Return(x)  => f(x).run
        case Suspend(r) => F.flatMap(r)(a => f(a).run)
        case FlatMap(xx, ff) => xx.flatMap(a => ff(a).flatMap(f)).run
      
    // below gives compiler error for stack overflow:
    //final def step: Free[F, A] = this match
    //  case FlatMap(FlatMap(x, f), g) => x.flatMap(a => f(a).flatMap(y => g(y).covary[F])).step
    //  case FlatMap(Return(x), f) => f(x).step
    //  case _ => this
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

object chapter13_C:
  enum Console[A]:
    case ReadLine extends Console[Option[String]]
    case PrintLine(line: String) extends Console[Unit]

    def toPar: Par[A] = this match
      case ReadLine        => Par.lazyUnit(Try(readLine()).toOption)
      case PrintLine(line) => Par.lazyUnit(println(line))

    def toThunk: () => A = this match
      case ReadLine        => () => Try(readLine()).toOption
      case PrintLine(line) => () => println(line)
      
  object Console:
    import chapter13_B.Free
    import chapter13_B.Free.*
    type ConsoleIO[A] = Free[Console, A]
    
    def readLn: ConsoleIO[Option[String]] = Suspend(ReadLine)
    def printLn(line: String): ConsoleIO[Unit] = Suspend(PrintLine(line))
    
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
  
  // testing the Console object
  import chapter13_C.Console
  import chapter13_B.Free
  val f1: Free[Console, Option[String]] = for {
    _  <- Console.printLn("I can only interact with the console")
    ln <- Console.readLn 
  } yield ln