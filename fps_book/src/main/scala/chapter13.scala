import scala.io.StdIn.readLine
import chapter11.Monad

import scala.annotation.tailrec

object chapter13: 
  
  sealed trait IO[A]:
    def map[B](f: A => B): IO[B] = flatMap(f.andThen(Return(_)))
    def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)
    def run[AA >: A]: A = IO.run(this)
    
    
  case class Return[A](a: A) extends IO[A]
  case class Suspend[A](resume: () => A) extends IO[A]
  case class FlatMap[A, B](sub: IO[A], k: A => IO[B]) extends IO[B]
  
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
    
@main def run_chapter13: Unit =
  import chapter13.*
  
  val x: IO[Unit] = IO.PrintLine("My name is Koen")
  
  def converter: IO[Unit] = for {
    _ <- IO.PrintLine("what is your name?")
    n <- IO.ReadLine
    _ <- IO.PrintLine(s"your name is: $n")
  } yield ()
  
  converter.run
  
  
  
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