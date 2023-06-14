import scala.io.StdIn.readLine
import chapter11.Monad

object chapter13: 
  
  type IO[A] = () => A
  
  extension[A](io: IO[A])
    def map[AA >: A, B](f: A => B): IO[B] = IO.map(io)(f)
    def flatMap[AA >: A, B](f: A => IO[B]): IO[B] = IO.flatMap(io)(f)
    def run: A = IO.run(io)
    def **[AA >: A, B](iob: IO[B]): IO[(A, B)] = IO.product(io, iob)
  
  object IO extends Monad[IO]:
    def run[A](ioa: IO[A]): A = ioa()
    override def unit[A](a: => A): IO[A] = () => a
    override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = () => run(f(run(fa)))
    
    def PrintLine(msg: String): IO[Unit] = unit(println(msg))
    def ReadLine: IO[String] = unit(readLine)

  
@main def run_chapter13: Unit =
  import chapter13.*
  
  val x: IO[Unit] = IO.PrintLine("My name is Koen")
  
  def converter: IO[Unit] = for {
    _ <- IO.PrintLine("what is your name?")
    n <- IO.ReadLine
    _ <- IO.PrintLine(s"your name is: $n")
  } yield ()
  
  val nextC: IO[Unit] = converter.flatMap(a => IO.PrintLine("HAHAHAH"))
  nextC.run
  
  

  val echo: IO[Unit] = IO.ReadLine.flatMap(IO.PrintLine)
  val readInt: IO[Int] = IO.ReadLine.map(_.toInt)
  val readInts: IO[(Int, Int)] = readInt ** readInt
  
  val multiple: IO[List[String]] = IO.replicateM(3, IO.ReadLine)

  

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