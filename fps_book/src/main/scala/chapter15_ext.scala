//import chapter13_A.IO
import cats.effect.IO
import java.io.{BufferedReader, FileReader}

object chapter15_ext:

  enum Process[F[_], O]: 
    case Await[F[_], O, A](req: F[A], recv: Either[Throwable, A] => Process[F, O]) extends Process[F, O]
    case Emit[F[_], O](head: O, tail: Process[F, O]) extends Process[F, O]
    case Halt(err: Throwable)
    
    def onHalt(f: Throwable => Process[F, O]): Process[F, O] = this match
      case Halt(err)        => Process.Try(f(err))
      case Emit(h, t)       => Emit(h, t.onHalt(f))
      case Await(req, recv) => Await(req, e => recv(e).onHalt(f))

    def flatMap[O2](f: O => Process[F, O2]): Process[F, O2] = this match
      case Halt(err)        => Halt(err)
      case Emit(h, t)       => Process.Try(f(h)) ++ t.flatMap(f)
      case Await(req, recv) => Await(req, e => recv(e).flatMap(f))
      
    def ++(p: => Process[F, O]): Process[F, O] =
      this.onHalt {
        case Process.End => p
        case err         => Halt(err)
      }
    
  object Process: 
    case object End extends Exception
    case object Kill extends Exception
    
    def await[F[_], O, A](req: F[A])(recv: Either[Throwable, A] => Process[F, O]): Process[F, O] = Await(req, recv)
    
    def Try[F[_], O](p: => Process[F, O]): Process[F, O] =
      try p
      catch {case e: Throwable => Halt(e)}

    // TODO: copy in the packages from: https://github.com/fpinscala/fpinscala/blob/first-edition/exercises/src/main/scala/fpinscala/iomonad/package.scala
    // TODO: Also its dependencies addded to chapter07 par2
    def runLog[O](src: Process[IO, O]): IO[IndexedSeq[O]] = IO.pure {
      //val E = java.util.concurrent.Executors.newFixedThreadPool(4)

      def go(cur: Process[IO, O], acc: IndexedSeq[O]): IndexedSeq[O] =
        cur match
          case Emit(h, t) => go(t, acc :+ h)
          case Halt(End) => acc
          case Halt(err) => throw err
          case Await(req, recv) =>
            val next = try for {
              a <- req
            } yield recv(Right(a))
              
              // recv(Right(req.run)) // unsafePerformIO(req)(E)))
            catch {
              case err: Throwable => recv(Left(err))
            }
            go(next, acc)

      try go(src, IndexedSeq.empty)
      finally println("DONE")
    }

    val p: Process[IO, String] = await(IO.pure(BufferedReader(FileReader("/Users/kvandenberg/tmp")))) {
      case Right(b) => 
        lazy val next: Process[IO, String] = await(IO.pure(b.readLine)) {
          case Left(e)     => await(IO.pure(b.close()))(_ => Halt(e))
          case Right(line) => 
            if line eq null then Halt(End)
            else Emit(line, next)
        }
        next
      case Left(e) => Halt(e)
    }
    
@main def run_chapter15_execute: Unit =
  import chapter15_ext.*
  
  val x = Process.runLog(Process.p)
  println(x)