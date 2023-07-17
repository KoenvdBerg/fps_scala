//import chapter13_A.IO
import cats.effect.IO
import chapter11.Monad
import java.io.{BufferedReader, FileReader}

object chapter15_ext:

  trait MonadCatch[F[_]] extends Monad[F]: 
    def attempt[A](a: F[A]): F[Either[Throwable, A]]
    def fail[A](t: Throwable): F[A]
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
      
    def runLog(using MC: MonadCatch[F]): F[IndexedSeq[O]] =
      def go(cur: Process[F, O], acc: IndexedSeq[O]): F[IndexedSeq[O]] =
        cur match
          case Emit(h, t) => go(t, acc :+ h)
          case Halt(Process.End) => MC.unit(acc)
          case Halt(err) => MC.fail(err)
          case Await(req, recv) => MC.flatMap (MC.attempt(req)) { e => go(Process.Try(recv(e)), acc) }
      go(this, IndexedSeq.empty)
      
    def asFinalizer: Process[F, O] = this match
      case Emit(h, t)       => Emit(h, t.asFinalizer)
      case Halt(e)          => Halt(e)
      case Await(req, recv) => Process.await(req) {
        case Left(Process.Kill) => this.asFinalizer
        case x                  => recv(x)
      }
      
    def onComplete(p: => Process[F, O]): Process[F, O] =
      this.onHalt {
        case Process.End => p.asFinalizer
        case err         => p.asFinalizer ++ Halt(err)
      }
    
  object Process: 
    case object End extends Exception
    case object Kill extends Exception
    
    def await[F[_], O, A](req: F[A])(recv: Either[Throwable, A] => Process[F, O]): Process[F, O] = Await(req, recv)
    
    def resource[R, O](acquire: IO[R], use: R => Process[IO, O], release: R => Process[IO, O]): Process[IO, O] =
      await[IO, R, O](acquire)(r => use(r).onComplete(release(r)))
      
    // 15.11
    def eval[F[_], A](a: F[A]): Process[F, A] = await(a) {
      case Left(err) => Halt(err)
      case Right(a)  => Emit(a, Halt(End))
    }
    def eval_[F[_], A](a: F[A]): Process[F, B] = ??? // eval(a).drain[B]
    
    def Try[F[_], O](p: => Process[F, O]): Process[F, O] =
      try p
      catch {case e: Throwable => Halt(e)}

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