import chapter11.Monad
import scala.io.Source
import cats.effect.IO
object chapter15: 
  
  enum Process[I, O]:
    case Emit(head: O, tail: Process[I, O])
    case Await(recv: Option[I] => Process[I, O])
    case Halt()
    
    def apply(s: LazyList[I]): LazyList[O] = this match
      case Halt()      => LazyList()
      case Await(recv) => s match
        case h #:: t => recv(Some(h)).apply(t)
        case xs      => recv(None).apply(xs)
      case Emit(h, t)  => h #:: t.apply(s)
      
    def repeat: Process[I, O] =
      def go(p: Process[I, O]): Process[I, O] = p match
        case Halt()      => go(this)
        case Await(recv) => Await {
          case None => recv(None)
          case i    => go(recv(i))
        }
        case Emit(h, t)  => Emit(h, go(t))
      go(this)
      
    def |>[O2](p2: Process[O, O2]): Process[I, O2] = p2 match
      case Halt()     => Halt()
      case Emit(h, t) => Emit(h, this |> t)
      case Await(f)   => this match
        case Halt()     => Halt() |> f(None)
        case Emit(h, t) => t |> f(Some(h))
        case Await(g)   => Await((i: Option[I]) => g(i) |> p2)
        
    def map[O2](f: O => O2): Process[I, O2] = this |> Process.lift(f)
    
    def ++(p: => Process[I, O]): Process[I, O] = this match
      case Halt()      => p
      case Emit(h, t)  => Emit(h, t ++ p)
      case Await(f)    => Await(x => f(x) ++ p)
      
    def flatMap[O2](f: O => Process[I, O2]): Process[I, O2] = this match
      case Halt()     => Halt()
      case Emit(h, t) => f(h) ++ t.flatMap(f)
      case Await(g)   => Await(x => g(x).flatMap(f))
      
    def monad[I]: Monad[[x] =>> Process[I, x]] = new Monad[[x] =>> Process[I, x]]:
      override def unit[O](o: => O): Process[I, O] = Emit(o, Halt())
      override def flatMap[O, O2](fa: Process[I, O])(f: O => Process[I, O2]): Process[I, O2] = fa.flatMap(f)

    // 15.6 part02
    def zipWithIndex: Process[I, (O, Int)] = Process.zip(this, Process.count.map(_ - 1))
    
      
     
  object Process: 
    
    def emit[I, O](h: O, t: Process[I, O] = Halt()): Process[I, O] = Emit(h, t)
    def await[I, O](recv: Option[I] => Process[I, O]): Process[I, O] = Await(recv)
    def halt[I, O]: Process[I, O] = Halt()
    def id[I]: Process[I, I] = lift(identity)
    
    def liftOne[I, O](f: I => O): Process[I, O] =
      await((i: Option[I]) => i match
        case Some(ii) => emit(f(ii), halt)
        case None     => halt
      )
      
    def lift[I, O](f: I => O): Process[I, O] = liftOne(f).repeat

    def filter[I](p: I => Boolean): Process[I, I] = await[I, I] {
      case Some(i) if p(i) => emit(i, halt)
      case _               => halt
    }.repeat

    def sum: Process[Double, Double] =
      def go(acc: Double): Process[Double, Double] =
        await {
          case Some(d) => emit(d + acc, go(d + acc))
          case None => halt
        }

      go(0.0)
      
    // 15.1
    def take[I](n: Int): Process[I, I] =
      await {
        case Some(i) if n >= 1 => emit(i, take(n - 1))
        case _                 => halt
      }
      
    def drop[I](n: Int): Process[I, I] =
      await {
        case Some(i) if n > 1  => drop(n - 1)
        case _                 => id
      }
      
    def takeWhile[I](f: I => Boolean): Process[I, I] =
      await {
        case Some(i) if f(i) => emit(i, takeWhile(f))
        case _               => halt
      }

    def dropWhile[I](f: I => Boolean): Process[I, I] =
      await {
        case Some(i) if f(i) => dropWhile(f)
        case Some(i)         => emit(i, id)
        case _               => halt
      }
      
    // 15.2
    def count[I]: Process[I, Int] =
      def go(acc: Int): Process[I, Int] =
        await {
          case Some(_) => emit(acc + 1, go(1 + acc))
          case None => halt
        }
      go(0)
      
    def mean: Process[Double, Double] =
      def go(n: Int, acc: Double): Process[Double, Double] =
        await {
          case Some(i) => emit((acc + i) / (n + 1),  go(n+1, acc + i))
          case None    => halt
        }
      go(0, 0)
      
    def loop[S, I, O](z: S)(f: (I, S) => (O, S)): Process[I, O] =
      await {
        case Some(i) => 
          val (o, s2): (O, S) = f(i, z)
          emit(o, loop(s2)(f))
        case None    => halt
      }
      
    // 15.4 
    def sumLoop: Process[Double, Double] = loop(0.0)((i: Double, s: Double) => (s + i, s + i))
    def countLoop[I]: Process[I, Int] = loop(0)((_: I, s: Int) => (s + 1, s + 1))
    def meanLoop: Process[Double, Double] = loop((0.0, 0.0))((i: Double, s: (Double, Double)) => ((s._2 + i) / (s._1 + 1), (s._1 + 1, s._2 + i)))
    
    // 15.6 part01
    def zipWithIndexLoop[I]: Process[I, (I, Int)] = loop(0)((i: I, s: Int) => ((i, s), s + 1))
    
    // 15.7
    def zip[A, B, C](p1: Process[A, B], p2: Process[A, C]): Process[A, (B, C)] = (p1, p2) match
        case (Halt(), _) => Halt()
        case (_, Halt()) => Halt()
        case (Emit(b, t1), Emit(c, t2)) => Emit((b, c), zip(t1, t2))
        case (Await(recv1), _) =>
          Await((oa: Option[A]) => zip(recv1(oa), feed(oa)(p2)))
        case (_, Await(recv2)) =>
          Await((oa: Option[A]) => zip(feed(oa)(p1), recv2(oa)))

    def feed[A, B](oa: Option[A])(p: Process[A, B]): Process[A, B] = p match
        case Halt() => p
        case Emit(h, t) => Emit(h, feed(oa)(t))
        case Await(recv) => recv(oa)
        
    // 15.8
    def exists[I](f: I => Boolean): Process[I, Boolean] =
      await {
        case Some(i) if f(i) => emit(true, halt)
        case Some(_)         => emit(false, exists(f))
        case None            => halt
      }
      
    def processFile[A, B](f: String, p: Process[String, A], z: B)(g: (B, A) => B): IO[B] = IO {

      def go(ss: Iterator[String], cur: Process[String, A], acc: B): B = cur match
        case Halt() => acc
        case Await(recv) =>
          val next = if ss.hasNext then recv(Some(ss.next)) else recv(None)
          go(ss, next, acc)
        case Emit(h, t) => go(ss, t, g(acc, h))

      val s = Source.fromFile(f)
      try go(s.getLines(), p, z)
      finally s.close
    }
    
      
@main def run_chapter15: Unit =


  /**
   * Check out: https://fs2.io/#/
   */

  import chapter15.Process
  
  val x: LazyList[Double] = LazyList(1,2,3,4,5,6,7,8,9.0)
  val z: LazyList[Char]   = LazyList('a', 'b', 'c', 'd')
  val sumF: Process[Double, Double] = Process.sum
  
  println(sumF.apply(x))
  
  println(s"take: ${Process.take(4).apply(x).toList}")
  println(s"drop: ${Process.drop(4).apply(x).toList}")
  println(s"takeWhile: ${Process.takeWhile((i: Double) => i < 5.9).apply(x).toList}")
  println(s"dropWhile: ${Process.dropWhile((i: Double) => i < 5.9).apply(x).toList}")
  println(s"count: ${Process.count.apply(z).toList}")
  println(s"mean: ${Process.mean.apply(x).toList}")
  println(s"sumLoop: ${Process.sum.apply(x).toList}")
  println(s"countLoop: ${Process.count.apply(z).toList}")
  println(s"meanLoop: ${Process.mean.apply(x).toList}")
  println(s"pipeline: ${(Process.filter((i: Double) => i % 2 == 0) |> Process.lift(_ + 1)).apply(x).toList}")
  println(s"++: ${(Process.sum ++ Process.mean).apply(x).toList}")
  println(s"zipWithIndex: ${Process.sum.zipWithIndex.apply(x).toList}")
  println(s"exists: ${Process.exists((i: Double) => i == 7.0).apply(x).toList}")


  
  
  