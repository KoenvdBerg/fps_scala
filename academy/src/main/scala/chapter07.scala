import java.util.concurrent.{Callable, ExecutorService, Executors, Future}
import scala.compiletime.testing.ErrorKind.Parser
import scala.concurrent.duration.TimeUnit

object chapter07 {

  def sum2(ints: IndexedSeq[Int]): Par[Int] =
    if ints.size <= 1 then
      Par.unit(ints.headOption.getOrElse(0))
    else
      val (l, r) = ints.splitAt(ints.length/2)
      Par.map2(Par.fork(sum2(l)), Par.fork(sum2(r)))(_ + _)

  type Par[A] = ExecutorService => Future[A]

  object Par:
    def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

    private case class UnitFuture[A](override val get: A) extends Future[A]:
      override def isDone = true
      override def get(timeout: Long, units: TimeUnit) = get
      override def isCancelled = false
      override def cancel(evenIfRunning: Boolean): Boolean = false

    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
      (es: ExecutorService) =>
        val af = a(es)
        val bf = b(es)
        UnitFuture(f(af.get, bf.get))

    def fork[A](a: => Par[A]): Par[A] =
      es => es.submit(new Callable[A] {
        override def call = a(es).get
      })

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    // 7.4
    def asyncF[A, B](f: A => B): A => Par[B] =
      (a: A) => lazyUnit(f(a))

    def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
      map(parList)(_.sorted)

    def map[A, B](pa: Par[A])(f: A => B): Par[B] =
      map2(pa, unit(()))((a, _) => f(a))

    def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] =
      val fbs: List[Par[B]] = ps.map(asyncF(f))
      sequence(fbs)

    // 7.5
    def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps match
      case h :: t => map2(h, fork(sequence(t)))(_ :: _)
      case Nil    => unit(Nil)

    def sequenceViaFold[A](ps: List[Par[A]]): Par[List[A]] =
      ps.foldRight(unit(List.empty[A]))((pa, acc) => map2(pa, acc)(_ :: _))

    // 7.6
    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
      val fls: List[Par[List[A]]] = as.map(asyncF((a: A) => if f(a) then List(a) else List()))
      map(sequence(fls))(_.flatten)



    def parSum(ints: IndexedSeq[Int]): Par[Int] =
      parSeq(ints, 0)(_ + _, i => i)

    def parMax(ints: IndexedSeq[Int]): Par[Int] =
      parSeq(ints, -Int.MaxValue)((i1, i2) => i1.max(i2), i => i)

    def parString(strs: List[String]): Par[Int] =
      val ss: Par[List[Int]] = sequence(strs.map(f => lazyUnit(f.split(" ").length)))
      map(ss)(_.sum)

    def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] =
      val f1 = (a: A, b: B) => (c: C) => f(a, b, c)
      val f2: Par[C => D] = map2(a, b)(f1)
      map2(f2, c)((f3: C => D, c: C) => f3(c))

    def map4[A, B, C, D, E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(f: (A, B, C, D) => E): Par[E] =
      val f1 = (a: A, b: B) => (c: C) => (d: D) => f(a, b, c, d)
      val f2: Par[C => D => E] = map2(a, b)(f1)
      val f3: Par[D => E] = map2(f2, c)((g: C => D => E, c: C) => g(c))
      map2(f3, d)((g: D => E, d: D) => g(d))

    def map5[A, B, C, D, E, F](a: Par[A], b: Par[B], c: Par[C], d: Par[D], e: Par[E])(f: (A, B, C, D, E) => F): Par[F] =
      val f1 = (a: A, b: B) => (c: C) => (d: D) => (e: E) => f(a, b, c, d, e)
      val f2: Par[C => D => E => F] = map2(a, b)(f1)
      val f3: Par[D => E => F] = map2(f2, c)((g: C => D => E => F, c: C) => g(c))
      val f4: Par[E => F] = map2(f3, d)((g: D => E => F, d: D) => g(d))
      map2(f4, e)((g: E => F, e: E) => g(e))

    def map5Short[A, B, C, D, E, F](a: Par[A], b: Par[B], c: Par[C], d: Par[D], e: Par[E])(f: (A, B, C, D, E) => F): Par[F] =
      val f1 = (a: A, b: B, c: C, d: D) => (e: E) => f(a, b, c, d, e)
      val f2: Par[E => F] = map4(a, b, c, d)(f1)
      map2(f2, e)((g: E => F, e: E) => g(e))

    def parSeq[A, Z](as: IndexedSeq[A], z: A)(f: (Z, Z) => Z, g: A => Z): Par[Z] =
      if as.size <= 1 then
        Par.unit(g(as.headOption.getOrElse(z)))
      else
        val (l, r) = as.splitAt(as.length / 2)
        map2(fork(parSeq(l, z)(f, g)), fork(parSeq(r, z)(f, g)))(f)

    def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
      p(e).get == p2(e).get

    def delay[A](fa: => Par[A]): Par[A] =
      es => fa(es)


  /**
   * 7.7
   * given: map(y)(id) == y then map(map(y)(g))(f) == map(y)(f compose g)
   * let: y be map(y)(f compose g) then:
   * map(map(y)(f compose g)(id) --> map over id is itself, thus we can remove 1 map and the id part, thus:
   * --> map(y)(f compose g) == map(y)(f compose g). Now we only have to rewrite it to the left side of the original
   * equation:  map(y)(f compose g) = map(map(y)(g))(f), this is possible because f compose g = f(g) = map(map(x)(g))(f)
   *
   */
//    def run[A](s: ExecutorService)(a: A): A = a(s)



//  trait ExecutorService:
//    def submit[A](a: Callable[A]): Future[A]
//  trait Callable[A]:
//    def call: A
//  trait Future[A]:
//    def get: A
//    def get(timeout: Long, unit: TimeUnit): A
//    def cancel(evenIfRunning: Boolean): Boolean
//    def isDone: Boolean
//    def isCancelled: Boolean

}



@main def run_chapter07(): Unit =
  import chapter07.*

  println("kldsjf")


  val x = Vector(1,2,3,4,5,6)
  println(sum2(x))

  val y = Par.unit(List(5,23,7,2,4))
  Par.sortPar(y)

  val z = Par.parFilter(List(1,2,43,5,6))((i: Int) => i <= 4)

  val q = Par.parSum(IndexedSeq(1,2,3,4,5,6))

  val a = Par.lazyUnit(42 + 1)
  val S = Executors.newFixedThreadPool(4)
  println(Par.equal(S)(a, Par.fork(a)))



// YARD:
//    def parSum(ints: IndexedSeq[Int]): Par[Int] =
//      if ints.size <= 1 then
//        Par.unit(ints.headOption.getOrElse(0))
//      else
//        val (l, r) = ints.splitAt(ints.length / 2)
//        map2(fork(parSum(l)), fork(parSum(r)))(_ + _)