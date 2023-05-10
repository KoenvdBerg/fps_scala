import state.State.unit

import math.*

object chapter06:
  trait RNG:
    def nextInt: (Int, RNG)

    def int: Rand[Int] = _.nextInt

    def unit[A](a: A): Rand[A] =
      rng => (a, rng)

    def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
      rng => {
        val (a, rng2) = s(rng)
        (f(a), rng2)
      }

    // 6.8
    def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
      rng => {
        val (a, rng2) = f(rng)
        g(a)(rng2)
      }

    // 6.9
    def _map[A, B](s: Rand[A])(f: A => B): Rand[B] =
      this.flatMap(s)(a => unit(f(a)))

    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
      this.flatMap(ra)(aa => this.map(rb)(bb => f(aa, bb)))



  type Rand[+A] = RNG => (A, RNG)

  case class SimpleRNG(seed: Long) extends RNG:
    override def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)

    def nonNegativeEven: Rand[Int] =
      map(RNG.nonNegativeInt)(i => i - i % 2)



    // 6.5
    def double: Rand[Double] =
      map(_.nextInt)(i => if i < 0 then i.toDouble / Int.MinValue else i.toDouble / Int.MaxValue)

    val randIntDouble: Rand[(Int, Double)] =
      RNG.both(int, double)


  object RNG:
    def randomPair(rng: RNG): ((Int, Int), RNG) =
      val (i1, rng2) = rng.nextInt
      val (i2, rng3) = rng2.nextInt
      ((i1, i2), rng3)

    // 6.1
    def nonNegativeInt(rng: RNG): (Int, RNG) =
      val (n, rng2) = rng.nextInt
      if n == Int.MinValue then
        (math.abs(Int.MinValue-1), rng2)
      else
        (math.abs(n), rng2)

    // 6.2
    def double(rng: RNG): (Double, RNG) =
      val (n, rng2) = rng.nextInt
      if n < 0 then
        (n.toDouble / Int.MinValue-1, rng2)
      else
        (n.toDouble / Int.MaxValue+1, rng2)

    // 6.3
    def intDouble(rng: RNG): ((Int, Double), RNG) =
      val (d, rng2): (Double, RNG) = double(rng)
      val (i, rng3): (Int, RNG) = rng2.nextInt
      ((i, d), rng3)

    def doubleInt(rng: RNG): ((Double, Int), RNG) =
      val (d, rng2): (Double, RNG) = double(rng)
      val (i, rng3): (Int, RNG) = rng2.nextInt
      ((d, i), rng3)

    def double3(rng: RNG): ((Double, Double, Double), RNG) =
      val (d1, rng2): (Double, RNG) = double(rng)
      val (d2, rng3): (Double, RNG) = double(rng2)
      val (d3, rng4): (Double, RNG) = double(rng3)
      ((d1, d2, d3), rng4)

    // 6.4
    def intsWithUnfold(count: Int)(rng: RNG): (List[Int], RNG) =
      val x = LazyList.unfold(rng)(r =>
        val (i, rngNext) = r.nextInt
        Some(((i, rngNext), rngNext)))
        .take(count)
        .toList
      (x.map(_._1), x.map(_._2).last)

    def ints(count: Int)(rng: RNG): (List[Int], RNG) =
      def go(c: Int, r: RNG, acc: List[Int] = Nil): (List[Int], RNG) =
        if c <= 0 then (acc, r)
        else
          val (i, rngNext): (Int, RNG) = r.nextInt
          go(c-1, rngNext, i :: acc)

      go(count, rng)

    // 6.6
    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
      rng => {
        val (a, rngA): (A, RNG) = ra(rng)
        val (b, rngB): (B, RNG) = rb(rngA)
        (f(a, b), rngB)
      }

    def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
      map2(ra, rb)((_, _))


    // 6.7
    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs match
      case h :: t => map2(h, sequence(t))(_ :: _)
      case Nil => rng => (Nil, rng)

    def ints2(count: Int)(rng: RNG): Rand[List[Int]] =
      sequence(List.fill(count)(rng.int))






@main def run_chapter06(): Unit =

  val x = chapter06.SimpleRNG(42)
  val y = x.int
  val z = x.int
  println(s"y: ${y(x)}, z: ${z(x)}")
  println(chapter06.RNG.map2(y, z)(_ + _)(x))

  val u = x.randIntDouble
  println(u(x))

  val g = List(chapter06.SimpleRNG(1), chapter06.SimpleRNG(2), chapter06.SimpleRNG(3), chapter06.SimpleRNG(4))
  val res = chapter06.RNG.sequence(g.map(_.unit(2)))
  println(res(x))
  println(res(x)._1)

  val tt = chapter06.RNG.ints2(5)(x)
  println(tt(x))

  val s = state.State
  println(s)

  println("### TESTING CUSTOM STATE ###")
  val r = state.State((s:Int) => (s*2, s+2))
  println(r.map(_ + 20).run(100))

