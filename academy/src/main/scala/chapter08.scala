import chapter06.SimpleRNG
import testing.Prop.{FailedCase, SuccessCount}

object PlayingWithScalaCheck extends App:
  import org.scalacheck.*
  import Prop.*

  def sum(ins: List[Int]): Int =
    ins.foldLeft(0)(_ + _)


  // 5.1
  val sumIns = Gen.listOf(Gen.choose(0, 1000))
  val sumProp = forAll(sumIns)(ns => sum(ns.reverse) == ns.sum)
  val sumIns2 = Gen.listOf(4)
  val sumProp2 = forAll(sumIns2)(ns => sum(ns) == 4 * ns.length)

  sumProp.check()
  sumProp2.check()

  // 5.2
  val prop3 = forAll(sumIns)(ns => ns.sorted.lastOption.getOrElse(()) == ns.max)
  prop3.check()


object testing:
  import chapter06.RNG
  import state.*

  def listOf[A](a: Gen[A]): Gen[List[A]] = ???
  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] = ???
  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???

  trait Prop:
    def check: Either[(FailedCase, SuccessCount), SuccessCount] = ???
    def &&(p: Prop): Prop = ???
//      new Prop:
//      override def check: Unit = Prop.this.check && p.check

  object Prop:
    type FailedCase = String
    type SuccessCount = Int

  case class Gen[A](sample: State[RNG, A]):

    // 8.4
    def choose(start: Int, stopExclusive: Int): Gen[Int] =
      Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

    // 8.5
    def boolean: Gen[Boolean] = Gen(State(RNG.nonNegativeInt).map(n => n % 2 == 0))

    def char: Gen[Char] = Gen(choose(97, 123).sample.map(_.toChar))

    def double: Gen[Double] = Gen(State(RNG.double))

    // 8.6
    def flatMap[B](f: A => Gen[B]): Gen[B] =
      Gen(sample.flatMap(s => f(s).sample))

    def listOfN(size: Gen[Int]): Gen[List[A]] =
      size.flatMap(s => Gen.listOfN(s, this))

  object Gen:
    def unit[A](a: => A): Gen[A] = Gen(State.unit(a))
    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
      Gen(State.sequence(List.fill(n)(g.sample)))

    // Can we generate strings somehow using our existing primitives?
    def stringOfN(n: Int, g: Gen[Char]): Gen[String] =
      Gen(Gen.listOfN(n, g).sample.map(_.mkString("")))

    def doubleInt(g: Gen[Int], rng: RNG): (Int, Int) =
      val (x1, rng2) = g.sample.run(rng)
      val (x2, _) = g.sample.run(rng2)
      (x1, x2)

    // 8.7
    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
      g1.boolean.flatMap(b => if b then g1 else g2)

    // 8.8
    def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
      import math.*
      val threshold: Double = math.abs(g1._2) / (math.abs(g1._2) + math.abs(g2._2))
      g1._1.double.flatMap(dd => if threshold < dd then g1._1 else g2._1)





@main def chapter08: Unit =
  println("KLFJD")

  import testing.*
  import state.*

  val rng = SimpleRNG(42)
  val x = Gen(State(rng.unit(1)))

  println(x.choose(1, 100).sample.run(rng))

  val y = Gen.unit(10).boolean
  println(y.sample.run(rng))
  println(Gen.listOfN(10, y))
  println(Gen.listOfN(10, x))

  val xx = x.choose(1, 100)
  println(Gen.doubleInt(xx, rng))

  println(Gen.stringOfN(20, x.char).sample.run(rng))
  val stringgen: Gen[String] = Gen.stringOfN(5, x.char)

  // below generates list of strings with length 5, as specified in the stringgen value.
  println(stringgen.listOfN(xx).sample.run(rng))