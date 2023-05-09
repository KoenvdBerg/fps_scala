import chapter06.SimpleRNG
import testing.Prop.{FailedCase, SuccessCount, TestCases}

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

  type MaxSize = Int
  case class Prop(run: (MaxSize, TestCases, RNG) => Result):

    // 8.9
    def &&(that: Prop): Prop = Prop(
      (max: MaxSize, n: TestCases, rng: RNG) => run(max, n, rng) match
        case Passed => that.run(max, n, rng)
        case Falsified(f, s) => Falsified(f, s))

    def ||(that: Prop): Prop = Prop(
      (max: MaxSize, n: TestCases, rng: RNG) => run(max, n, rng) match
        case Passed => Passed
        case Falsified(f, _) => that.tag(f).run(max, n, rng))

    def tag(msg: String): Prop = Prop(
      (max: MaxSize, n: TestCases, rng: RNG) => run(max, n, rng) match
        case Passed => Passed
        case Falsified(f, s) => Falsified(msg + "\n" + f, s))
  sealed trait Result:
    def isFalsified: Boolean
  case object Passed extends Result:
    override def isFalsified: Boolean = false
  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result:
    override def isFalsified: Boolean = true


  object Prop:
    type TestCases = Int
    type FailedCase = String
    type SuccessCount = Int

    def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
      forAll(g.forSize(_))(f)
    def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop(
      (max: MaxSize, n: Int, rng: RNG) =>
        val casesPerSize: Int = (n + (max - 1)) / max
        val props: LazyList[Prop] =
          LazyList.from(0)
            .take(n.min(max) + 1)
            .map(i => forAll(g(i))(f))
        val prop: Prop = props
          .map(p => Prop(
            (max: MaxSize, _, rng: RNG) =>
              p.run(max, casesPerSize, rng)
          )).toList.reduce(_ && _)
        prop.run(max, n, rng)
    )

    def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop(
      (max: MaxSize, n: Int, rng: RNG) =>
            randomStream(as)(rng).zip(LazyList.from(0)).take(n).map{
            case (a, i) => try {
              if f(a) then Passed else Falsified(a.toString, i)
            } catch { case e: Exception => Falsified(buildMsg(a, e), i)}
          }.find(_.isFalsified).getOrElse(Passed)
        )



    def randomStream[A](g: Gen[A])(rng: RNG): LazyList[A] =
      LazyList.unfold(rng)(rng => Some(g.sample.run(rng)))

    def buildMsg[A](s: A, e: Exception): String =
      s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace: \n ${e.getStackTrace.mkString("\n")}"

  case class Gen[A](sample: State[RNG, A]):
    // 8.6
    def flatMap[B](f: A => Gen[B]): Gen[B] =
      Gen(sample.flatMap(s => f(s).sample))

    // 8.10
    def unsized: SGen[A] = SGen((_: Int) => this)

    def listOfN(size: Gen[Int]): Gen[List[A]] =
      size.flatMap(s => Gen.listOfN(s, this))

  object Gen:
    def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

    // 8.4
    def choose(start: Int, stopExclusive: Int): Gen[Int] =
      Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

    def char: Gen[Char] = Gen(choose(97, 123).sample.map(_.toChar))
    def double: Gen[Double] = Gen(State(RNG.double))

    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
      Gen(State.sequence(List.fill(n)(g.sample)))

    // 8.12
    def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(
      (n: Int) => listOfN(n, g)
    )

    // Can we generate strings somehow using our existing primitives?
    def stringOfN(n: Int, g: Gen[Char]): Gen[String] =
      Gen(Gen.listOfN(n, g).sample.map(_.mkString("")))

    def doubleInt(g: Gen[Int], rng: RNG): (Int, Int) =
      val (x1, rng2) = g.sample.run(rng)
      val (x2, _) = g.sample.run(rng2)
      (x1, x2)


    // 8.5
    def boolean: Gen[Boolean] = Gen(State(RNG.nonNegativeInt).map(n => n % 2 == 0))

    // 8.7
    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
      boolean.flatMap(b => if b then g1 else g2)

    // 8.8
    def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
      import math.*
      val threshold: Double = math.abs(g1._2) / (math.abs(g1._2) + math.abs(g2._2))
      double.flatMap(dd => if threshold < dd then g1._1 else g2._1)


  case class SGen[A](forSize: Int => Gen[A])



@main def chapter08: Unit =
  println("KLFJD")

  import testing.*
  import state.*

  val rng = SimpleRNG(42)
  val x = Gen(State(rng.unit(1)))

  println(Gen.choose(1, 100).sample.run(rng))

  val y = Gen.boolean
  println(y.sample.run(rng))
  println(Gen.listOfN(10, y))
  println(Gen.listOfN(10, x))

  val xx = Gen.choose(1, 100)
  println(Gen.doubleInt(xx, rng))

  println(Gen.stringOfN(20, Gen.char).sample.run(rng))
  val stringgen: Gen[String] = Gen.stringOfN(5, Gen.char)

  // below generates list of strings with length 5, as specified in the stringgen value.
  println(stringgen.listOfN(xx).sample.run(rng))