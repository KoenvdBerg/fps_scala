import chapter06.SimpleRNG
import testing.Prop.{FailedCase, SuccessCount, TestCases}
import chapter06_state.State.*
import chapter06_state.State

import java.util.concurrent.{ExecutorService, Executors}

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
  val prop3 = forAll(sumIns)(ns => ns.isEmpty || ns.sorted.last == ns.max)
  prop3.check()


object testing:
  import chapter06.RNG

  type MaxSize = Int
  case class Prop(run: (MaxSize, TestCases, RNG) => Result):

    // 8.9
    def &&(that: Prop): Prop = Prop(
      (max: MaxSize, n: TestCases, rng: RNG) => run(max, n, rng) match
        case Passed => that.run(max, n, rng)
        case other  => other)

    def ||(that: Prop): Prop = Prop(
      (max: MaxSize, n: TestCases, rng: RNG) => run(max, n, rng) match
        case Falsified(f, _) => that.tag(f).run(max, n, rng)
        case other => other)

    def tag(msg: String): Prop = Prop(
      (max: MaxSize, n: TestCases, rng: RNG) => run(max, n, rng) match
        case Falsified(f, s) => Falsified(msg + "\n" + f, s)
        case other => other)
  sealed trait Result:
    def isFalsified: Boolean
  case object Passed extends Result:
    override def isFalsified: Boolean = false
  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result:
    override def isFalsified: Boolean = true
  case object Proved extends Result:
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

    def run(p: Prop, maxSize: Int = 100, testCases: Int = 100, rng: RNG = SimpleRNG(System.currentTimeMillis)): Unit =
      p.run(maxSize, testCases, rng) match
        case Falsified(msg, n) =>
          println(s"! Falsified after $n passed tests: \n $msg")
        case Passed =>
          println(s"+ OK, passed $testCases tests.")
        case Proved =>
          println(s"+ OK, proved property")

    def check(p: => Boolean): Prop = Prop(
      (_, _, _) => if p then Proved else Falsified(" ()", 0))

    val S: Gen[ExecutorService] = Gen.weighted(
      Gen.choose(1, 4).map(Executors.newFixedThreadPool) -> 0.75,
      Gen.unit(Executors.newCachedThreadPool) -> 0.25)

    import chapter07.Par
    def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
      forAll(S ** g){ case (s, a) => f(a)(s).get}

  case class Gen[A](sample: State[RNG, A]):
    // 8.6
    def flatMap[B](f: A => Gen[B]): Gen[B] =
      Gen(sample.flatMap(s => f(s).sample))

    def map[B](f: A => B): Gen[B] =
      Gen(sample.map(s => f(s)))

    def map2[B, C](b: Gen[B])(f: (A, B) => C): Gen[C] =
      Gen(sample.map2(b.sample)(f))

    def **[B](g: Gen[B]): Gen[(A, B)] =
      this.map2(g)((_, _))

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

    def treeOfN[A](n: Int, g: Gen[A]): Gen[Tree[A]] =
      Gen(State.sequenceTree(Tree.fill(n)(g.sample)))

    // 8.13
    def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(
      (n: Int) => if n <= 0 then listOfN(1, g) else listOfN(n, g)
      // can be replaced with n.max(1)
    )

    import Tree.*
    def treeOf[A](g: Gen[A]): SGen[Tree[A]] = SGen(
      (n: Int) => treeOfN(n, g)
    )

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

  val smallInt: Gen[Int] = Gen.choose(-10, 10)
  val maxProp: Prop = Prop.forAll(Gen.listOf1(smallInt))(
    (ns: List[Int]) =>
      val max = ns.max
      !ns.exists(_ > max)
  )
  Prop.run(maxProp)

  // 8.14
  val ints: Gen[Int] = Gen.choose(0, 100)
  val sortedPropInt: Prop = Prop.forAll(Gen.listOf1(ints))(
    (ns: List[Int]) =>
      !ns.sorted.dropRight(1).exists(_ > ns.max)
  )
  Prop.run(sortedPropInt)

  val sortedProp2: Prop = Prop.forAll(Gen.listOf(ints))(
    (ns: List[Int]) =>
      val sortedns = ns.sorted

      sortedns.isEmpty || sortedns.tail.isEmpty || !sortedns.zip(sortedns.tail).exists(
        (a: Int, b: Int) => a > b
      ) && ns.exists(sortedns.contains(_))
        && sortedns.exists(ns.contains(_))

  )

  Prop.run(sortedProp2)

  import java.util.concurrent.{ExecutorService, Executors}
  import chapter07.Par

  // method 1 using forAll
  val ES: ExecutorService = Executors.newCachedThreadPool
  val p1: Prop = Prop.forAll(Gen.unit(Par.unit(1)))(i =>
    Par.map(i)(_ + 1)(ES).get == Par.unit(2)(ES).get)
  Prop.run(p1)

  // method 2 using check
  val p2: Prop = Prop.check{
    val p1: Par[Int] = Par.map(Par.unit(1))(_ + 1)
    val p2: Par[Int] = Par.unit(2)
    p1(ES).get == p2(ES).get()
  }
  Prop.run(p2)

  // method 3 using Par.equal
  val p3 = Prop.check(
    Par.equal(
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2)
    )(ES).get
  )
  Prop.run(p3)

  val pint: Gen[Par[Int]] = Gen.choose(0,10).map(Par.unit)
  val p4: Prop = Prop.forAllPar(pint)(n => Par.equal(Par.map(n)(y => y), n))
  Prop.run(p4)

  // 8.17
  val pfork = Gen.choose(0, 10).map(Par.unit)
  val p5: Prop = Prop.forAllPar(pfork)(n => Par.equal(Par.fork(n), n))
  Prop.run(p5)

  // 8.18
  // takeWhile should satisfy:
  //  - all of the returned elements should be present in the original List
  //  - the order of the returned elements should be unchanged
  //  - the returned elements should begin at the head of the list
  //  - the returned elements by takeWhile + the returned elements by dropWhile using the same A => Boolean function
  //    should return the original List.

  val isEven: Int => Boolean = (i: Int) => i % 2 == 0
  val takeWhileProp: Prop = Prop.forAll(Gen.listOf1(ints))((ns: List[Int]) => ns.takeWhile(isEven).forall(isEven))
  Prop.run(takeWhileProp)

  // properties for take
  val takeProp: Prop = Prop.forAll(Gen.listOf(ints))(
    (ns: List[Int]) =>
      val res: List[Int] = ns.take(5)

      res.isEmpty || res.length == 5.min(ns.length) && res.head == ns.head
      && res.exists(ns.contains) && ns.exists(res.contains)
  )
  Prop.run(takeProp)

  // sized generator for the Tree datatype from chapter03 -- p. 144
  val tt = Tree.fill(4)(ints.sample)
  val treegen: Gen[Tree[Int]] = Gen.treeOfN(4, smallInt)

  // properties for Tree
  val treeProp: Prop = Prop.check {
    val t1: Tree[Int] = Tree.mapf(Leaf(1))(_ + 1)
    val t2: Tree[Int] = Leaf(2)
    t1 == t2
  }
  val treeProp2: Prop = Prop.check {
    val t1: Tree[Int] = Tree.map(Leaf(1))(_ + 1)
    val t2: Tree[Int] = Leaf(2)
    t1 == t2
  }
  val treeProp3: Prop = Prop.forAll(Gen.treeOf(smallInt))(
    (ns: Tree[Int]) =>
      val res: Int = Tree.size(ns)+1        // any tree size
      res == math.pow(2, Tree.depth(ns)+1)  // generated tree size with fill should be 2 to the power depth nodes
  )

  Prop.run(treeProp, maxSize = 5)
  Prop.run(treeProp2, maxSize = 5)
  Prop.run(treeProp3, maxSize = 10)  // treesize not too big to prevent heapspace errors


