import testing.*


object chapter10:

  trait Monoid[A]:
    def op(a1: A, a2: A): A
    def zero: A

  object Monoid:

    val stringMonoid: Monoid[String] = new Monoid[String]:
      override def op(a1: String, a2: String): String = a1 + a2
      override def zero: String = ""

    def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]]:
      override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
      override def zero: List[A] = Nil

    // 10.1
    val intAddition: Monoid[Int] = new Monoid[Int]:
      override def op(a1: Int, a2: Int): Int = a1 + a2
      override def zero: Int = 0

    val intMultiplication: Monoid[Int] = new Monoid[Int]:
      override def op(a1: Int, a2: Int): Int = a1 * a2
      override def zero: Int = 1

    val booleanOr: Monoid[Boolean] = new Monoid[Boolean]:
      override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
      override def zero: Boolean = false

    val booleanAnd: Monoid[Boolean] = new Monoid[Boolean]:
      override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
      override def zero: Boolean = true

    // 10.2
    def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]]:
      override def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)
      override def zero: Option[A] = None

    // 10.3
    def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A]:
      override def op(a1: A => A, a2: A => A): A => A = a1.compose(a2)
      override def zero: A => A = (a: A) => a

    // 10.4
    def monoidLawsZero[A](m: Monoid[A], gen: Gen[A]): Prop =
      Prop.forAll(gen)(
        (s: A) =>
          m.op(m.zero, s) == m.op(s, m.zero)
      )

    def monoidLawsOp[A](m: Monoid[A], gen: Gen[A]): Prop =
      Prop.forAll(
        for {
          x <- gen
          y <- gen
          z <- gen
        } yield (x, y, z))((s: (A, A, A)) =>
      m.op(m.op(s._1, s._2), s._3) == m.op(s._1, m.op(s._2, s._3))
      )

    //  10.5
    def concatenate[A](as: List[A], m: Monoid[A]): A =
      as.foldLeft(m.zero)(m.op)

    def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
      // concatenate(as.map(f), m)  // Using map
      as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))  // without using map


    // 10.6
    def foldRightMap[A, B](as: List[A], z: B)(f: (A, B) => B): B =
      foldMap(as, listMonoid[B]){
        case Nil => List(z)
        case h :: t => f(h, t.head)
      }


@main def run_chapter10: Unit =
  println("LKDJFKLSF")
  import chapter10.Monoid.*
  import testing.Gen.*

  val s: Gen[String] = Gen.stringOfN(10, Gen.char)
  val y: Gen[Int] = Gen.choose(0, 10)

  // Zero law for Monoids
  Prop.run(monoidLawsZero(stringMonoid, s))
  Prop.run(monoidLawsZero(listMonoid[Int], Gen.listOfN(10, y)))
  Prop.run(monoidLawsZero(listMonoid[String], Gen.listOfN(10, s)))
  Prop.run(monoidLawsZero(intAddition, y))
  Prop.run(monoidLawsZero(intMultiplication, y))
  Prop.run(monoidLawsZero(booleanOr, Gen.boolean))
  Prop.run(monoidLawsZero(booleanAnd, Gen.boolean))
  Prop.run(monoidLawsZero(optionMonoid, y.map(x => if x < 5 then Some(x) else None)))
  Prop.run(monoidLawsZero(endoMonoid[Int], y.map((f: Int) => (g: Int) => f + g)))

  // Combine (op) law for Monoids
  Prop.run(monoidLawsOp(stringMonoid, s))
  Prop.run(monoidLawsOp(listMonoid[Int], Gen.listOfN(10, y)))
  Prop.run(monoidLawsOp(listMonoid[String], Gen.listOfN(10, s)))
  Prop.run(monoidLawsOp(intAddition, y))
  Prop.run(monoidLawsOp(intMultiplication, y))
  Prop.run(monoidLawsOp(booleanOr, Gen.boolean))
  Prop.run(monoidLawsOp(booleanAnd, Gen.boolean))
  Prop.run(monoidLawsOp(optionMonoid, y.map(x => if x < 5 then Some(x) else None)))
  Prop.run(monoidLawsOp(endoMonoid[Int], y.map((f: Int) => (g: Int) => f + g)))

  // testing foldMap
  val x: Int = foldMap(List(1, 5, 10), intAddition)((f: Int) => f + 1)
  println(x)




