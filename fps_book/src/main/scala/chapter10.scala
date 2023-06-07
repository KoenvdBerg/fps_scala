import chapter10.Foldable
import chapter10.Monoid.WC.Stub
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
      val curriedf = (a: A) => (b: B) => f(a, b)
      foldMap(as, endoMonoid)(curriedf)(z)

    // 10.7
    def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
      if v.length <= 0 then m.zero
      else if v.length == 1 then f(v(0))
      else
        val (half1, half2): (IndexedSeq[A], IndexedSeq[A]) = v.splitAt(v.length / 2)
        m.op(foldMapV(half1, m)(f), foldMapV(half2, m)(f))

    // 10.9
    def isOrdered(is: IndexedSeq[Int]): Boolean =
      if is.length <= 1 then true
      else
        foldMapV(is, orderMonoid)((i: Int) => Some(i, true)) match
          case None    => false
          case Some((_, b)) => b

    val orderMonoid: Monoid[Option[(Int, Boolean)]] = new Monoid[Option[(Int, Boolean)]]:
      override def op(a1: Option[(Int, Boolean)], a2: Option[(Int, Boolean)]): Option[(Int, Boolean)] = (a1, a2) match
        case (Some(n), None)    => Some(n)
        case (None, Some(n))    => Some(n)
        case (Some(x, b1), Some(y, b2)) => Some(y.min(x), x <= y && b1 && b2)
        case _    => sys.error("boom!!!")
      override def zero: Option[(Int, Boolean)] = None

    // 10.10
    enum WC:
      case Stub(chars: String)
      case Part(lStub: String, words: Int, rStub: String)

    val wcMonoid: Monoid[WC] = new Monoid[WC]:
      import WC.*
      override def op(a1: WC, a2: WC): WC = (a1, a2) match
        case (Stub(a), Stub(b))                   => Stub(a + b)
        case (Stub(a), Part(l, w, r))             => Part(l + a, w, r)
        case (Part(l, w, r), Stub(a))             => Part(l, w, r + a)
        case (Part(l1, w1, r1), Part(l2, w2, r2)) => Part(l1, w1 + w2 + (if (r1 + l2) == "" then 0 else 1), r2)
      override def zero: WC = Stub("")

    // 10.11
    def countWords(s: String): Int =
      import WC.*
      val res: WC = foldMapV(s.toVector, wcMonoid)((c: Char) => if c.isWhitespace then Part("", 0, "") else Stub(c.toString))
      res match
        case Stub(_)       => 0
        case Part(l, w, r) => w + l.length.min(1) + r.length.min(1)


    // 10.16
    def productMonoid[A, B](a: Monoid[A], b: Monoid[B]): Monoid[(A, B)] =
      new Monoid[(A, B)]:
        override def zero: (A, B) = (a.zero, b.zero)
        override def op(a1: (A, B), a2: (A, B)): (A, B) = (a.op(a1._1, a2._1), b.op(a1._2, a2._2))

    def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
      new Monoid[Map[K, V]]:
        override def zero: Map[K, V] = Map.empty[K, V]

        override def op(a1: Map[K, V], a2: Map[K, V]): Map[K, V] =
          (a1.keySet ++ a2.keySet).foldLeft(zero)((acc, k) =>
            acc.updated(k, V.op(a1.getOrElse(k, V.zero), a2.getOrElse(k, V.zero)))
          )
    val M: Monoid[Map[String, Map[String, Int]]] = mapMergeMonoid(mapMergeMonoid(intAddition))

    // 10.17
    def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] =
      new Monoid[A => B]:
        override def zero: A => B = (a: A) => B.zero
        override def op(a1: A => B, a2: A => B): A => B = (a: A) => B.op(a1(a), a2(a))

    // 10.18
    def bag[A](as: IndexedSeq[A]): Map[A, Int] =
      val bagMonoid: Monoid[Map[A, Int]] = mapMergeMonoid(intAddition)
      as
        .map((a: A) => Map(a -> 1))
        .foldLeft(bagMonoid.zero)(bagMonoid.op)

    def bagFold[A](as: IndexedSeq[A]): Map[A, Int] =
      val bagMonoid: Monoid[Map[A, Int]] = mapMergeMonoid(intAddition)
      foldMapV(as, bagMonoid)((a: A) => Map(a -> 1))

  trait Foldable[F[_]]:
    def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B = ???
    def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B = ???
    def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B = ???
    def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)

    // 10.15
    def toList[A](fa: F[A]): List[A] =
      foldLeft(fa)(List.empty[A])((b, a) => a :: b)


  object Foldable:

    // 10.12
    object listFoldable extends Foldable[List]:
      override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
        as.foldRight(z)(f)
      override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
        as.foldLeft(z)(f)
      override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
        as.foldLeft(mb.zero)((b, a) => mb.op(b, f(a)))

    object streamFoldable extends Foldable[LazyList]:
      override def foldRight[A, B](as: LazyList[A])(z: B)(f: (A, B) => B): B =
        as.foldRight(z)(f)
      override def foldLeft[A, B](as: LazyList[A])(z: B)(f: (B, A) => B): B =
        as.foldLeft(z)(f)
      override def foldMap[A, B](as: LazyList[A])(f: A => B)(mb: Monoid[B]): B =
        as.foldLeft(mb.zero)((b, a) => mb.op(b, f(a)))
    // 10.13
    import Tree.*
    object treeFoldable extends Foldable[Tree]:
      override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =  as match
        case Leaf(v)      => f(v, z)
        case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
      override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match
        case Leaf(v)      => f(z, v)
        case Branch(l, r) => foldLeft(l)(foldLeft(r)(z)(f))(f)
      override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
        foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))


    object optionFoldable extends Foldable[Option]:
      override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as match
        case None    => z
        case Some(v) => f(v, z)
      override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as match
        case None    => z
        case Some(v) => f(z, v)
      override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
        foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))


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

  println(foldMapV(Vector("lorem", "ipsum", "dolor", "sit", "amet"), stringMonoid)(a => a))
  println(foldMapV(Vector("lorem"), stringMonoid)(a => a))
  println(foldMapV(Vector(), stringMonoid)((a: String) => a))

  // testing ordering Monoid
  val xx: IndexedSeq[Int] = IndexedSeq(1,6,3,8,2)
  val yy: IndexedSeq[Int] = IndexedSeq(1, 6, 10, 15, 200)
  val zz: IndexedSeq[Int] = IndexedSeq(7,1,2,3,4,5,6,7)
  println(isOrdered(xx))
  println(isOrdered(yy))
  println(isOrdered(zz))

  // testing the WC monoid
  import WC.*
  val q1: WC = Part("lorem", 1, "do")
  val q2: WC = Part("lor", 2, "")
  val q3: WC = Part("consquitur", 0, "amet")

  println("\n##### TESTING WC MONOID #####")
  println(wcMonoid.op(wcMonoid.zero, q1))
  println(wcMonoid.op(q1, wcMonoid.zero))
  println(wcMonoid.op(q1, q2))
  println(wcMonoid.op(wcMonoid.op(q1, q2), q3))
  println(wcMonoid.op(q1, wcMonoid.op(q2 ,q3)))

  val testString: String = """dolor sit amet consequitur lorem ipsum bueno y mi caballo esta enfermo."""
  println(countWords(testString))

  println("\n##### TESTING TREE FOLDS #####")
  val t: Tree[Int] = Branch(Branch(Leaf(1), Leaf(100)), Leaf(30))
  println(Foldable.treeFoldable.foldLeft(t)(0)(_ + _))
  println(Foldable.treeFoldable.foldMap(t)(_.toString)(stringMonoid))

  println("\n##### TESTING OPTION FOLDS #####")
  val o: Option[Int] = Some(100)
  println(Foldable.optionFoldable.foldLeft(o)(1)(_ + _))
  println(Foldable.optionFoldable.foldMap(o)(_.toString)(stringMonoid))

  println("\n##### PROOF PRODUCT MONOID #####")
  val p: List[(String, Int)] = List(("joost", 200), ("bakt", 100), ("brood", 50))
  val combi: chapter10.Monoid[(String, Int)] = productMonoid(stringMonoid, intAddition)
  println(p.foldRight(combi.zero)(combi.op))

  Prop.run(monoidLawsZero(combi, y.map((i: Int) => (s"$i", i))))
  Prop.run(monoidLawsOp(combi, y.map((i: Int) => (s"$i", i))))

  // testing the mapmergemonoid
  val m1 = Map("o1" -> Map("i1" -> 1, "i2" -> 2))
  val m2 = Map("o1" -> Map("i2" -> 3))
  println(chapter10.Monoid.M.op(m1, m2))

  // testing bag
  println("\n##### TESTING BAG #####")
  val x123 = Vector(1,2,3,5,6,3,1,2,3,4,7)
  val y123 = Vector("a", "rose", "is", "a", "rose")
  println(chapter10.Monoid.bag(x123))
  println(chapter10.Monoid.bag(y123))
  println(chapter10.Monoid.bagFold(y123))







