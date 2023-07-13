package summer_school

/**
 * Help can be found here: 
 * 
 * https://github.com/nmcb/fpa-course-intro/blob/master/fpa/src/main/haskell/afp.hs
 * 
 * 
 */


object AFPLab2: 
  
  trait Functor[F[_]]: 
    
    def fmap[A, B](fa: F[A])(f: A => B): F[B]
    extension [A](fa: F[A]) 
      def map[B](f: A => B): F[B] = fmap(fa)(f)
    
  trait Applicative[F[_]](using val functor: Functor[F]):
    def pure[A](a: A): F[A]
    def ap[A, B](fa: F[A])(gf: F[A => B]): F[B]
    
    extension [A](fa: F[A])
      def <*>[B](gf: F[A => B]): F[B] = ap(fa)(gf)
      def map2[B, C](fb: F[B])(f: (A, B) => C): F[C] = fb <*> fa.map(f.curried)

  trait Monad[F[_]](using val applicative: Applicative[F]):
    def unit[A](a: A): F[A] = applicative.pure(a)
    def bind[A, B](fa: F[A])(f: A => F[B]): F[B]
    extension [A](fa: F[A])
      def flatMap[B](f: A => F[B]): F[B] = bind(fa)(f)
      
  trait Monoid[A]: 
    def mzero: A
    def mappend(a1: A, a2: A): A

  trait MonoidBox[F[_]]:
    def empty[A]: F[A]
    def append[A](l: F[A])(r: F[A]): F[A]
      
  trait Foldable[F[_]]: 
    def foldMap[A, B](fa: F[A])(f: A => B)(using monoid: Monoid[B]): B
      
  trait Traversable[F[_]](using val functor: Functor[F], val foldable: Foldable[F]):
    def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(using applicative: Applicative[G]): G[F[B]]
    
  enum Tree[+A]: 
    case Leaf(a: A)
    case Node(l: Tree[A], r: Tree[A])
    
  object Tree: 
    
    given Functor[Tree] with
      override def fmap[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match
        case Leaf(a) => Leaf(f(a))
        case Node(l, r) => Node(fmap(l)(f), fmap(r)(f))

          
    given Applicative[Tree] with
      override def pure[A](a: A): Tree[A] = Leaf(a)
      override def ap[A, B](fa: Tree[A])(gf: Tree[A => B]): Tree[B] = gf match
        case Leaf(f) => fa.map(f)
        case Node(l, r) => Node(ap(fa)(l), ap(fa)(r))

      
    given Monad[Tree] with
      override def bind[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match
        case Leaf(v)    => f(v)
        case Node(l, r) => Node(bind(l)(f), bind(r)(f))

    given Monoid[String] with
      override def mappend(a1: String, a2: String): String = a1 + a2
      override def mzero: String = ""

    given MonoidBox[List] with
      def empty[A]: List[A] =
        Nil
      def append[A](l: List[A])(r: List[A]): List[A] =
        l match
          case h :: t => h :: append(t)(r) 
          case Nil => r
            
    given Foldable[Tree] with
      override def foldMap[A, B](fa: Tree[A])(f: A => B)(using monoid: Monoid[B]): B = fa match 
        case Leaf(v)    => f(v)
        case Node(l, r) => monoid.mappend(foldMap(l)(f), foldMap(r)(f))

    given Traversable[Tree] with
      override def traverse[G[_], A, B](fa: Tree[A])(f: A => G[B])(using applicative: Applicative[G]): G[Tree[B]] = 
        import applicative.functor.*
        fa match
          case Leaf(v)    => f(v).map(Leaf.apply[B])
          case Node(l, r) => traverse(l)(f).map2(traverse(r)(f))((a, b) => Node(a, b))




  enum RoseTree[+A]: 
    case RoseNode(v: A, vs: List[RoseTree[A]])
    case RoseLeaf

  object RoseTree: 

    given Functor[RoseTree] with
      override def fmap[A, B](fa: RoseTree[A])(f: A => B): RoseTree[B] = fa match
        case RoseNode(v, vs) => RoseNode(f(v), vs.map((rt: RoseTree[A]) => fmap(rt)(f)))
        case RoseLeaf => RoseLeaf

    given Applicative[RoseTree] with
      override def pure[A](a: A): RoseTree[A] = RoseNode(a, Nil)

      override def ap[A, B](fa: RoseTree[A])(gf: RoseTree[A => B]): RoseTree[B] = gf match 
        case RoseLeaf => RoseLeaf
        case RoseNode(g, gs) => fa match
          case RoseLeaf => RoseLeaf
          case RoseNode(v, vs) =>
            RoseNode(g(v), gs.zip(vs).map((gfs: RoseTree[A => B], vss: RoseTree[A]) => ap(vss)(gfs)))

    given Monad[RoseTree] with
      override def bind[A, B](fa: RoseTree[A])(f: A => RoseTree[B]): RoseTree[B] = fa match         
        case RoseLeaf => RoseLeaf
        case RoseNode(v1, vs1) => f(v1) match
          case RoseLeaf          => RoseLeaf
          case RoseNode(v2, vs2) => RoseNode(v2, vs2 ++ vs1.map((rt: RoseTree[A]) => bind(rt)(f)))

    given Foldable[RoseTree] with
      override def foldMap[A, B](fa: RoseTree[A])(f: A => B)(using monoid: Monoid[B]): B = fa match
        case RoseLeaf => monoid.mzero
        case RoseNode(v, vs) => monoid.mappend(f(v),
          vs.foldLeft(monoid.mzero)((b: B, rt: RoseTree[A]) => monoid.mappend(b, foldMap(rt)(f))))


@main def tree_data: Unit =
  import AFPLab2.RoseTree.*
  import AFPLab2.{RoseTree, Tree}
  import AFPLab2.Tree.*
  import AFPLab2.Tree.given_Monoid_String

  // TREE
  val t = Tree.Node(Tree.Leaf(1), Tree.Leaf(3))
  val g: Tree[Int => Int] = Tree.Leaf((i: Int) => i * i)
  println("######### TREE DATA ##########")
  println(s"The Tree is: $t")
  println(s"Functor: ${t.map((i: Int) => (i + 10) * i)}")
  println(s"Applicative: ${t <*> g}")
  println(s"Monad: ${t.flatMap((i: Int) => Tree.Node(Tree.Leaf(i), Tree.Leaf(i*i)))}")
  println(s"Foldable: ${Tree.given_Foldable_Tree.foldMap(t)((i: Int) => s"${i*i} and ")}")



  // ROSETREE
  val r1: RoseTree[Int] = RoseNode(4, List(RoseNode(3, List(RoseNode(6, Nil))), RoseNode(99, Nil)))
  val rf: RoseTree[Int => Int] = r1.map((x: Int) => (i: Int) => (i + 10) * (x - 2))
  println("\n######### ROSETREE DATA ##########")
  println(s"The RoseTree is: $r1")
  println(s"Functor: ${r1.map((i: Int) => i * 2)}")
  println(s"Applicative: ${r1 <*> rf}")
  println(s"Monad: ${r1.flatMap((i: Int) => RoseTree.RoseNode(i, List(RoseNode(i+i, Nil))))}")
  println(s"Foldable: ${RoseTree.given_Foldable_RoseTree.foldMap(r1)((i: Int) => s"${i*i} kljf lksj")}")


  // MONOID BOX
  import AFPLab2.Tree.given_MonoidBox_List.*
  val y = List(1,2,3,4,5)
  val z = List(6,7,8,9)
  println("MONOIDBOX: " + append(y)(z))
