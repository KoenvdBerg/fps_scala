package summer_school

import summer_school.AFPLab2.RoseTree.{RoseLeaf, RoseNode}
import summer_school.AFPLab2.Tree.{Leaf, Node}
import summer_school.AFPLab2.{RoseTree, Tree}

/**
 * Help can be found here: 
 * 
 * https://github.com/nmcb/fpa-course-intro/blob/master/fpa/src/main/haskell/afp.hs
 * 
 * 
 */




object AFPLab2: 
  
  trait Functor[F[_]]: 
    extension [A](fa: F[A]) 
      def fmap[B](f: A => B): F[B]
    
  trait Applicative[F[_]](using functor: Functor[F]):
    def map[A, B](fa: F[A])(f: A => B): F[B] = fa.fmap(f)
    extension [A](fa: F[A]) 
      def pure(a: A): F[A]
      def <*>[B](gf: F[A => B]): F[B]
      def map2[B, C](fb: F[B])(f: (A, B) => C): F[C] = fb <*> fa.fmap(f.curried)

  trait Monad[F[_]](using App: Applicative[F]): 
    extension [A](fa: F[A])
      def unit(a: A): F[A]
      def flatMap[B](f: A => F[B]): F[B]
      
  trait Monoid[A]: 
    def mzero: A
    def mappend(a1: A, a2: A): A
      
  trait Foldable[F[_]]: 
    extension [A](fa: F[A])
      def foldMap[B](m: Monoid[B])(f: A => B): B
      
  trait Traversable[F[_]]:
    extension [A](fa: F[A]) 
      def traverse[G[_]: Applicative, B](f: A => G[B]): G[F[B]]
    
  enum Tree[+A]: 
    case Leaf(a: A)
    case Node(l: Tree[A], r: Tree[A])
    
  object Tree: 
    
    given Functor[Tree] with
      extension [A](fa: Tree[A])  
        def fmap[B](f: A => B): Tree[B] = fa match
          case Leaf(a)    => Leaf(f(a))
          case Node(l, r) => Node(l.fmap(f), r.fmap(f))
          
    given Applicative[Tree] with
      extension [A](fa: Tree[A]) 
        def pure(a: A): Tree[A] = Leaf(a)
        def <*>[B](gf: Tree[A => B]): Tree[B] = gf match
          case Leaf(f)    => fa.fmap(f)
          case Node(l, r) => Node(fa <*> l, fa <*> r)

    given Monad[Tree] with 
      extension [A](fa: Tree[A])
        def unit(a: A): Tree[A] = Leaf(a)
        def flatMap[B](f: A => Tree[B]): Tree[B] = fa match
          case Leaf(v)    => f(v)
          case Node(l, r) => Node(l.flatMap(f), r.flatMap(f))

    val stringMonoid: Monoid[String] = new Monoid[String]:
      override def mappend(a1: String, a2: String): String = a1 + a2
      override def mzero: String = ""
    given Foldable[Tree] with 
      extension [A](fa: Tree[A])
        def foldMap[B](m: Monoid[B])(f: A => B): B = fa match
          case Leaf(v)    => f(v)
          case Node(l, r) => m.mappend(l.foldMap(m)(f), r.foldMap(m)(f))
    
    // cannot find the reason why below doesn't compile
    //given Traversable[Tree] with
    //  extension [A](fa: Tree[A]) 
    //    def traverse[G[_]: Applicative, B](f: A => G[B])(using g: Applicative[G]): G[Tree[B]] = fa match
    //      case Leaf(v)    => f(v).fmap(Leaf.apply)
    //      case Node(l, r) => Node(l.traverse(f), r.traverse(f))
          
        
        
    
  enum RoseTree[+A]: 
    case RoseNode(v: A, vs: List[RoseTree[A]])
    case RoseLeaf
    
  object RoseTree: 
    
    given Functor[RoseTree] with 
      extension [A](fa: RoseTree[A]) 
        def fmap[B](f: A => B): RoseTree[B] = fa match
          case RoseNode(v, vs) => RoseNode(f(v), vs.map(_.fmap(f)))
          case RoseLeaf => RoseLeaf
          
    given Applicative[RoseTree] with 
      extension [A](fa: RoseTree[A]) 
        def pure(a: A): RoseTree[A] = RoseNode(a, Nil)
        def <*>[B](gf: RoseTree[A => B]): RoseTree[B] = gf match
          case RoseLeaf => RoseLeaf
          case RoseNode(g, gs) => fa match
            case RoseLeaf => RoseLeaf
            case RoseNode(v, vs) => 
              RoseNode(g(v), gs.zip(vs).map((gfs: RoseTree[A => B], vss: RoseTree[A]) => vss <*> gfs))

    given Monad[RoseTree] with
      extension [A](fa: RoseTree[A])
        def unit(a: A): RoseTree[A] = RoseNode(a, Nil)
        def flatMap[B](f: A => RoseTree[B]): RoseTree[B] = fa match
          case RoseLeaf => RoseLeaf
          case RoseNode(v1, vs1) => f(v1) match
            case RoseLeaf          => RoseLeaf
            case RoseNode(v2, vs2) => RoseNode(v2, vs2 ++ vs1.map(_.flatMap(f)))
            
    given Foldable[RoseTree] with
      extension [A](fa: RoseTree[A])
        def foldMap[B](m: Monoid[B])(f: A => B): B = fa match
          case RoseLeaf => m.mzero
          case RoseNode(v, vs) => m.mappend(f(v), 
            vs.foldLeft(m.mzero)((b: B, rt: RoseTree[A]) => m.mappend(b, rt.foldMap(m)(f))))

    
@main def tree_data: Unit =
  import AFPLab2.RoseTree.*
  import AFPLab2.{RoseTree, Tree}
  
  val t = Tree.Node(Tree.Leaf(1), Tree.Leaf(3))
  val g: Tree[Int => Int] = Tree.Leaf((i: Int) => i * i)
  println("######### TREE DATA ##########")
  println(s"The Tree is: $t")
  println(s"Functor: ${t.fmap((i: Int) => (i + 10) * i)}")
  println(s"Applicative: ${t <*> g}")
  println(s"Monad: ${t.flatMap((i: Int) => Tree.Node(Tree.Leaf(i), Tree.Leaf(i*i)))}")
  println(s"Foldable: ${t.foldMap(Tree.stringMonoid)((i: Int) => s"${i*i} and ")}")
  
  
  
  // ROSETREE
  val r1: RoseTree[Int] = RoseNode(4, List(RoseNode(3, List(RoseNode(6, Nil))), RoseNode(99, Nil)))
  val rf: RoseTree[Int => Int] = r1.fmap((x: Int) => (i: Int) => (i + 10) * (x - 2))
  println("\n######### ROSETREE DATA ##########")
  println(s"The RoseTree is: $r1")
  println(s"Functor: ${r1.fmap((i: Int) => i * 2)}")
  println(s"Applicative: ${r1 <*> rf}")
  println(s"Monad: ${r1.flatMap((i: Int) => RoseTree.RoseNode(i, List(RoseNode(i+i, Nil))))}")
  println(s"Foldable: ${r1.foldMap(Tree.stringMonoid)((i: Int) => s"${i*i} kljf lksj")}")

