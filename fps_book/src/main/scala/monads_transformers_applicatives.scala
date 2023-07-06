import AFPLab2.RoseTree.RoseLeaf

/**
 * Help can be found here: 
 * 
 * https://github.com/nmcb/fpa-course-intro/blob/master/fpa/src/main/haskell/afp.hs
 * 
 * 
 */




object AFPLab2: 
  
  sealed trait Functor[F[_]]: 
    extension [A](fa: F[A]) 
      def fmap[B](f: A => B): F[B]
    
  sealed trait Applicative[F[_]](using functor: Functor[F]):
    extension [A](fa: F[A]) 
      def pure(a: A): F[A]
      //def map2[B, C](fb: F[B])(f: (A, B) => C): F[C]  fa.map2(gf)((a: A, ga: A => B) => ga(a))
      def <*>[B](gf: F[A => B]): F[B]
    
  sealed trait Monad[F[_]](using App: Applicative[F]): 
    extension [A](fa: F[A])
      def unit(a: A): F[A]
      def flatMap[B](f: A => F[B]): F[B]
    
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

    
@main def ksdjf: Unit =
  import AFPLab2.Tree
  import AFPLab2.RoseTree.*
  import AFPLab2.RoseTree
  
  val t = Tree.Node(Tree.Leaf(1), Tree.Leaf(3))
  val g: Tree[Int => Int] = Tree.Leaf((i: Int) => i * i)
  println(t)
  println(t <*> g)
  
  
  
  // ROSETREE
  val r1: RoseTree[Int] = RoseNode(4, List(RoseNode(3, List(RoseNode(6, Nil)))))
  println(r1.fmap((i: Int) => i * 2))


//  def map2[B, C](fb: Tree[B])(f: (A, B) => C): Tree[C] = fa match
//    case Node(l, r) => Node(l.map2(fb)(f), r.map2(fb)(f))
//    case Leaf(va) => fb match
//      case Node(l, r) => Node(fa.map2(l)(f), fa.map2(r)(f))
//      case Leaf(vb) => Leaf(f(va, vb))