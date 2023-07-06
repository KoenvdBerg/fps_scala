object AFPLab1: 
  
  enum PermTree[+A]: 
    case Node(l: List[(A, PermTree[A])])
    case Leaf
    
  object PermTree: 
    
    def split[A](xs: List[A]): List[(A, List[A])] = xs match
      case Nil    => Nil
      case h :: t => (h, t) :: (for {
        (y, ys) <- split(t)
      } yield (y, h::ys))
      
    def perms[A](xs: List[A]): List[List[A]] = xs match
      case Nil => List(List.empty)
      case x   => for {
        (v, vs) <- split(x)
        p       <- perms(vs)
      } yield v :: p
      
    def listToPermTree[A](xs: List[A]): PermTree[A] = xs match
      case Nil => Leaf
      case x   => Node( for {
        (v, vs) <- split(x)
        p = listToPermTree(vs)
      } yield (v, p))

    def listToPermTreePrune[A](xs: List[A], a: A, f: (A, A) => Boolean): PermTree[A] = xs match
      case Nil => Leaf
      case x => Node(for {
        (v, vs) <- split(x)
        p = listToPermTreePrune(vs, v, f)
        if f(a, v)
      } yield (v, p))
      
    def permTreeToPerms[A](t: PermTree[A]): List[List[A]] = t match
      case Leaf      => List(List.empty)
      case Node(Nil) => List.empty
      case Node(l)   => l.flatMap((a: A, ta: PermTree[A]) => permTreeToPerms(ta).map(a :: _))
      
    def pruneNode[A](n: PermTree[A], f: (A, A) => Boolean, a1: A): PermTree[A] = n match
      case Node(l) if l.nonEmpty => Node(l.flatMap { (a2: A, nt: PermTree[A]) =>
        if f(a1, a2) then List((a2, pruneNode(nt, f, a2))) else Nil
      })
      case x => x
        
    def prune[A](t: PermTree[A], f: (A, A) => Boolean): PermTree[A] = t match
      case Node(l) => Node(for {
        (a1, nt) <- l
      } yield (a1, pruneNode(nt, f, a1)))
      
    def smoothPerms(d: Int, is: List[Int]): List[List[Int]] =
      val t = listToPermTree(is)
      permTreeToPerms(prune(t, (x: Int, y: Int) => math.abs(x - y) < d))

    def smoothPermsQuick(d: Int, is: List[Int]): List[List[Int]] =
      val t: PermTree[Int] = Node( for {
        (v, vs) <- split(is)
      } yield (v, listToPermTreePrune(vs, v, (x: Int, y: Int) => math.abs(x - y) < d)))
      permTreeToPerms(t)
      
@main def PermutationTree: Unit =
  import AFPLab1.PermTree
  import AFPLab1.PermTree.{Node, Leaf}

  val x: List[Int] = List(1,40,3,60,5,16,7,18,9, 10)


  val start1: Long = System.currentTimeMillis
  val res1 = PermTree.smoothPerms(7, x)
  println(s"[${System.currentTimeMillis - start1}ms]")
  
  val start2: Long = System.currentTimeMillis
  val res2 = PermTree.smoothPermsQuick(7, x)
  println(s"[${System.currentTimeMillis - start2}ms]")
  

  