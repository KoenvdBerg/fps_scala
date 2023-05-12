sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // 3.25
  def size[A](ta: Tree[A]): Int = ta match
    case Branch(l, r) => 1 + size(r) + size(l)
    case Leaf(_) => 1

  // 3.26
  def maximum(ta: Tree[Int]): Int = ta match
    case Branch(l, r) => maximum(l).max(maximum(r))
    case Leaf(v) => v

  //3.27
  def depth[A](ta: Tree[A]): Int = ta match
    case Branch(l, r) => (depth(l)+1).max(depth(r)+1)
    case Leaf(_) => 0

  // 3.28
  def map[A, B](ta: Tree[A])(f: A => B): Tree[B] = ta match
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    case Leaf(v) => Leaf(f(v))

  // 3.29
  def fold[A, B](ta: Tree[A])(f: (B, B) => B)(g: A => B): B = ta match
    case Leaf(v) => g(v)
    case Branch(l, r) => f(fold(l)(f)(g), fold(r)(f)(g))

  def sizef[A](ta: Tree[A]): Int =
    fold[A, Int](ta)((a, b) => a + b + 1)(_ => 1)

  def maximumf(ta: Tree[Int]): Int =
    fold[Int, Int](ta)((a, b) => a.max(b))(a => a)

  def depthf[A](ta: Tree[A]): Int =
    fold[A, Int](ta)((a, b) => (a+1).max(b+1))(_ => 0)

  def mapf[A, B](ta: Tree[A])(f: A => B): Tree[B] =
    fold[A, Tree[B]](ta)((a, b) => Branch(a, b))(a => Leaf(f(a)))

  def fill[A](size: Int)(elem: => A): Tree[A] =
    def go(acc: Tree[A]): Tree[A] =
      if Tree.depthf(acc) >= size then acc
      else acc match
        case Leaf(_) => go(Branch(Leaf(elem), Leaf(elem)))
        case t@Branch(_, _) => go(Branch(t, t))
    go(Leaf(elem))


}


@main def run_chapter03_tree(): Unit =
  println("### 3.25 ###")
  val test = Branch(Leaf("joost"), Branch(Branch(Leaf("bar"), Leaf("baz")), Leaf("foobar")))
  println(Tree.size(test))

  println("### 3.26 ###")
  println(Tree.maximum(Branch(Leaf(1), Branch(Branch(Leaf(200), Leaf(3)), Leaf(4)))))

  println("### 3.27 ###")
  println(Tree.depth(Branch(Leaf(1), Branch(Branch(Leaf(200), Leaf(3)), Branch(Leaf(6), Branch(Leaf(9), Leaf(10)))))))

  println("### 3.28 ###")
  println(Tree.map(Branch(Leaf(1), Branch(Branch(Leaf(200), Leaf(3)), Branch(Leaf(6), Branch(Leaf(9), Leaf(10))))))(_ + 1))
  println(Tree.map(Branch(Leaf("joost"), Branch(Branch(Leaf("bar"), Leaf("baz")), Leaf("foobar"))))(f => s"hello: ${f}"))

  println("### 3.29 ###")
  val testtree = Branch(Leaf(1), Branch(Branch(Leaf(200), Leaf(3)), Leaf(4)))
  println(Tree.sizef(testtree))
  println(Tree.maximumf(testtree))
  println(Tree.depthf(testtree))
  println(Tree.mapf(testtree)(f => f * 2))

  val tfill: Tree[Int] = Tree.fill(4)(4)
  println(tfill)


