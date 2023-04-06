sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // 3.25
  def size[A](ta: Tree[A]): Int = ta match
    case Branch(l, r) => 1 + size(r) + size(l)
    case Leaf(_) => 1

  // 3.26
//  def maximum(ta: Tree[Int]): Int =
//    def go(tta: Tree[Int])
}

@main def runtree(): Unit =
  val test = Branch(Leaf("joost"), Branch(Branch(Leaf("bar"), Leaf("baz")), Leaf("foobar")))
  println(Tree.size(test))