package summer_school

import summer_school.AFPLab2.Monoid

object InstrumentedState: 
  
  case class Counts(binds: Int, returns: Int, gets: Int, puts: Int)
  
  val countMonoid: Monoid[Counts] = new Monoid[Counts]:
    override def mzero: Counts = Counts(0,0,0,0)
    override def mappend(a1: Counts, a2: Counts): Counts = 
      Counts(a1.binds + a2.binds, a1.returns + a2.returns, a1.gets + a2.gets, a1.puts + a2.puts)
      
  val oneBind: Counts   = Counts(1,0,0,0)
  val oneReturn: Counts = Counts(0,1,0,0)
  val oneGet: Counts    = Counts(0,0,1,0)
  val onePut: Counts    = Counts(0,0,0,1)
  
  type StateC[S, +A] = (S, Counts) => (A, S, Counts)
  
  def unit[S, A](a: A): StateC[S, A] = (s: S, c: Counts) => (a, s, countMonoid.mappend(c, oneReturn))
  
  extension [S, A](sa: StateC[S, A])
    def flatMap[B](f: A => StateC[S, B]): StateC[S, B] = (s: S, c: Counts) => 
      val (a1, s1, c1) = sa(s, c)
      f(a1)(s1, countMonoid.mappend(c1, oneBind))
    def map[B](f: A => B): StateC[S, B] = sa.flatMap((a: A) => unit(f(a)))
    def modify(f: S => S): StateC[S, Unit] = for {
      s <- get
      _ <- set(f(s))
    } yield ()
    def get: StateC[S, S] = (s: S, c: Counts) => (s, s, countMonoid.mappend(c, oneGet))
    def set(s: S): StateC[S, Unit] = (_, c: Counts) => ((), s, countMonoid.mappend(c, onePut))
    def run(in: S): (A, S, Counts) = sa(in, countMonoid.mzero)
    
object labelTree: 
  
  enum Tree[+A]:
    case Branch(l: Tree[A], v: A, r: Tree[A])
    case Leaf
    
  object Tree:
    import InstrumentedState.*
    def label[A](si: StateC[Int, A], t: Tree[A]): StateC[Int, Tree[(Int, A)]] = t match
      case Leaf => InstrumentedState.unit(Leaf)
      case Branch(l, v, r) => for {
        ll <- label(si, l)
        i  <- si.get
        _  <- si.set(i + 1)
        rr <- label(si, r)
      } yield Branch(ll, (i, v), rr)
  
@main def runLabelTree: Unit =
  import labelTree.Tree.*
  import InstrumentedState.*
  
  val tree = Branch(Branch(Leaf, "B", Leaf), "A", Leaf)
  println(label(unit(""), tree).run(42))
  
