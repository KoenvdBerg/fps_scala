object chapter06_state:
  case class State[S, +A](run: S => (A, S)):
    def flatMap[B](f: A => State[S, B]): State[S, B] = State.flatMap(this)(f)
    def map[B](f: A => B): State[S, B] = State.map(this)(f)
    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State.map2(this, sb)(f)
    def modify[S](f: S => S): State[S, Unit] = for {
      s <- get
      _ <- set(f(s))
    } yield ()
    def get[S]: State[S, S] = State.get
    def set[S](s: S): State[S, Unit] = State.set(s)
  
  
  object State:
  
    def unit[S, A](a: A): State[S, A] = State((s: S) => (a, s))

    def flatMap[S, A, B](sa: State[S, A])(f: A => State[S, B]): State[S, B] =
      State((s: S) => {
        val (a, s2) = sa.run(s)
        f(a).run(s2)
      })

    def map[S, A, B](sa: State[S, A])(f: A => B): State[S, B] =
      flatMap(sa)(a => State.unit(f(a)))

    def map2[S, A, B, C](sa: State[S, A], sb: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap(sa)(a => sb.map(b => f(a, b)))
    
    def get[S]: State[S, S] = State(s => (s, s))

    def set[S](s: S): State[S, Unit] = State(_ => ((), s))
    
    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = fs match
      case h :: t => h.map2(sequence(t))(_ :: _)
      case Nil => State((s: S) => (Nil, s))
  
    import Tree.*
    def sequenceTree[S, A](fs: Tree[State[S, A]]): State[S, Tree[A]] = fs match
      case Branch(left, right) => sequenceTree(left).map2(sequenceTree(right))(Branch(_, _))
      case Leaf(v) => State((s: S) =>
        val (nextv, nexts): (A, S) = v.run(s)
        (Leaf(nextv), nexts))

object Candy:
  import chapter06_state.State

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int):

    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
      val startState = State(i => (i, this))  //(this.coins, this.candies)
      for {
        _ <- State.sequence(inputs.map((i: Input) => startState.modify(update(i))))
        s <- startState.get
      } yield (s.candies, s.coins)

    def update = (i: Input) => (s: Machine) =>
      println(s"$i, $s")
      (i, s) match
        case (Coin, s) if s.candies > 0 => Machine(true, candies, coins + 1)
        case (Turn, s) if s.locked && s.candies > 0 => Machine(false, candies - 1, coins)
        case _ => s

@main def run_chapter06_candymachine(): Unit =
  import Candy.{Coin, Turn, Machine, Input}
  import chapter06_state.State
  val input: List[Input] = List(Coin, Coin, Coin, Turn, Turn, Coin)
  val m = Machine(true, 5, 10)
  println(m.update(Coin)(m))

  val res: State[Machine, (Int, Int)] = m.simulateMachine(input)
  println(res.run(m))

