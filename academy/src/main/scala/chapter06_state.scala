package state

case class State[S, +A](run: S => (A, S)):

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State((s: S) => {
      val (a, s2) = run(s)
      f(a).run(s2)
    })

  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))


object State:

  def unit[S, A](a: A): State[S, A] = State((s: S) => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = fs match
    case h :: t => h.map2(sequence(t))(_ :: _)
    case Nil => State((s: S) => (Nil, s))


object Candy:

  import state.*

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
  val input: List[Input] = List(Coin, Coin, Coin, Turn, Turn, Coin)
  val m = Machine(true, 5, 10)
  println(m.update(Coin)(m))

  val res: State[Machine, (Int, Int)] = m.simulateMachine(input)
  println(res.run(m))

