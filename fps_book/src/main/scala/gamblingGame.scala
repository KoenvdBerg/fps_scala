import chapter06_state.State

object gambling:

  type MonadGamble[A] = State[Int, A]

  object MonadGamble:
    enum Coin:
      case H, T

    enum Dice:
      case D1, D2, D3, D4, D5, D6

    enum Outcome:
      case Win, Lose

    val diceRolls: Map[Dice, Int] = Map(Dice.D1 -> 1, Dice.D2 -> 2, Dice.D3 -> 3,
      Dice.D4 -> 4, Dice.D5 -> 5, Dice.D6 -> 6)


    extension[A] (fa: MonadGamble[A])
      def unit(a: A): MonadGamble[A] = State.unit(a)
      def flatMap[B](f: A => MonadGamble[B]): MonadGamble[B] = fa.flatMap(f)
      def map[B](f: A => B): MonadGamble[B] = fa.map(f)
      def toss: MonadGamble[Coin] = ???
      def roll: MonadGamble[Dice] = ???
      def tossN(n: Int): MonadGamble[List[Coin]] = ???


    def game[A](m: MonadGamble[A], in: Int): Outcome = 
      
      val program: State[Int, Outcome] = for {
        tosses <- m.tossN(6)
        roll <- m.roll
        res <- if diceRolls(roll) >= tosses.count(_ == Coin.H) then State.unit(Outcome.Win) else State.unit(Outcome.Lose)
      } yield res
      program.run(in)._1