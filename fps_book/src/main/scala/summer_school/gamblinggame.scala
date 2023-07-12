package summer_school

import summer_school.gambling.MonadGamble.{Coin, Outcome}
import summer_school.AFPLab2.{Applicative, Functor, Monad, Monoid}
import cats.data.State
import summer_school.gambling.MonadGamble


/**
 * 
 * Answers to the questions of Advanced Functional Programming
 * 
 * https://uu-afp.github.io/as2.html#monads-for-a-gambling-game
 * 
 * Monads for a gambling game 
 */

object gambling:

  enum DecisionTree[+A]: 
    case Result(value: A)
    case Decision(ds: List[DecisionTree[A]])

  object DecisionTree:

    def treeToList[A](dt: DecisionTree[A]): List[A] = dt match
      case Result(v)    => List(v)
      case Decision(vs) => vs.flatMap((d: DecisionTree[A]) => treeToList(d)) 
    def pure[A](a: A): DecisionTree[A] = Result(a)

    def sequence[A](dtl: List[DecisionTree[A]]): DecisionTree[List[A]] = dtl match
      case h :: t => h.flatMap((a: A) => sequence(t).map((b: List[A]) => a :: b))
      case Nil => Result(Nil)

    given Functor[DecisionTree] with
      override def fmap[A, B](fa: DecisionTree[A])(f: A => B): DecisionTree[B] = fa match
        case Result(v)    => Result(f(v))
        case Decision(ds) => Decision(ds.map((t: DecisionTree[A]) => fmap(t)(f)))

    given Applicative[DecisionTree] with
      override def pure[A](a: A): DecisionTree[A] = Result(a)

      override def ap[A, B](fa: DecisionTree[A])(gf: DecisionTree[A => B]): DecisionTree[B] = gf match
        case Result(v)    => fa.map(v)
        case Decision(ds) => fa match
          case Result(v)    => Decision(ds.map((f: DecisionTree[A => B]) => ap(fa)(f)))  
          case Decision(vs) => Decision(ds.zip(vs).map((gs: DecisionTree[A => B], vss: DecisionTree[A]) => ap(vss)(gs)))

    given Monad[DecisionTree] with
      override def unit[A](a: A): DecisionTree[A] = given_Applicative_DecisionTree.pure(a)
      override def bind[A, B](fa: DecisionTree[A])(f: A => DecisionTree[B]): DecisionTree[B] = fa match
        case Result(v)    => f(v)
        case Decision(ds) => Decision(ds.map(flatMap(_)(f)))



  object MonadGamble:

    type MonadGamble[A] = State[RNG.Seed, A]

    enum Coin:
      case H, T

    enum Outcome:
      case Win, Lose

    extension[A] (fa: MonadGamble[A])
      def unit(a: A): MonadGamble[A] = State.pure(a)
      def flatMap[B](f: A => MonadGamble[B]): MonadGamble[B] = fa.flatMap(f)
      def map[B](f: A => B): MonadGamble[B] = fa.map(f)
    def toss: MonadGamble[Coin] = RNG.Seed.choose(0,2).map((i: Int) => if i == 0 then Coin.H else Coin.T)
    def roll: MonadGamble[Int] = RNG.Seed.choose(1, 7)
    def tossN(n: Int): MonadGamble[List[Coin]] = RNG.Seed.chooseM(0,2, n)
      .map((i: List[Int]) => i.map((x: Int) => if x == 0 then Coin.H else Coin.T))

    def tossT: DecisionTree[Coin] = DecisionTree.Decision(List(DecisionTree.Result(Coin.H), (DecisionTree.Result(Coin.T))))
    def tossTN(n: Int): DecisionTree[List[Coin]] = DecisionTree.sequence(List.fill(n)(tossT))
    def rollT: DecisionTree[Int] = DecisionTree.Decision((1 until 6).toList.map(DecisionTree.Result.apply))

    def gamblingGame: MonadGamble[Outcome] = for {
        tosses <- tossN(6)
        roll <- roll
        res <- if roll >= tosses.count(_ == Coin.H) then State.pure(Outcome.Win) else State.pure(Outcome.Lose)
      } yield res

    def gamblingTree: DecisionTree[Outcome] = for {
      tosses <- tossTN(6)
      roll   <- rollT
      res    <- if roll >= tosses.count(_ == Coin.H) then DecisionTree.pure(Outcome.Win) else DecisionTree.pure(Outcome.Lose)
    } yield res

    def probabilityOfWinning(dt: DecisionTree[Outcome]): Double =
      val outcomes = DecisionTree.treeToList(dt)
      outcomes.count(_ == Outcome.Win) / outcomes.length.toDouble



@main def gamblingGame: Unit =

  import summer_school.RNG.*
  import gambling.MonadGamble
  import gambling.DecisionTree

  val x: Seed = Seed(10L)

  val game: MonadGamble.MonadGamble[Outcome] = MonadGamble.gamblingGame   
  val exampleOutcome = game.run(x).value._2
  val allOutcomes: DecisionTree[Outcome] = MonadGamble.gamblingTree
  val winningChance: Double = MonadGamble.probabilityOfWinning(allOutcomes)
  println(s"The chance of winning in this game is: $winningChance")