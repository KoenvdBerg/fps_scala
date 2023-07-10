package summer_school

import cats.data.State


object RNG:

  final case class Seed(long: Long):
    def next: Seed = Seed(long * 6364136223846793005L + 1442695040888963407L)

  object Seed:

    def nextLong: State[Seed, Long] = State((seed: Seed) => (seed.next, seed.long))

    def nonNegativeLong: State[Seed, Long] = for {
      n <- nextLong
    } yield if n == Long.MinValue then math.abs(Long.MinValue - 1)
    else math.abs(n)

    def choose(start: Int, stopExclusive: Int): State[Seed, Int] = for {
      n <- nonNegativeLong
    } yield (start + n % (stopExclusive - start)).toInt

    def sequence[A](fs: List[State[Seed, A]]): State[Seed, List[A]] = fs match
      case h :: t => h.flatMap((a: A) => sequence(t).map((b: List[A]) => a :: b))
      case Nil => State.pure(Nil)

    def chooseM(start: Int, stopExclusive: Int, n: Int): State[Seed, List[Int]] =
      sequence(List.fill(n)(choose(start, stopExclusive)))
      
        
