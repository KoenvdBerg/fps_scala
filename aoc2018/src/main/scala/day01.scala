import scala.io.*

object day01 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: List[Int] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .map(s => s.toInt)
      .toList

  private val answer1: Int = input.sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  def firstTwiceFast(is: List[Int]): Int =
    def go(iss: List[Int], acc: Set[Int], z: Int): Int = iss match
      case h :: t =>
        val state = z + h
        if acc.contains(state) then state
        else
          go(t, acc ++ Set(state), state)
      case Nil => go(is, acc, z)

    go(is, Set.empty[Int], 0)

  val answer2 = firstTwiceFast(input)

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]") // 83173