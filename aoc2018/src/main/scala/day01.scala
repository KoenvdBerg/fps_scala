import scala.annotation.tailrec
import scala.io.*

/**
 * PART 1:
 *
 * To solve, parse the input to a list of integers and then sum the list.
 *
 * PART 2:
 *
 * To solve, keep a state while recursively summing the frequencies. The code uses a Set to determine if the
 * state has already been seen before. If so, it returns that state and that is the answer for part 2.
 *
 */

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

  /**
   * Loops over input and finds the first state that's encountered twice.
   */
  def firstTwice(is: List[Int]): Int =
    @tailrec
    def go(iss: List[Int], seen: Set[Int], z: Int): Int = iss match
      case h :: t =>
        val state: Int = z + h               // keep track of the state
        if seen.contains(state) then state   // exit condition
        else
          go(t, seen + state, state)         // continue, add state to set
      case Nil => go(is, seen, z)            // input is exhausted, start again with new state

    go(is, Set.empty[Int], 0)

  val answer2 = firstTwice(input)

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")