// IDEA: convert Char to Int and then scanRight for diff is abs 26 (or smth)
// if HIT --> recur

import scala.io.*
import math.*
import scala.annotation.tailrec


/**
 *
 * PART 1:
 *
 * The solution to part consisted of two functions:
 * - function that finds a pair
 * - function that continues finding and removing pairs until no pair can be found anymore
 *
 * The combined use of these two functions yielded the result. Important to note to convert the characters to Integers.
 * Then the absolute difference between e.g. A and a or t and T is always 32. This property can be used to quickly
 * find polymers.
 *
 * PART 2:
 *
 * This consisted of running the polymer against the entire alphabet, and then computing for every result
 * the length of the left-over polymer (after destroying that alphabet pair first). The minimum value was the solution.
 *
 */

object day05 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: String =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList
      .head

  /**
   * This function finds the first hit in a string given a comparison predicate. Is used here to find the first
   * pair.
   */
  def findFirstPair(s: String)(f: (Char, Char) => Boolean): String =
    @tailrec
    def loop(n: Int): String = {
      if n + 1 >= s.length then ""  // exit condition 1: no pairs found
      else if f(s(n), s(n + 1)) then s"${s(n)}${s(n+1)}"  // exit condition 2: pair found thus return pair
      else
        loop(n + 1)
    }
    loop(0)

  /**
   * This funcion keeps on removing polymer pairs from the string until it's no longer possible. Then it returns
   * the remaining string.
   */
  @tailrec
  def destructPolymer(polymer: String): String =
    val hit = findFirstPair(polymer)((x: Char, y: Char) => math.abs(x.toInt - y.toInt) == 32)
    if hit == "" then polymer  // exit condition: return polymer that can no longer be destroyed.
    else destructPolymer(polymer.replace(hit, ""))

  private val answer1 = destructPolymer(input).length
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  val alphabet = Range(97,123).toList.map(_.toChar)
  val res = alphabet.map(
    c =>
      val newin = input.replace(c.toString, "").replace(c.toUpper.toString, "")
      destructPolymer(newin).length
  )
  private val answer2 = res.min
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")

