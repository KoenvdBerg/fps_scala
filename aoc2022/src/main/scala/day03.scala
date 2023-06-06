import scala.io.*
import math.*
import scala.annotation.tailrec

/**
 * PART 01:
 *
 * PART 02:
 *
 */


object day03 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: List[String] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList

  def charToValue(c: Char): Int =
    if c.isUpper then
      Char.char2int(c.toLower) - (96 - 26)
    else
      Char.char2int(c) - 96


  def findChar(s: String): Char =
    val (s1, s2): (String, String) = s.splitAt(s.length / 2)
    val x: Set[Char] = s1.toSet.intersect(s2.toSet)
    x.head

  def computeScore(s: String): Int =
    charToValue(findChar(s))

  private val answer1: Int = input.map(computeScore).sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  val answer2 = None
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
