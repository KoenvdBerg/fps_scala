import scala.io.*
import math.*
import scala.annotation.tailrec

/**
 * PART 01:
 *
 * Working with sets it's easy to find the overlapping character
 *
 * PART 02:
 *
 * Again working with sets, see implementation
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
    if c.isUpper then c.toLower.toInt - (96 - 26)
    else c.toInt - 96


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

  def findChar3(ss: List[String], acc: Set[Char] = Set.empty[Char]): Char = ss match
    case Nil    => if acc.size > 1 then sys.error("BOOM") else acc.head
    case h :: t =>
      if acc.isEmpty then findChar3(t, acc ++ h)
      else findChar3(t, h.toSet.intersect(acc))


  val answer2: Int = input
    .grouped(3)
    .map((ls: List[String]) => findChar3(ls))
    .map((c: Char) => charToValue(c))
    .sum
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
