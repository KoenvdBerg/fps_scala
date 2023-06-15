import scala.io.*
import math.*
import scala.annotation.tailrec

/**
 * PART 01:
 *
 * Involves making a map of the scores and then a fold on the input
 *
 * PART 02:
 *
 * Involves making a map of the new scores and then a fold on the input
 */


object day02 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: List[(Char, Char)] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList
      .map((s: String) => (s(0), s(2)))

  val scores: Map[Char, Map[Char, Int]] = Map(
    'Y' -> Map(
      'A' -> 8,
      'B' -> 5,
      'C' -> 2
    ),
    'X' -> Map(
      'A' -> 4,
      'B' -> 1,
      'C' -> 7
    ),
    'Z' -> Map(
      'A' -> 3,
      'B' -> 9,
      'C' -> 6
    )
  )


  private val answer1 = input.foldLeft(0)((i: Int, c: (Char, Char)) => i + scores(c._2)(c._1))
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  val scores2: Map[Char, Map[Char, Int]] = Map(
    'Y' -> Map(
      'A' -> 4,
      'B' -> 5,
      'C' -> 6
    ),
    'X' -> Map(
      'A' -> 3,
      'B' -> 1,
      'C' -> 2
    ),
    'Z' -> Map(
      'A' -> 8,
      'B' -> 9,
      'C' -> 7
    )
  )

  val answer2 = input.foldLeft(0)((i: Int, c: (Char, Char)) => i + scores2(c._2)(c._1))
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
