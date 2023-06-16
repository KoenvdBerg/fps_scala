import scala.io.*
import math.*
import scala.annotation.tailrec

/**
 * PART 01:
 *
 * Basically a foldLeft heavily using the .sliding operator on Strings
 *
 * PART 02:
 *
 * Idem to part01, but then with sliding window of 14
 *
 */


object day06 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: String =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .mkString("")

  def findMarker(i: (Int, Boolean), s: String): (Int, Boolean) =
    if i._2 then i
    else
      val test: Boolean = s.toSet.size == s.length
      if test then (i._1, true)
      else (i._1 + 1, false)

  private val res1: (Int, Boolean) = input
    .sliding(4)
    .foldLeft((0, false))(findMarker)
  private val answer1: Int = res1._1 + 4
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  private val res2: (Int, Boolean) = input
    .sliding(14)
    .foldLeft((0, false))(findMarker)
  val answer2:  Int = res2._1 + 14
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
