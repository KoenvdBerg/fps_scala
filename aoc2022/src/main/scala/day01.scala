import scala.io.*
import math.*
import scala.annotation.tailrec

/**
 * PART 01:
 *
 * Easy foldRight
 *
 * PART 02:
 *
 * Sort and then take 3 and sum
 *
 */


object day01 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: List[String] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList


  private val answer1 = input.foldRight(List(0))((a: String, b: List[Int]) => if a == "" then 0 :: b else (b.head + a.toInt) :: b.tail)
  println(s"Answer day $day part 1: ${answer1.max} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  val answer2 = answer1.sortBy(-_).take(3).sum
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
