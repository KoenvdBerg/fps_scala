import jdk.jshell.execution.Util

import scala.io.*
import math.*
import scala.annotation.tailrec
import scala.util.matching.Regex

object day13 extends App:

  private val day: String = "13"

  private val start1: Long =
    System.currentTimeMillis

  private val input =

    def parseButton(s: String) = s match
      case s"Button $_: X+$x, Y+$y" => x.toLong -> y.toLong
      case _ => sys.error("cannot parse button")

    def parsePrize(s: String) = s match
      case s"Prize: X=$x, Y=$y" => x.toLong -> y.toLong
      case _ => sys.error("cannot parse prize")

    val in = Source
      .fromResource(s"day$day.txt")
      .getLines
      .toSeq
      .filterNot(_.isEmpty)
      .grouped(3)
      .toSeq
    val a = in.map(_.head).map(parseButton)
    val b = in.map(_(1)).map(parseButton)
    val p = in.map(_.last).map(parsePrize)
    Seq(a,b,p).transpose.map(s => Prize(s.head, s(1), s(2)))


  case class Prize(a: (Long, Long), b: (Long, Long), p: (Long, Long)):

    def formula(nb: Long): Long = (p._1 - b._1 * nb) / a._1

    def check(na: Long, nb: Long): Boolean =
      na * a._1 + nb * b._1 == p._1 && na * a._2 + nb * b._2 == p._2


    // b = (py - Y1 * px) / (-X2 + Y2 * X1)

    def solveB = (p._2 - a._2 * p._1) / (b._2 * a._1 - b._1)
//    def solveB = (p._2 - a._2 * p._1) / (-a._2 * b._1 + b._2)
//    def solveB(nb: Long) = a._2.toDouble * (p._1 - b._1 * nb)/a._1.toDouble + b._2 * nb

    def solve =
      (for
        nb <- Range(0, 101).map(_.toLong)
        na = formula(nb)
      yield (na, nb)).filter(check)



  /**
   *
   * solve
   * Y1 * a + Y2 * b = py
   * Y1 * (px - X2 * b)/X1 + Y2 * b = py
   * Y1 * px/X1 - X2*b/X1 + Y2*b = py
   * Y1*px/X1 - X2*b/X1 + Y2*b = py
   * Y1*px/X1*b - X2/X1 + Y2 = py/b
   * Y1*px/b - X2 + Y2*X1 = py/b
   * Y1*px - X2*b + Y2*X1*b = py
   * -X2*b + Y2*X1*b = py - Y1*px
   * b(-X2 + Y2*X1) = py - Y1*px
   * b(-X2 + Y2*X1) = py - Y1*px
   * b = (py - Y1*px ) / (-X2 + Y2*X1)
   *
   * Y1 * (px - X2 * b)/X1 = py - Y2*b
   * (Y1*px - Y1*X2*b)/X1 = py - Y2*b
   *
   *
   * Y1*px - Y1*X2*b + Y2*b = py
   * - Y1*X2*b + Y2*b = py - Y1*px
   * b(-Y1*X2 + Y2) = py - Y1*px
   * b = (py - Y1*px) / (-Y1*X2 + Y2)
   *
   * X1 * a + X2 * b = px
   * X1 * a = px - X2 * b
   * a = (px - X2 * b) / X1
   *
   * fill in a:
   * X1 * (px - X2 * b)/X1 + X2 * b = px
   *
   * X1 * (px - X2 * b)/X1 + X2 * b = px
   * X1*px - X1*X2*b + X2*b = px
   * X1*X2*b + X2*b = px - X1*px
   * b(X1*X2 + X2) = px - X1*px
   * b = (px - X1*px) / (X1*X2 + X2)
   *
   *
   * 94 * n1 + 22 * n2 = 8400
   * 94 * n1 = 8400 - 22 * n2
   * n1 = (8400 - 22 * n2) / 94
   *
   */

//  private val answer1 = input.flatMap(_.solve).map(t => t._1 * 3 + t._2).sum
//  printlln(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

//  println(input.head)
//  val res = input.head.solveB(1)

//  val p = input.head
//  Range(0, 200).map( i =>
//    val l = i.toLong
//    val r = p.solveB(l)
//    r
//  ).sliding(2).map(v => v.last - v.head).toSeq

//  val sb = input.head.solveB(0)
//  val x = Range(0, 101).map(i => sb + i * 59.04255319148979)
//  println(sb)
//  println(x)

  // todo: work out the formula better. It should be solvable

  private val answer2 = input.head.solveB
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")


