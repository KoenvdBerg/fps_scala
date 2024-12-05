import jdk.jshell.execution.Util

import scala.io.*
import math.*
import scala.annotation.tailrec
import scala.util.matching.Regex

object day03 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: String =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .mkString

  def findMul(s: String): Option[(Long, Long)] = s match
    case s"$mul($i1,$i2)" => Some((i1.toLong, i2.toLong))
    case _ => None

  val regex = "mul\\(\\d{1,10},\\d{1,10}\\)".r

  val res = regex.findAllIn(input).toSeq.flatMap(findMul)

  private val answer1 = res.map((i1, i2) => i1 * i2).sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  def algorithm(s: String): Long =

    @tailrec
    def go(cur: String, sum: Long, d: Boolean): Long =
      if cur.isEmpty then sum
      else
        cur match
          case s"do()$r" => go(cur.drop(1), sum, true)
          case s"don't()$r" => go(cur.drop(1), sum, false)
          case s"mul($i1,$i2)$r" if i1.toLongOption.isDefined && i2.toLongOption.isDefined => go(cur.drop(1), if d then sum + i1.toLong * i2.toLong else sum, d)
          case _ => go(cur.drop(1), sum, d)

    go(s, 0, true)

  private val answer2 = algorithm(input)
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")


