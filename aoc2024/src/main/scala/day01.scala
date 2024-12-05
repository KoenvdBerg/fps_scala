import jdk.jshell.execution.Util

import scala.io.*
import math.*
import scala.annotation.tailrec
import scala.util.matching.Regex

object day01 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .map(parse)
      .toSeq

  def parse(s: String): (Int, Int) =
    val res = s.split(" ").toVector
    (res.head.strip().toInt, res.last.strip().toInt)

  val l1 = input.map(_._1).sorted
  val l2 = input.map(_._2).sorted

  private val answer1 = l1.zip(l2).map((a, b) => math.abs(a - b)).sum
    println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  def algortithm(i: Int): Int = l2.count(_ == i) * i

  private val answer2 = l1.map(algortithm).sum
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")


