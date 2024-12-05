import jdk.jshell.execution.Util

import scala.io.*
import math.*
import scala.annotation.tailrec
import scala.util.matching.Regex

object day02 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: Seq[Seq[Int]] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toSeq
      .map(parse)

  def parse(s: String): Seq[Int] = s.split(" ").toSeq.map(_.toInt)

  def isSafe(si: Seq[Int]): Boolean =
    val x = si.sliding(2).map(p => p.last - p.head).toSeq
    x.forall(i => i > 0 && i <= 3) || x.forall(i => i < 0 && i >= -3)

  private val answer1 = input.count(isSafe)
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  def prep(si: Seq[Int]): Seq[Seq[Int]] = si.indices.map(i => si.patch(i, Seq.empty, 1))

  def safe2(si: Seq[Int]): Boolean = prep(si).exists(isSafe)

  private val answer2 = input.count(safe2)
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
