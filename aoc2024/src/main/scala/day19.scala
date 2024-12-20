import jdk.jshell.execution.Util

import scala.io.*
import math.*
import scala.annotation.tailrec
import scala.util.matching.Regex
import aoc2024.Optim.Memoize

object day19 extends App:

  private val day: String = "19"

  private val start1 =
    System.currentTimeMillis

  private val (patterns, designs) =
    val in = Source
      .fromResource(s"day$day.txt")
      .getLines

    val patterns = in.next().split(",").toSeq.map(_.trim)
    in.next()
    val designs = in.toSeq
    patterns -> designs

  val patternMap: Map[Int, Seq[String]] = patterns.groupBy(_.length)

  val memoizeAlgorithm = Memoize[String, Long]((d: String) => algorithm(d)).getMemoizedf

  def algorithm(design: String): Long =
    if design.isEmpty then 1L
    else
      val l: Int = design.length
      val selection: Map[Int, Seq[String]] = patternMap.filter(_._1 <= l)
      val next: Seq[String] = selection
        .map((i, opt) =>
          if opt.contains(design.take(i)) then design.drop(i) else "X"
        ).toSeq.filterNot(_ == "X")

      next.map(memoizeAlgorithm).sum

  private val answer1 = designs.map(algorithm).count(_ > 0)
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  private val answer2 = designs.map(algorithm).sum
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")


