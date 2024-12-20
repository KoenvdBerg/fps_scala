import jdk.jshell.execution.Util

import scala.io.*
import math.*
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.util.matching.Regex
import aoc2024.SequenceUtils.*
import aoc2024.Optim.Memoize

object day11 extends App:

  private val day: String = "11"

  private val start1: Long =
    System.currentTimeMillis

  private val input: Seq[Long] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .mkString
      .split(" ")
      .toSeq
      .map(_.toLong)

  case class Stone(iter: Int):

    def split(s: Long, it: Int): Long =
      if it >= iter then 1L
      else
        val stringId = s"$s"
        if s == 0 then split(1L, it + 1)
        else if stringId.length % 2 == 0 then
          val splitted = stringId.splitAt(stringId.length / 2)
          solveMemoized(splitted._1.toLong, it + 1) + solveMemoized(splitted._2.toLong, it + 1)
        else solveMemoized(s * 2024, it + 1)

    val solveMemoized: ((Long, Int)) => Long = Memoize[(Long, Int), Long]((s: Long, it: Int) => split(s, it)).getMemoizedf

  val stones = input.map(l => (l, 0))
  private val answer1 = stones.map(Stone(25).solveMemoized).sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  private val answer2 = stones.map(Stone(75).solveMemoized).sum
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
