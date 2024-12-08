import jdk.jshell.execution.Util

import scala.io.*
import math.*
import scala.annotation.tailrec
import scala.util.matching.Regex
import aoc2024.{BoundedGrid}

object day04 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: String =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .mkString

  private val width: Int =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector.head.length

  val grid: BoundedGrid = BoundedGrid.fromString(input, width)

  val toFind = IndexedSeq('X', 'M', 'A', 'S')

  def searchOnce(in: Seq[Char]): Int =
    in.sliding(4).count(_ == toFind) +
    in.reverse.sliding(4).count(_ == toFind)

  private val answer1 = (grid.diagonalValues ++ grid.rowValues ++ grid.columnValues)
    .map(s => searchOnce(s.mkString)).sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  val toFind2 = IndexedSeq('M', 'A', 'S').mkString

  def isXmas(p: (Int, Int), grid: BoundedGrid): Boolean =
    val cross: Seq[Seq[(Int, Int)]] = grid.crossNeighbour(p)
    val crossValues: Seq[String] = cross.map(_.map(p => grid.grid(p._2)(p._1)).mkString)
    crossValues.forall(slash => slash == toFind2 || slash.reverse == toFind2)

  val locationA: Seq[(Int, Int)] = grid.getPointsOf('A')
  private val answer2 = locationA.count(i => isXmas(i, grid))
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
