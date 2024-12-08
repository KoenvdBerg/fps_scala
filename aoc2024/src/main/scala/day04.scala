import jdk.jshell.execution.Util

import scala.io.*
import math.*
import scala.annotation.tailrec
import scala.util.matching.Regex

import aoc2024.FlatGrid

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

  val grid: FlatGrid = FlatGrid(input.length, width).rotateClockWise.transpose.transpose.rotateCounterClockWise

  val toFind = IndexedSeq('X', 'M', 'A', 'S')

  def searchOnce(in: Seq[Char]): Int =
    in.sliding(4).count(_ == toFind) +
    in.reverse.sliding(4).count(_ == toFind)

  private val answer1 = (grid.diagonalValues(input) ++ grid.rowValues(input) ++ grid.columnValues(input))
    .map(s => searchOnce(s.mkString)).sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  val toFind2 = IndexedSeq('M', 'A', 'S').mkString

  def isXmas(i: Int, grid: FlatGrid, matrix: String): Boolean =
    val cross: Seq[Seq[Int]] = grid.getCrossIndicesAt(i)
    if cross.isEmpty then false
    else
      cross
        .map(slash => slash.map(matrix))
        .forall(slash => slash.mkString == toFind2 || slash.reverse.mkString == toFind2)

  val locationA: IndexedSeq[Int] = input.zipWithIndex.filter(_._1 == 'A').map(_._2)
  private val answer2 = locationA.count(i => isXmas(i, grid, input))
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
