import aoc2024.BoundedGrid
import jdk.jshell.execution.Util

import scala.io.*
import math.*
import scala.annotation.tailrec
import scala.util.matching.Regex
import aoc2024.Algorithms.GraphTraversal.*

object day18 extends App:

  private val day: String = "18"

  private val start1: Long =
    System.currentTimeMillis

  private val points: Seq[(Int, Int)] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toSeq
      .map(s =>
        val i1 = s.takeWhile(_ != ',').toInt
        val i2 = s.dropWhile(_ != ',').slice(1, 101).toInt
        i1 -> i2
      )

  val grid: BoundedGrid = BoundedGrid(71, 71, Seq.empty)

  def step(localPoints: Seq[(Int, Int)])(p: (Int, Int)): Map[(Int, Int), Int] =
    val next = grid.neighbours4(p)
      .filterNot(localPoints.contains)
      .map(p => p -> 1)
      .toMap
    next

  val target = (70, 70)
  private val answer1 = shortestDistance(step(points.take(1024)))((0,0), target)
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  @tailrec
  def algorithm(i: Int): (Int, Int) =
    val todo = points.take(i)
    val path = shortestDistance(step(todo))((0,0), target)
    if path.isEmpty then points(i - 1)
    else
      algorithm(i + 1)

  private val answer2 = algorithm(2800)
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")


