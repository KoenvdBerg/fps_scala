import aoc2024.BoundedGrid
import aoc2024.BoundedGrid.*
import aoc2024.Algorithms.GraphTraversal
import jdk.jshell.execution.Util

import scala.io.*
import math.*
import scala.annotation.tailrec
import scala.util.matching.Regex

object day16 extends App:

  private val day: String = "16"

  private val start1: Long =
    System.currentTimeMillis

  private val grid: BoundedGrid =
    val in = Source
      .fromResource(s"day$day.txt")
      .getLines
      .toSeq
    BoundedGrid.fromString(in.mkString, in.head.length)

  case class R(p: (Int, Int), dir: (Int, Int)):
    def step: R = copy(p = p + dir)
    def rotate: R = copy(dir = dir.rotate)
    def neighbours: Seq[R] =
      Seq(
        R(p._1 -> (p._2 - 1), (0, -1)),
        R((p._1 - 1) -> p._2, (-1, 0)),
        R((p._1 + 1) -> p._2, (1, 0)),
        R(p._1 -> (p._2 + 1), (0, 1))
      )
    def compareDir(that: R): Boolean = dir._1 == that.dir._1 && dir._2 == that.dir._2

  def step(r: R): Map[R, Int] =
    r.neighbours
      .filterNot(rr => grid.get(rr.p) == '#')
      .map(rr => if rr.compareDir(r) then rr -> 1 else rr -> 1001)
      .toMap

  val s = grid.getPointsOf('S').head
  val start = R(s, (1, 0))
  val e = grid.getPointsOf('E').head
  val end = R(e, (0, -1))

  private val answer1 = GraphTraversal.shortestDistance(step)(start, end)
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  val dijk = GraphTraversal.dijkstra(step)(start)
  private val answer2 = dijk._1
    .filter(_._1 == end)

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")


