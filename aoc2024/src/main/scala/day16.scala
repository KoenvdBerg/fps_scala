import aoc2024.Algorithms.GraphTraversal
import aoc2024.BoundedGrid
import aoc2024.BoundedGrid.*
import jdk.jshell.execution.Util

import scala.io.*
import math.*
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable
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

  case class R(p: (Int, Int), dir: (Int, Int)): //, path: Seq[(Int, Int)]):
    def neighbours: Seq[(R, Int)] = Seq(
      R(p + dir, dir) -> 1,
      R(p, dir.rotate) -> 1000,
      R(p, dir.rotateI) -> 1000
    )

    def flip = copy(dir = dir.rotate.rotate)

  object R:
    def step(r: R): Map[R, Int] = r.neighbours.filterNot((rr,_) => grid.get(rr.p) == '#').map((rr, i) => rr -> i).toMap

    def compare(r1: Option[Int], r2: Option[Int]): Int = (r1, r2) match
      case (Some(i1), Some(i2)) => i1 + i2
      case _ => -1

  val s = grid.getPointsOf('S').head
  val start = R(s, (1, 0))
  val e = grid.getPointsOf('E').head
  val end = R(e, (0, -1))

  val ds = GraphTraversal.dijkstra(R.step)(start)._1
  private val answer1 = ds(end)
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  val ends = Seq((1, 0), (-1, 0), (0, 1), (0, -1)).map(dir => R(e, dir))
  val de = GraphTraversal.dijkstraMultipleStarts(R.step)(ends)._1
  val allPoints = grid.getPointsOf('.').appended(s).appended(e)

  val res2 = for
    p <- allPoints
    d <- Seq((1, 0), (-1, 0), (0, 1), (0, -1))
    r = R(p, d)
    if R.compare(ds.get(r), de.get(r.flip)) == answer1
  yield p

  private val answer2 = res2.distinct.length
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
