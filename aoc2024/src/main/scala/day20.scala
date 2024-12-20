import jdk.jshell.execution.Util

import scala.io.*
import math.*
import scala.annotation.tailrec
import scala.util.matching.Regex
import aoc2024.BoundedGrid
import aoc2024.BoundedGrid.*
import aoc2024.Algorithms.GraphTraversal.*

object day20 extends App:

  private val day: String = "20"

  private val start1: Long =
    System.currentTimeMillis

  private val grid: BoundedGrid = BoundedGrid.fromResource(s"day$day.txt")

  def step(p: (Int, Int)): Map[(Int, Int), Int] =
    grid.neighbours4(p)
      .filterNot(cur => grid.get(cur) == '#')
      .map(cur => cur -> 1)
      .toMap

  val start = grid.getPointsOf('S').head
  val end = grid.getPointsOf('E').head
  val route = shortestPath(step)(start, end).get

  extension (p: (Int, Int))
    def n2Step: Seq[(Int, Int)] =
      Seq(
        (p._1, p._2 - 2),
        (p._1 - 2, p._2),
        (p._1 + 2, p._2),
        (p._1, p._2 + 2)
      )

  def algorithm(r: Seq[(Int, Int)]) =

    val routeLength = r.length - 1  // the start position does not count in length

    @tailrec
    def go(i: Int, cheats: Seq[Int]): Seq[Int] =
      if i >= routeLength then cheats
      else
        val p = r(i)
        val n = p.n2Step
        val skips = n
          .map(pp => r.indexOf(pp))
          .filter(ii => ii != -1 && (ii - 2) > i)
          .map(ii => ii - i - 2)
        go(i + 1, cheats.appendedAll(skips))

    go(0, Seq.empty)

  private val answer1 = algorithm(route)
    .groupBy(identity)
    .map((skipped, n) => skipped -> n.length)
    .toSeq
    .filter(_._1 >= 100)
    .map(_._2)
    .sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  def algorithm2(r: Seq[(Int, Int)]) =

    val routeLength = r.length - 1 // the start position does not count in length

    @tailrec
    def go(i: Int, cheats: Seq[Int]): Seq[Int] =
      if i >= routeLength then cheats
      else
        val p: (Int, Int) = r(i)
        val remaining: Seq[(Int, Int)] = r.drop(i + 1)
        val dists: Seq[((Int, Int), Int)] = remaining
          .map(t => t -> p.manhattan(t))
          .filter((_, d) => d <= 20)
        val skips = dists
          .map((pp, d) =>
            val ii = r.indexOf(pp)
            ii -> d
            )
          .filter((ii, d) => ii != -1 && (ii - d) > i)
          .map((ii, d) => ii - i - d)
        go(i + 1, cheats.appendedAll(skips))

    go(0, Seq.empty)

  private val answer2 = algorithm2(route)
    .groupBy(identity)
    .map((skipped, n) => skipped -> n.length)
    .toSeq
    .filter(_._1 >= 100)
    .map(_._2)
    .sum
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")


