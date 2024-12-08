import aoc2024.{BoundedGrid}
import aoc2024.BoundedGrid.*
import jdk.jshell.execution.Util

import scala.io.*
import math.*
import scala.annotation.tailrec
import scala.util.matching.Regex
import scala.util.Try

object day06 extends App:

  private val day = "day06"

  private val start1: Long =
    System.currentTimeMillis

  val grid = BoundedGrid.fromResource(s"$day.txt")

  case class S(direction: (Int, Int), cur: (Int, Int), seen: Set[(Int, Int)], been: Map[((Int, Int), (Int, Int)), Int]):

    def oob(grid: BoundedGrid): Boolean = !grid.withinBounds(cur)

    def isLoop: Boolean =
      been.get(cur -> direction) match
        case None => false
        case Some(i) => i >= 2

    def step(grid: BoundedGrid): S =
      val n = cur + direction
      val o = grid.grid(n._2)(n._1)
      if o == '#' then S(direction.rotate, cur, seen, been)
      else
        S(direction, n, seen + cur, been.updated(cur -> direction, been.getOrElse(cur -> direction, 0) + 1))

  @tailrec
  def algorithm(state: S, grid: BoundedGrid): S =
    if !grid.withinBounds(state.cur + state.direction) then state.copy(seen = state.seen + state.cur)
    else algorithm(state.step(grid), grid)

  val p = grid.getPointsOf('^').head
  val s = S((0, -1), p, Set(p), Map.empty)

  private val answer1 = algorithm(s, grid).seen.size
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  @tailrec
  def algorithm2(state: S, grid: BoundedGrid): Boolean =
    if !grid.withinBounds(state.cur + state.direction) then false
    else if state.isLoop then true
    else algorithm2(state.step(grid), grid)

  private val answer2 = grid.rows.flatten
    .foldLeft(0) {(res: Int, i: (Int, Int)) =>
      if grid.grid(i._2)(i._1) == '#' then res
      else if i == p then res
      else
        val g = grid.patchOne(i, '#')
        val r = algorithm2(s,g)
        if r then res + 1 else res
    }
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")


