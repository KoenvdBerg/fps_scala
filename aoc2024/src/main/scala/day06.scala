import aoc2024.FlatGrid
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

  private val input: String =
    Source
      .fromResource(s"$day.txt")
      .getLines
      .mkString

  private val width: Int =
    Source
      .fromResource(s"day06.txt")
      .getLines
      .toSeq.head.length

  val grid = FlatGrid(input.length, width)

  case class S(direction: Int, cur: Int, seen: Set[Int], been: Map[(Int, Int), Int]):

    def oob: Boolean =
      if direction == width then cur >= grid.gridLength
      else if direction == -1 then grid.isLeftBound(cur)
      else if direction == -width then cur < 0
      else grid.isRightBound(cur - 1)

    def isLoop: Boolean =
      been.get(cur -> direction) match
        case None => false
        case Some(i) => i >= 2

    def rotate: S = if direction == width then copy(direction = -1)
      else if direction == -1 then copy(direction = -width)
      else if direction == -width then copy(direction = 1)
      else copy( direction = width)

    def step(field: String): S =
      val n = cur + direction
      val o = field(n)
      if o == '#' then rotate
      else
        S(direction, cur + direction, seen + cur, been.updated(cur -> direction, been.getOrElse(cur -> direction, 0) + 1))

  @tailrec
  def algorithm(state: S): S =
    if state.oob then state
    else
      Try(state.step(input)).toOption match
        case None => state.copy(seen = state.seen + state.cur)
        case Some(her) => algorithm(her)

  val p = input.indexWhere(_ == '^')
  val s = S(-width, p, Set(p), Map((p -> -width) -> 1))

  private val answer1 = algorithm(s).seen.size
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  @tailrec
  def algorithm2(state: S, field: String): Boolean =
    if state.oob then false
    else if state.isLoop then true
    else
      Try(state.step(field)).toOption match
        case None => false
        case Some(her) => algorithm2(her, field)

  private val answer2 = input.indices
    .foldLeft(0) {(res: Int, i: Int) =>
      if input(i) == '#' then res
      else if i == p then res
      else
        val ni = input.updated(i, '#')
        val r = algorithm2(s,ni)
        if r then res + 1 else res
    }

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")


