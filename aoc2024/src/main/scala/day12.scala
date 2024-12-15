import jdk.jshell.execution.Util

import scala.io.*
import math.*
import scala.annotation.tailrec
import scala.util.matching.Regex
import aoc2024.BoundedGrid

import scala.collection.immutable.Queue

object day12 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  case class Fence(locs: Seq[(Int, Int)], area: Int, perimeter: Int)

  object Fence:
    val grid: BoundedGrid = BoundedGrid.fromResource("day12.txt")

    def empty: Fence = Fence(Seq.empty, 0, 0)

    def computeFence(pos: (Int, Int), c: Char): Fence =

      val in = Queue(pos)

      def go(q: Queue[(Int, Int)], f: Fence): Fence =
        if q.isEmpty then f
        else
          val (cur, nextQ) = q.dequeue
          val ns = grid.neighbours4Unbounded(cur)
          val n = ns.filter(p => grid.withinBounds(p) && grid.get(p) == c)
          val perm = ns.filterNot(p => grid.withinBounds(p) && grid.get(p) == c)
          go(nextQ.appendedAll(n).filterNot(f.locs.contains).distinct, Fence(f.locs.appended(cur), f.area + 1, f.perimeter + perm.length))

      go(in, empty)

    def computeAll: Seq[Fence] =

      def go(todo: Seq[(Int, Int)], acc: Seq[Fence]): Seq[Fence] =
        if todo.isEmpty then acc
        else
          val cur = todo.head
          val c = grid.get(cur)
          val fence = computeFence(cur, c)
          go(todo.filterNot(fence.locs.contains), acc.appended(fence))

      go(grid.rows.flatten, Seq.empty)

    def perimeterX(locs: Seq[(Int, Int)], x: Int): Int =
      locs
        .filter(_._1 == x)
        .map(_._2)
        .sorted
        .sliding(2).count(g => (g.last - g.head) > 1) + 1

    def perimeterY(locs: Seq[(Int, Int)], y: Int): Int =
      locs
        .filter(_._2 == y)
        .map(_._1)
        .sorted
        .sliding(2)
        .count(g => (g.last - g.head) > 1) + 1

    def perimeterForFence(f: Fence): Int =
      val left = f.locs.map(grid.neighbourLeft).filterNot(f.locs.contains)
      val rl = left.map(_._1).distinct.map(x => perimeterX(left, x))

      val right = f.locs.map(grid.neighbourRight).filterNot(f.locs.contains)
      val rr = right.map(_._1).distinct.map(x => perimeterX(right, x))

      val above = f.locs.map(grid.neighbourAbove).filterNot(f.locs.contains)
      val ra = above.map(_._2).distinct.map(y => perimeterY(above, y))

      val below = f.locs.map(grid.neighbourBelow).filterNot(f.locs.contains)
      val rb = below.map(_._2).distinct.map(y => perimeterY(below, y))

      (rl ++ rr ++ ra ++ rb).sum

  private val fences = Fence.computeAll
  private val answer1 = fences
    .map(f => f.perimeter * f.area).sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  private val answer2 = fences.map(f => Fence.perimeterForFence(f) * f.area).sum
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")


