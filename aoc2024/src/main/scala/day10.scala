import aoc2024.BoundedGrid
import jdk.jshell.execution.Util

import scala.io.*
import math.*
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.util.matching.Regex

object day00 extends App:

  private val day: String = "10"

  private val start1: Long =
    System.currentTimeMillis

  private val input: BoundedGrid =
    val in = Source
      .fromResource(s"day$day.txt")
      .getLines
      .toSeq
    BoundedGrid.fromString(in.mkString, in.head.length)

  def step(grid: BoundedGrid, cur: (Int, Int)): Seq[(Int, Int)] =
    val n: Seq[(Int, Int)] = grid.neighbours4(cur)
    val curI: Int = s"${grid.grid(cur._2)(cur._1)}".toInt
    val next = n.filter(p =>
      val i = s"${grid.grid(p._2)(p._1)}"
      if i.head == '.' then false
      else
        i.toInt - curI == 1
    )
    next

  def algorithm(q: Queue[(Int, Int)], grid: BoundedGrid, acc: Int): Int =
    if q.isEmpty then acc
    else
      val (n, nq) = q.dequeue
      if grid.grid(n._2)(n._1) == '9' then
        algorithm(nq, grid, acc + 1)
      else
        val next = step(grid, n)
        algorithm(nq.enqueueAll(next), grid, acc)

  def algorithmAt(p: (Int, Int), grid: BoundedGrid): Int =
    algorithm(Queue(p), grid, 0)

  val starts = input.


  private val answer1 = algorithmAt((3, 0), input)
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  private val answer2 = ""
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")


