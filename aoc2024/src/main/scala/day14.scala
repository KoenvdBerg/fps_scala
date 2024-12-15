import aoc2024.BoundedGrid
import aoc2024.BoundedGrid.*
import aoc2024.SequenceUtils.*
import jdk.jshell.execution.Util

import scala.io.*
import math.*
import scala.annotation.tailrec
import scala.util.matching.Regex

object day14 extends App:

  case class R(p: (Int, Int), v: (Int, Int)):

    def move(time: Int): R = copy(p = grid.wrapMove(p, v, time))

  private val day: String = "14"

  private val start1: Long =
    System.currentTimeMillis

  private val input: Seq[R] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toSeq
      .map(parse)

  def parse(s: String): R = s match
    case s"p=$x,$y v=$v1,$v2" => R(x.toInt -> y.toInt, v1.toInt -> v2.toInt)

  val grid = BoundedGrid(101, 103, Seq.empty)

  private val answer1 = input
    .map(_.move(100))
    .map(p => p -> grid.classifyQuadrant(p.p))
    .filterNot(_._2 == -1)
    .groupBy(_._2)
    .map(_._2.length)
    .product
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  def computeTree(robots: Seq[R], maxT: Int): Int =

    def createGrid(points: Seq[(Int, Int)]): String =
      (for
        x <- Range(0, 101)
        y <- Range(0, 103)
      yield if points.contains(x -> y) then '#' else '.').mkString

    @tailrec
    def go(r: Seq[R], t: Int): Int =
      if t > maxT then t
      else
        val g = createGrid(r.map(_.p))
        if g.contains("########") then
          t
        else
          println(t)
          go(r.map(_.move(t)), t + 1)

    go(robots, 4188)


  // too low: 4188
  private val answer2 = computeTree(input, 10001)
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
