import scala.io.*
import math.*
import scala.annotation.tailrec
import scala.collection.mutable
import aoc2022.FlatGrid
import aoc2022.Grid2D.Point
import aoc2022.Algorithms.GraphTraversal.*

/**
 * 
 * EXAMPLE FOR A*
 *
 */


object day27 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val (width: Int, input): (Int, Vector[Char]) =
    val infile = Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector

    (infile.head.length, infile.flatMap(_.toVector))

  def groundGraph(ground: Vector[Char], width: Int): Graph[Int] =
    (i: Int) =>
      val neighbours: Vector[Int] = FlatGrid.neighbours4(i, width, ground.length)
      if neighbours.isEmpty then Map.empty
      else
        neighbours
          .filter((n: Int) => ground(n) == '.' || ground(n) == 'G')
          .map((f: Int) => (f -> 1))
          .toMap


  def preference(target: Int, width: Int)(index: Int): Int =
    val t: Point = FlatGrid.indexToPoint(target, width)
    val s: Point = FlatGrid.indexToPoint(index, width)
    s.manhattan(t)

  val source = input.indexOf('E')
  val target = input.indexOf('G')
  val res1 = shortestPath(groundGraph(input, width))(source, target, preference(target, width))

  private val answer1 = None
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  val res2 = shortestPath(groundGraph(input, width))(source, target)

  private val answer2 = None
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
