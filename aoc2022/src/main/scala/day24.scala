import scala.io.*
import math.*
import scala.annotation.tailrec
import aoc2022.Grid2D.Point
import jdk.javadoc.internal.doclets.toolkit.util.DocFinder.Input

/**
 * PART 01:
 *
 * Main idea: being at point x,y, if  there would've been a blizzard at this point in the original input, I would be hit.
 * Counts for all four blizzard directions.
 *
 * PART 02:
 *
 * Sort and then take 3 and sum
 *
 */


object day24 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: Vector[Vector[Char]] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector.drop(1).dropRight(1)
      .map(_.toVector.drop(1).dropRight(1))



  object Field:
    val width: Int = input.head.length
    val height: Int = input.length

    def isInBounds(in: Point): Boolean = in.x >= 0 && in.x < width && in.y >= 0 && in.y < height

    def blizzardLocs(in: Point, time: Int): Vector[(Char, Point)] = Vector(
      ('>', Point(math.abs(in.x - time) % width, in.y)),
      ('<', Point((in.x + time) % width, in.y)),
      ('^', Point(in.x, (in.y + time) % height)),
      ('v', Point(in.x, math.abs(in.y - time) % height)))

    def blizzardPath(field: Vector[Vector[Char]], start: Point, finish: Point): Int =

      def go(active: Set[Point], time: Int): Int =

        val nextLocs: Set[Point] = active.flatMap(_.adjacentInclusive)
        if nextLocs.contains(finish) then time
        else
          val next = nextLocs
            .filter((p: Point) => isInBounds(p) && blizzardLocs(p, time).forall((c, r) => field(r.y)(r.x) != c))
          go(active ++ next, time + 1)

      go(Set(start), 1)
  // TODO: fix bug on the example input

  println(input.mkString("\n"))
  private val start: Point = Point(0, -1)
  private val answer1: Int = Field.blizzardPath(input, start, Point(Field.width - 1, Field.height))
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  private val answer2 = None
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
