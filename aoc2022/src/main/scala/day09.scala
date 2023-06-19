import scala.io.*
import math.*
import scala.annotation.tailrec
import aoc2022.Grid2D.Point


/**
 * PART 01:
 *
 * I changed my solution for part01 based on the requirements for part02. The main thing I found out was that you 
 * can make a knot move like the previous not by just looking at the location of that previous knot: 
 *
 * val dist: Pointa = head - tail
 * if math.abs(dist.x) <= 1 && math.abs(dist.y) <= 1 then don't move
 * else tail + Point(dist.x.sign, dist.y.sign) 
 *
 * Once figured out, the remainder could be solved by a foldLeft, and a scanLeft on every knot in the knotList. 
 *
 * PART 02:
 *
 * After adjusting part01 to the requirements for part02, the solution was a simple run for the 9th knot. 
 *
 */


object day09 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: List[(Char, Int)] =

    def parseMove(s: String): (Char, Int) =
      val parts = s.split(" ")
      (parts.head.head, parts.last.toInt)

    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList
      .map(parseMove)

  extension(p: Point)
    def snapToPoint(that: Point): Point = 
      val dist: Point = that - p
      if math.abs(dist.x) <= 1 && math.abs(dist.y) <= 1 then p  // points are too close no movement needed
      else p + Point(dist.x.sign, dist.y.sign)

  case class RopeMovement(knots: Vector[Point], seen: Set[Point]):
    def moveRope(dir: Point): RopeMovement =
      val newT: Vector[Point] = knots.drop(1).scanLeft(knots.head + dir)((thisH: Point, thisT: Point) => thisT.snapToPoint(thisH))
      RopeMovement(newT, seen + newT.last)

  def simulate(rm: RopeMovement, move: (Char, Int)): RopeMovement =

    def doMove(acc: RopeMovement, n: Int): RopeMovement =
      if n <= 0 then acc
      else
        move._1 match
          case 'R' => doMove(acc.moveRope(Point(1, 0)), n - 1)
          case 'L' => doMove(acc.moveRope(Point(-1, 0)), n - 1)
          case 'U' => doMove(acc.moveRope(Point(0, 1)), n - 1)
          case 'D' => doMove(acc.moveRope(Point(0, -1)), n - 1)
    doMove(rm, move._2)

  private val res1: RopeMovement = input.foldLeft(RopeMovement(Vector.fill(2)(Point(0,0)), Set.empty))(simulate)
  private val answer1 = res1.seen.size
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  private val res2: RopeMovement = input.foldLeft(RopeMovement(Vector.fill(10)(Point(0,0)), Set.empty))(simulate)
  private val answer2 = res2.seen.size
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
