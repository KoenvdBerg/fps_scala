import scala.io.*
import math.*
import scala.collection.mutable
import scala.collection.mutable.Stack
import aoc2018.Grid2D.Point

/**
 * PART 1 & 2:
 *
 * The essence of this problem is that the stars first contract to their most compact form (i.e. the message) and then
 * start expanding again towards infinity. My solution works by looking only at the maximum Y-coordinate found for
 * every star in every iteration. This Y-coordinate forms a parabola over time that looks like this:
 *
 *  y  │  x                   x
 *     │  x                  xx
 *     │  xx                 x
 *     │   xx               xx
 *     │    x              xx
 *     │    xx            xx
 *     │     xx         xxx
 *     │      xxxx   xxx
 *     │         xxOxx
 *     │
 *     └──────────────────────
 *                 H      t ->
 *
 * At point O the lowest Y coordinate is found at time H. At that time the stars form the message. The solution works
 * like this:
 *
 * 0) parse in the coordinates and directions of the stars
 * 1) find the lowest point O in the stars for the Y-coordinate.
 * 2) print the stars at point H to the terminal using my generalized grid printer. The stars are also aligned to the
 * origin (i.e. point x=0, y=0)
 *
 * The above has been defined in the function: alignStarsInSky()
 *
 */


object day10 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: Vector[StarVector] =

    val parser: String => StarVector = {
      case s"position=<${x}, ${y}> velocity=<${vx}, ${vy}>" =>
        StarVector(Point(x.strip().toInt, y.strip().toInt), Point(vx.strip().toInt, vy.strip().toInt))
      case _ => sys.error("boom")
    }

    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector
      .map(parser)

  case class StarVector(pos: Point, dir: Point):
    def updateCoords: StarVector =
      // updates the starvector based on direction
      this.copy(pos = Point(pos.x + dir.x, pos.y + dir.y))

  object Window:
    def windowMaxY(in: Vector[StarVector]): Int = in.maxBy((s: StarVector) => s.pos.y).pos.y

    def centerToOrigin(in: Vector[StarVector]): Vector[StarVector] =
      // this ensures that the window can be nicely printed without whitespace around the borders.
      val minX: Int = in.minBy((s: StarVector) => s.pos.x).pos.x
      val minY: Int = in.minBy((s: StarVector) => s.pos.y).pos.y
      in.map((s: StarVector) => s.copy(pos = Point(s.pos.x - minX, s.pos.y - minY)))

    def alignStarsInSky(s: Vector[StarVector], time: Int = 0): (Vector[StarVector], Int) =
      val thisWindow: Int = windowMaxY(s)                   // current max Y-coordinate
      val next: Vector[StarVector] = s.map(_.updateCoords)
      val nextWindow: Int = windowMaxY(next)                // next max Y-coordinate
      if nextWindow > thisWindow then (centerToOrigin(s), time)  // exit condition, return stars and total time
      else
        alignStarsInSky(next, time + 1)

  val res1: (Vector[StarVector], Int) = Window.alignStarsInSky(input)
  Point.print2dGrid(res1._1.map((s: StarVector) => (s.pos, '#')), ' ')
  println(s"Answer day $day part 1: ^^^^^^^^ [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  private val answer2 = res1._2
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
