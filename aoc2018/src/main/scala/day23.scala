import scala.io.*
import aoc2018.Grid2D.Point
import scala.util.Random

/**
 * PART 01:
 *
 * Solving this involves three steps:
 *
 *  1. finding the bot with the biggest radius
 *  2. computing the distance of each bot to the bot with the biggest range
 *  3. distances lower or equal to the biggest radius should be counted
 *
 * PART 02:
 *
 * Disclosure: Solution adapted from looking at the answers on Reddit.
 *
 * Looking at the puzzle input you can notice that the result is right along the edge of the radia of multiple bots.
 *
 * Turns out that the final solution has the same property. If that wasn't the case, the below solution wouldn't work.
 *
 * It makes line segments for the distance to the point 0,0,0 +- radius. So a bot like this: pos=<12,14,12>, r=2 would
 * end up with: (36, 1) and (40, -1). The 1 marks the start of the line segment, and the -1 the end of the line segment.
 *
 * The solution then works by making a monoid that can only fold right that keeps a maximum count. This monoid is then
 * applied to a sorted list of line segments.
 *
 */

object day23 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  case class Bot(x: Int, y: Int, z: Int, r: Int):

    def distance(to: Bot): Int =
      math.abs(x - to.x) + math.abs(y - to.y) + math.abs(z - to.z)


  private val input: List[Bot] =

    def parseBot(s: String): Bot = s match
      case s"pos=<$x,$y,$z>, r=$r" => Bot(x.toInt, y.toInt, z.toInt, r.toInt)
      case _                       => sys.error("BOOM")

    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList
      .map(parseBot)


  val biggestRad: Bot = input.maxBy((b: Bot) => b.r)

  val inRange: List[Int] = input
    .map((b: Bot) => b.distance(biggestRad))
    .filter((d: Int) => d <= biggestRad.r)

  private val answer1 = inRange.length
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  /**
   * Creates line segments as Tuple like: (distance to 0,0,0, start or end of segment, total count (always 0))
   */
  def createLineSegments(bots: List[Bot]): List[Line] =
    val radiusLines: List[Line] = bots.flatMap{ (b: Bot) =>
      val d: Int = b.distance(Bot(0,0,0,0))
      List(((d - b.r).max(0), 1, 0), (d + b.r, -1, 0))}
    radiusLines.sortBy(_._1)

  type Line = (Int, Int, Int)
  object Line:
    def op(l1: Line, l2: Line): Line =
      val c: Int = l1._2 + l2._2                  // add count of next line segment to current count
      if c > l1._3 then (l1._1.max(l2._1), c, c)  // if count is higher than maxcount, store distance
      else (l1._1, l1._2, c)                      // else continue and track count
    def zero: Line = (0, 0, 0)

  import Line.*
  private val answer2 = createLineSegments(input).foldRight(Line.zero)(Line.op)
  println(s"Answer day $day part 2: distance of ${answer2._1} with ${answer2._3} nanobots [${System.currentTimeMillis - start2}ms]")
