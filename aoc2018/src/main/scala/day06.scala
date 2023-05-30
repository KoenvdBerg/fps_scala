import scala.io.*
import math.*

/**
 *
 * PART 1:
 *
 * To solve this problem, I created a 2d grid that has every point between the smallest and largest point in the
 * spacemap. Then for all those points I computed the manhattan distance to all target points from the input. The point
 * was then assigned to the closest target point. This gave a Map that linked every target point to all it's closest points.
 *
 * Then to find the solution, I filtered the target points to not have any closest point on the border of the spacemap.
 * The largest target point that had that was the result.
 *
 * PART 2:
 *
 * This is somewhat easier than part 1. Basically compute for every point the sum of the manhattan distances to all
 * the target points. Then filter all these results for the just computed sum to be lower than 10000. The number of
 * points that remain is the answer.
 *
 */

object day06 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  case class Point(x: Int, y: Int)

  private val input: List[Point] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .map(g =>
        val x = g.split(",")
        Point(x.head.strip().toInt, x.last.strip().toInt))
      .toList

  def manhattan(p: Point, targets: List[Point]): (Point, Point) =
    val dist: List[Int] = targets.map(t => abs(p.x - t.x) + abs(p.y - t.y))
    val closest: (Int, Int) = dist.zipWithIndex.min
    if dist.count(_ == closest._1) > 1 then (Point(-1,-1), p)
    else (targets(closest._2), p)

  val minp: Point = Point(0,0)
  val maxp: Point = Point(input.maxBy(_.x).x, input.maxBy(_.y).y)
  val spacemap: IndexedSeq[Point] = for {
    x <- Range(minp.x, maxp.x)
    y <- Range(minp.y, maxp.y)
  } yield Point(x, y)

  val pointgroups: Map[Point, IndexedSeq[Point]] = spacemap
    .map(manhattan(_, input))         // compute the manhatten distance for every point on the spacemap to every point in the input
    .groupBy(o => o._1)               // group by the points from the input
    .map((x, y) => (x, y.map(_._2)))  // take the point of the spacemap and link them to the points from the input
  val res1: Map[Point, IndexedSeq[Point]] = pointgroups
    .filter((_, y) => !y.exists(p => p.x == minp.x | p.x == maxp.x | p.y == minp.y | p.y == maxp.y))  // filter for no point out of bounds

  private val answer1 = res1.map((_, y) => y.size).max
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  /**
   * Computes for any given point the sum of manhatten distances to a list of target points
   */
  def manhattan2(p: Point, targets: List[Point]): Double =
    val dists: List[Int] = targets.map(t => abs(p.x - t.x) + abs(p.y - t.y))
    dists.sum

  val res2: IndexedSeq[Double] = spacemap.map(manhattan2(_, input))
  private val answer2: Int = res2.count(_ < 10000)
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
