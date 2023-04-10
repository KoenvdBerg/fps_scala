import scala.io.*
import math.*

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
    val dist = targets.map(t => abs(p.x - t.x) + abs(p.y - t.y))
    val closest = dist.zipWithIndex.min
    if dist.count(_ == closest._1) > 1 then (Point(-1,-1), p)
    else (targets(closest._2), p)

  val minp = Point(0,0)
  val maxp = Point(input.maxBy(_.x).x, input.maxBy(_.y).y)
  val spacemap = for {
    x <- Range(minp.x, maxp.x)
    y <- Range(minp.y, maxp.y)
  } yield Point(x, y)

  val pointgroups = spacemap.map(manhattan(_, input)).groupBy(o => o._1).map((x, y) => (x, y.map(_._2)))
  val res1 = pointgroups.filter((_, y) => !y.exists(p => p.x == minp.x | p.x == maxp.x | p.y == minp.y | p.y == maxp.y))

  private val answer1 = res1.map((_, y) => y.size).max
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis
  def manhattan2(p: Point, targets: List[Point]): Double =
    val dists = targets.map(t => abs(p.x - t.x) + abs(p.y - t.y))
    dists.sum

  val res2 = spacemap.map(manhattan2(_, input))
  private val answer2 = res2.count(_ < 10000)
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
