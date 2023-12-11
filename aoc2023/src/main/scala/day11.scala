import scala.io.*
import math.*
import scala.annotation.tailrec

object day11 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: Vector[String] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector

  case class Point(x: Long, y: Long):
    def manhattan(that: Point): Long =
      math.abs(this.x - that.x) + math.abs(this.y - that.y)

  def expand(space: Vector[Point], n: Int): Vector[Point] =
    val hDiff: Vector[(Long, Point)] = space.sortBy(_.x).sliding(2).map(f => (f(1).x - f(0).x, f(1))).toVector
    val colums: Vector[Point] = hDiff.foldLeft(space.sortBy(_.x).take(1)){ (res: Vector[Point], in: (Long, Point)) =>
      if in._1 <= 1 then res.appended(in._2)
      else res.map(p => p.copy(x = (p.x + 1) - (n * (in._1 - 1)))).appended(in._2)
    }

    val vDiff: Vector[(Long, Point)] = colums.sortBy(_.y).sliding(2).map(f => (f(1).y - f(0).y, f(1))).toVector
    val rows: Vector[Point] = vDiff.foldLeft(colums.sortBy(_.y).take(1)) { (res: Vector[Point], in: (Long, Point)) =>
      if in._1 <= 1 then res.appended(in._2)
      else res.map(p => p.copy(y = (p.y +1) - (n * (in._1 - 1)))).appended(in._2)
    }
    rows

  def getGalaxyPoints(space: Vector[String]): Vector[Point] =
    (for
      y <- space.indices
      x <- space.head.indices
      if space(y)(x) == '#'
    yield Point(x, y)).toVector

  val galaxies: Vector[Point] = getGalaxyPoints(input)
  val expanded: Vector[Point] = expand(galaxies, 2)
  val answer1: Long = expanded.foldLeft((0L, Vector.empty)){ (res: (Long, Vector[Point]), in: Point) =>
    val shortestDist: Long = expanded.filter(g => !res._2.contains(g)).map(c => in.manhattan(c)).sum
    (res._1 + shortestDist, in +: res._2)
  }._1
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  val galax2: Vector[Point] = expand(galaxies, 1000000)
  val answer2: Long = galax2.foldLeft((0L, Vector.empty)) { (res: (Long, Vector[Point]), in: Point) =>
    val shortestDist: Long = galax2.filter(g => !res._2.contains(g)).map(c => in.manhattan(c)).sum
    (res._1 + shortestDist, in +: res._2)
  }._1
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
