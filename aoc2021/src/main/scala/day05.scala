import scala.annotation.tailrec
import scala.io.*
import aoc2021.Grid2D.{Line, Point}

object day05 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: Vector[(Int, Int, Line)] =
    
    def parse(s: String): (Int, Int, Line) = s match
      case s"$x1,$y1 -> $x2,$y2" if x1.toInt == x2.toInt && y1.toInt < y2.toInt => (y1.toInt, y2.toInt, Line.makeLine(Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt)))
      case s"$x1,$y1 -> $x2,$y2" if x1.toInt == x2.toInt => (y2.toInt, y1.toInt, Line.makeLine(Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt)))
      case s"$x1,$y1 -> $x2,$y2" if x1.toInt < x2.toInt => (x1.toInt, x2.toInt, Line.makeLine(Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt)))
      case s"$x1,$y1 -> $x2,$y2" => (x2.toInt, x1.toInt, Line.makeLine(Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt)))
      case _ => sys.error(s"Cannot parse $s")
    
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector
      .map(parse)
  
  private val res1 = input
    .map(in => (in._3.delta, in._3.getRange(in._1, in._2)))

  private val answer1 = res1.filter(_._1 == 0).flatMap(_._2).groupBy(identity).count(_._2.length > 1)
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  private val answer2 = res1.flatMap(_._2).groupBy(identity).count(_._2.length > 1)
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")