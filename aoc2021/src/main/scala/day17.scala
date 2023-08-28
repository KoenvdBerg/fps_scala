import scala.annotation.tailrec
import scala.io.*
import aoc2021.Grid2D.Point

object day17 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: (Point, Point) =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList
      .map {
        case s"target area: x=$x1..$x2, y=$y1..$y2" => (Point(x1.toInt, y2.toInt), Point(x2.toInt, y1.toInt)) 
      }
      .head
    
  case class Probe(loc: Point, v: Point): 
    def next: Probe =
      val newX: Int = loc.x + v.x
      val newY: Int = loc.y + v.y
      Probe(Point(newX, newY), Point(v.x - v.x.sign, v.y - 1))
      
    @tailrec
    final def inTarget(a: Point, b: Point, highPoint: Int = -1): Option[Int] =
      if loc.x >= a.x && loc.x <= b.x && loc.y <= a.y && loc.y >= b.y then Some(highPoint)
      else if loc.x > b.x || loc.y < b.y then None
      else
        next.inTarget(a, b, highPoint.max(loc.y))
      
  object Probe: 
    
    def findHighPoint(a: Point, b: Point): Vector[Int] = for {
        x  <- (0 to b.x).toVector
        y  <- (-1000 to 200).toVector
        hp <- Probe(Point(0,0), Point(x, y)).inTarget(a, b)
      } yield hp
    
  private val answer1 = Probe.findHighPoint(input._1, input._2).max
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  private val answer2 = Probe.findHighPoint(input._1, input._2).length
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")