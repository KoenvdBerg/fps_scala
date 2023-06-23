import scala.io.*
import math.*
import scala.annotation.tailrec
import aoc2022.Grid2D.{Point, Line}

/**
 * PART 01:
 *
 *
 * PART 02:
 *
 *
 */


object day15 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: Vector[Sensor] =
    
    def parseSensor(s: String): Sensor = s match
      case s"Sensor at x=$x, y=$y: closest beacon is at x=$bx, y=$by" => 
        val sensor: Point = Point(x.toInt, y.toInt)
        val beacon: Point = Point(bx.toInt, by.toInt)
        Sensor(sensor, beacon, sensor.manhattan(beacon))
      case _ => sys.error(s"Couldn't parse sensor: $s")
    
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .map(parseSensor)
      .toVector
    
  case class Sensor(loc: Point, beacon: Point, innerDist: Int)
  
  extension (p: Point) 
    def manhattan(that: Point): Int = math.abs(p.x - that.x) + math.abs(p.y -  that.y)
  
  object Sensor: 
    
    def distressBeacon(ss: Vector[Sensor], in: Point): Option[Point] =
      val withinRange: Boolean = ss.exists((s: Sensor) => s.loc.manhattan(in) <= s.innerDist)
      if withinRange then None else Some(in)
      
    def sensorEnds(s: Sensor, spacing: Int = 0): Vector[Point] =
      val d: Int = s.innerDist + spacing
      Vector(
        Point(s.loc.x, s.loc.y + d),
        Point(s.loc.x, s.loc.y - d),
        Point(s.loc.x + d, s.loc.y),
        Point(s.loc.x - d, s.loc.y))
      
    def sensorEdges(s: Sensor, spacing: Int = 0): Vector[Line] =
      val ends: Vector[Point] = sensorEnds(s, spacing)
      Vector(
        Line.makeLine(ends(0), ends(2)),
        Line.makeLine(ends(0), ends(3)),
        Line.makeLine(ends(1), ends(2)),
        Line.makeLine(ends(1), ends(3)),
      )
      
    def sensorEdgeFragments(s: Sensor, spacing: Int = 0): Vector[Int => Option[Int]] =
      val ends: Vector[Point] = sensorEnds(s, spacing)
      Vector(
        Line.makeLine(ends(0), ends(2)).fyBounded(ends(0).x.min(ends(2).x), ends(2).x.max(ends(0).x)),
        Line.makeLine(ends(0), ends(3)).fyBounded(ends(0).x.min(ends(3).x), ends(3).x.max(ends(0).x)),
        Line.makeLine(ends(1), ends(2)).fyBounded(ends(1).x.min(ends(2).x), ends(2).x.max(ends(1).x)),
        Line.makeLine(ends(1), ends(3)).fyBounded(ends(1).x.min(ends(3).x), ends(3).x.max(ends(1).x))
      )
      
  private val ySel: Int = 2000000
  private val edgeFragments: Vector[Int => Option[Int]] = input.foldLeft(Vector.empty)((vl: Vector[Int => Option[Int]], s: Sensor) => vl ++ Sensor.sensorEdgeFragments(s))
  private val ends: Vector[Int] = edgeFragments.flatMap(f => f(ySel))
  private val knownBeacons: Int = input.map(_.beacon.y).distinct.count((yb: Int) => yb == ySel)
  private val res1: Int = ends.max - ends.min + 1  // +1 accounts for the inclusive count of range
  private val answer1: Int = res1 - knownBeacons   // subtract the known beacons in that row
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  private val (lower, upper): (Int, Int) = (0, 4000000)
  private val edges: Vector[Line] = input.foldLeft(Vector.empty[Line])((vl: Vector[Line], s: Sensor) => vl ++ Sensor.sensorEdges(s, 1))
  private val pointsOfInterest: Set[Point] = edges.foldLeft((Set.empty[Point], edges)) {
    (s: (Set[Point], Vector[Line]), l: Line) =>
      val intersections: Set[Point] = s._2.flatMap((l2: Line) => l.intersect(l2)).toSet
      (s._1 ++ intersections, edges)
  }._1
  private val search: Set[Point] = pointsOfInterest.flatMap((p: Point) => Sensor.distressBeacon(input, p))
  private val distressBeacon: Point = search.filter((p: Point) => p.x > lower && p.x < upper && p.y > lower && p.y < upper).head
  private val answer2: Long = distressBeacon.x * 4000000L + distressBeacon.y
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")

