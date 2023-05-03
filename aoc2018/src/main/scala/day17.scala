import scala.io.*
import math.*
import aoc2018.Grid2D.Point


object day17 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: Vector[(Point, Char)] =

    def parseClay(s: String): Vector[(Point, Char)] = s match
      case s"x=$x, y=${y1}..${y2}" => Range(y1.toInt, y2.toInt+1).map(y => (Point(x.toInt, y), '#')).toVector
      case s"y=$y, x=${x1}..${x2}" => Range(x1.toInt, x2.toInt+1).map(x => (Point(x, y.toInt), '#')).toVector
      case _ => sys.error("BOOM!!")

    val parsed: Vector[(Point, Char)] = Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector
      .flatMap(parseClay)

    val xMin: Int = parsed.minBy(_._1.x)._1.x
    val offset: Int = 500 - xMin
    val ground: Vector[(Point, Char)] = parsed.map(f => (Point(f._1.x - xMin, f._1.y), f._2)).toSet.toVector
    ground ++ Vector((Point(offset, 0), '+'))




  Point.print2dGrid(input)



  private val answer1 = None
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis


  private val answer2 = None
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
