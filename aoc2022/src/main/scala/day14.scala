import scala.io.*
import math.*
import scala.annotation.tailrec
import aoc2022.Grid2D.Point
import aoc2022.FlatGrid.printFlatGrid


/**
 * PART 01:
 *
 * Easy foldRight
 *
 * PART 02:
 *
 * Sort and then take 3 and sum
 *
 */


object day14 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis
  
  private val rock: Vector[Point] =
    
    def parseRock(s: String): Vector[Point] =
      val rockPoints: Vector[Point] = s.split(" -> ").toVector.map {
        case s"$x,$y" => Point(x.toInt, y.toInt)
        case _        => sys.error(s"Couldn't parse rock: $s")
      }
      rockPoints.foldLeft(Vector.empty[Point])((t: Vector[Point], p: Point) => 
        if t.isEmpty then Vector(p)
        else t.dropRight(1) ++ t.last.smear(p)
      ) 
    
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector
      .flatMap(parseRock)
      .distinct
    
  enum Sand:
    case Left, Right, Down, Rest
    
  object Sand: 
    
    def goDir(p: Point, dir: Sand): Point = dir match
      case Left  => Point(p.x - 1, p.y + 1)
      case Right => Point(p.x + 1, p.y + 1)
      case Down  => Point(p.x, p.y + 1)
      case _     => p
    
    def checkDir(p: Point, obstacles: Vector[Point], dir: Sand): Boolean = !obstacles.contains(goDir(p, dir))
      
    @tailrec
    def simSand(rock: Vector[Point], sand: Vector[Point], loc: Point)(toInf: Boolean): Vector[Point] =
      val nextMove: Sand = 
        if checkDir(loc, rock ++ sand, Down) then Down
        else if checkDir(loc, rock ++ sand, Left) then Left
        else if checkDir(loc, rock ++ sand, Right) then Right
        else Rest

      val exit: Boolean = 
        if toInf then loc.y >= rock.maxBy(_.y).y       // for part01
        else loc == Point(500, 0) && nextMove == Rest  // for part02
      if loc.y >= 10000 then sys.error("SAND SLIPS AWAY AND THAT SHOULD NOT HAPPEN")
      else if exit then sand
      else nextMove match
          case Rest  => simSand(rock, loc +: sand, Point(500, 0))(toInf)
          case dir     => simSand(rock, sand, goDir(loc, dir))(toInf)
  

  private val start: Point = Point(500, 0)
  private val res1 = Sand.simSand(rock, Vector.empty[Point], start)(true)
  private val answer1 = res1.length
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis


//  private val minX: Point = Point(rock.minBy(_.x).x - 10000, rock.maxBy(_.y).y + 2)
//  private val maxX: Point = Point(rock.maxBy(_.x).x + 10000, rock.maxBy(_.y).y + 2)
//  private val newRock: Vector[Point] = rock ++ minX.smear(maxX)
//  private val res2: Vector[Point] = Sand.simSand(newRock, Vector.empty[Point], start)(false)
//
//  val x: String = Point.gridPrintable(start +: (newRock))((p: Point) => 
//    if newRock.contains(p) then '#' 
//    else if p == start then '+'
//    else if res2.contains(p) then 'o'
//    else '.')
//  println(x)
//
//  val answer2 = res2.length + 1  // accounting for the last sand at the source +
//  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
