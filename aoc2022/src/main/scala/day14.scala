import scala.io.*
import math.*
import scala.annotation.tailrec
import aoc2022.Grid2D.Point


/**
 * PART 01:
 *
 * - similar to problem aoc2018 day 17
 * - started out with using a Set
 * - improved the printing of multiple points by converting Vector[Point] to a flattened grid 
 * - used the smear() function on Point again
 * - moved on to a counter
 * 
 * - explain the maxY variable and earlier recomputation of maxY in every iteration. Explain how that optimization was
 * big once discovered. 
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
  
  private val rock: Set[Point] =
    
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
      .flatMap(parseRock)
      .toSet
    
  enum Sand:
    case Left, Right, Down, Rest
    
  object Sand: 
    
    def goDir(p: Point, dir: Sand): Point = dir match
      case Left  => Point(p.x - 1, p.y + 1)
      case Right => Point(p.x + 1, p.y + 1)
      case Down  => Point(p.x, p.y + 1)
      case _     => p
    
    def checkDir(p: Point, obstacles: Set[Point], dir: Sand): Boolean = 
      !obstacles.contains(goDir(p, dir))

    @tailrec
    def simSand(obstacles: Set[Point], sand: Int, loc: Point)(toInf: Boolean): Int =
      val nextMove: Sand = 
        if checkDir(loc, obstacles, Down) then Down
        else if checkDir(loc, obstacles, Left) then Left
        else if checkDir(loc, obstacles, Right) then Right
        else Rest

      val exit: Boolean = 
        if toInf then loc.y >= maxY                      // for part01
        else loc == Point(500, 0) && nextMove == Rest    // for part02
      if exit then sand
      else nextMove match
          case Rest  => simSand(obstacles + loc, sand + 1, Point(500, 0))(toInf)  // new grain of sand starts falling
          case dir   => simSand(obstacles, sand, goDir(loc, dir))(toInf)          // progress a falling grain of sand
  
  private val start: Point = Point(500, 0)
  private val maxY: Int = rock.maxBy(_.y).y
  private val answer1: Int = Sand.simSand(rock, 0, start)(true)
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  private val minX: Point = Point(rock.minBy(_.x).x - 10000, rock.maxBy(_.y).y + 2)
  private val maxX: Point = Point(rock.maxBy(_.x).x + 10000, rock.maxBy(_.y).y + 2)
  private val newRock: Set[Point] = rock ++ minX.smear(maxX).toSet
  private val answer2: Int = Sand.simSand(newRock, 0, start)(false) + 1 // +1 accounts for the last grain at source + 

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
