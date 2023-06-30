import scala.io.*
import math.*
import scala.annotation.tailrec

/**
 * PART 01:
 *
 * Easy foldRight
 *
 * PART 02:
 * 
 * 
 * (1) flooding algorithm to find outside air blocks that are reachable from -1,-1,-1
 *      - use set input blocks and if new block not in that set to determine if block is viable outside block
 * (2) result is surface connection to outside air blocks and input
 *
 * 
 *
 * Sort and then take 3 and sum
 *
 */


object day18 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: List[Cube] =
    
    def parseCube(s: String): Cube = s match
      case s"$x,$y,$z" => Cube(x.toInt, y.toInt, z.toInt)
      case _           => sys.error(s"Cannot parse cube: $s")
    
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList
      .map(parseCube)

  case class Cube(x: Int, y: Int, z: Int): 
    
    def manhattan(that: Cube): Int = math.abs(x - that.x) + math.abs(y - that.y) + math.abs(z - that.z)
    def isConnected(that: Cube): Boolean = this.manhattan(that) == 1
    def adjacent: Set[Cube] = Set(
      Cube(x-1, y, z),
      Cube(x+1, y, z),
      Cube(x, y-1, z),
      Cube(x, y+1, z),
      Cube(x, y, z-1),
      Cube(x, y, z+1))

  private val res1: Int = input.foldLeft(0)((s: Int, c: Cube) => s + input.count((p: Cube) => c.isConnected(p)))
  private val answer1 = input.length * 6 - res1
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis
    
  import aoc2022.Algorithms.floodAlgorithm
  
  private val bound: Int = List(input.maxBy(_.x).x, input.maxBy(_.y).y, input.maxBy(_.z).z).max + 1
  
  def path(lava: Set[Cube])(c: Cube): Set[Cube] = 
    c.adjacent
      .filter((c: Cube) => c.x >= -1 && c.x <= bound && c.y >= -1 && c.y <= bound && c.z >= -1 && c.z <= bound)
      .diff(lava)
    
  private val outsideBlocks: Set[Cube] = floodAlgorithm(path(input.toSet))(Cube(-1,-1,-1))
  private val answer2 = input.foldLeft(0)((s: Int, c: Cube) => s + outsideBlocks.count((p: Cube) => c.isConnected(p)))
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
