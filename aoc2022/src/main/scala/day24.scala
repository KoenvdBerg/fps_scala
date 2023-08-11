import scala.io.*
import math.*
import scala.annotation.tailrec
import aoc2022.Grid2D.Point

/**
 * PART 01:
 *
 * To compute the blizzard locations at a certain time I used the following idea: 
 * 
 *         -1012345
 *        -1#.#####
 *         0#.....#
 *         1#.>...#
 *         2#..E..#
 *         3#.....#
 *         4#...v.#
 *         5#####.#
 * 
 * If an Elve (E) moves to Point(2,2) at time 1, then if a blizzard would be present in the original input at Points 
 * - Point(1,2) : > 
 * - Point(3,2) : < 
 * - Point(2,3) : `^`
 * - Point(2,1) : v
 * 
 * the Elve would not be able to move to Point(2,2). In the above example, at all 4 points, no blizzard of the corresponding
 * type is present, and thus the Elve can move to Point(2,2). 
 * 
 * This can be computed with the doesntCollide() function.  
 * 
 * To track all the correct paths, I used a Set instead of a queue. This set holds all the current states, and each 
 * item in the set is used to compute the next state. Each iteration consists of all Elve locations and the 
 * corresponding time. 
 * 
 * PART 02:
 *
 * Used the implementation from part01 two more times, see below. 
 *
 */


object day24 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: Vector[Vector[Char]] =
    
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector
      .map(_.toVector.drop(1).dropRight(1))
      .drop(1).dropRight(1)
    
    
  object Field:

    val width: Int = input.head.length
    val height: Int = input.length
    
    def doesntCollide(field: Vector[Vector[Char]], loc: Point, time: Int): Boolean = 

      def wrap(x: Int, rem: Int): Int =
        val mod = x % rem
        if mod < 0 then mod + rem else mod

      loc.x >= 0 
        && loc.x < width
        && loc.y >= 0 
        && loc.y < height
        && field(loc.y)(wrap(loc.x - time, width)) != '>'
        && field(loc.y)(wrap(loc.x + time, width)) != '<'
        && field(wrap(loc.y - time, height))(loc.x) != 'v'
        && field(wrap(loc.y + time, height))(loc.x) != '^'
      
    
    def blizzardPath(field: Vector[Vector[Char]], start: Point, finish: Point, startTime: Int): Int =

      @tailrec
      def go(active: Set[Point], time: Int): Int =
        val nextLocs: Set[Point] = active.flatMap(_.adjacentInclusive)
        if nextLocs(finish) then time
        else
          val next: Set[Point] = nextLocs.filter((p: Point) => doesntCollide(field, p, time))
          go(next + start, time + 1)  // add start in case waiting at start is needed

      go(Set(start), startTime)
      
  private val start: Point = Point(0, -1)
  private val end: Point = Point(Field.width - 1, Field.height)
  private val answer1: Int = Field.blizzardPath(input, start, end, 1)
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  private val res2: Int = Field.blizzardPath(input, end, start, answer1)
  private val answer2: Int = Field.blizzardPath(input, start, end, res2)
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
