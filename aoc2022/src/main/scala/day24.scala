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
 * This can be computed with the blizzardLocs() function.  
 * 
 * To track all the correct paths, I used a Set instead of a queue. This set holds all the current states, and each 
 * item in the set is used to compute the next state. Each iteration consists of all Elve locations and the 
 * corresponding time. 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 *
 * PART 02:
 *
 * Sort and then take 3 and sum
 *
 */


object day24 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: Vector[(Char, Point)] =
    
    val infile = Source
      .fromResource(s"day$day.txt")
      .getLines
      .zipWithIndex
      .toVector

    infile
      .flatMap((ss: String, y: Int) => ss.zipWithIndex.map((cc: Char, x: Int) => (cc, Point(x, y))))
      .filterNot(_._1 == '.')

  object Field:
    val nextMoves: Map[Char, Point] = Map('>' -> Point(1, 0), '<' -> Point(-1, 0), 'v' -> Point(0, 1), 
      '^' -> Point(0, -1), '#' -> Point(0, 0))
    
    def move(width: Int, height: Int)(blizzard: (Char, Point)): (Char, Point) = 
      val nextLoc: Point = blizzard._2 + nextMoves(blizzard._1)
      blizzard._1 match
        case '>' if nextLoc.x == width  => ('>', Point(1, nextLoc.y))
        case '<' if nextLoc.x == 0      => ('<', Point(width - 1, nextLoc.y))  
        case 'v' if nextLoc.y == height => ('v', Point(nextLoc.x, 1))
        case '^' if nextLoc.y == 0      => ('^', Point(nextLoc.x, height - 1))
        case _                          => (blizzard._1, nextLoc)
    
    def blizzardStates(maxT: Int, startLocs: Vector[(Char, Point)], width: Int, height: Int): Map[Int, Vector[Point]] =
      (1 to maxT).foldLeft((startLocs, Map(0 -> startLocs.map(_._2)))) { (res, t) =>
        val updated = res._1.map(move(width, height))
        (updated, res._2 + (t -> updated.map(_._2)))
      }._2
    
    def blizzardPath(fieldStates: Map[Int, Vector[Point]], start: Point, finish: Point, startTime: Int): Int =

      def go(active: Set[Point], time: Int): Int =
        val nextLocs: Set[Point] = active.flatMap(_.adjacentInclusive)
        if nextLocs(finish) then time
        else if active.isEmpty then { println("ERROR, no path possible") ; time }
        else if time >= 1000 then 9999
        else
          val next: Set[Point] = nextLocs
            .filter((p: Point) => p.x >= 0 && p.y >= 0 && !fieldStates(time).contains(p))
          //println(s"time: $time --> $next")
          go(next, time + 1)

      go(Set(start), startTime)
      
  // TODO: look at the code of JP

  private val width: Int = input.maxBy(_._2.x)._2.x
  private val height: Int = input.maxBy(_._2.y)._2.y
  private val start: Point = Point(1, 0)
  private val end: Point = Point(width - 1, height)
  private val blizzards: Map[Int, Vector[Point]] = Field.blizzardStates(1000, input, width, height)
  private val answer1: Int = Field.blizzardPath(blizzards, start, end, 0)
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")
  


  private val start2: Long =
    System.currentTimeMillis
    
  private val res2: Int = Field.blizzardPath(blizzards, end, start, answer1)
  private val answer2: Int = Field.blizzardPath(blizzards, start, end, res2)
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")

//def blizzardStates(maxT: Int, startLocs: Set[(Char, Point)], width: Int, height: Int): Map[Int, Set[(Char, Point)]] =
//  (1 to maxT).foldLeft((startLocs, Map(0 -> startLocs.map(_._2)))) { (res, t) =>
//    val updated = res._1.map(move(width, height))
//    (updated, res._2 + (t -> updated.map(_._2)))
//  }._2

//val tests = blizzards.toVector.sortBy(_._1).map((_, x) => Point.gridPrintable(x.map(_._2).toVector)
//((p: Point) =>
//  if x.map(_._2)(p) then
//    if x.count(_._2 == p) > 1 then s"${x.count(_._2 == p)}".head else x.filter(_._2 == p).head._1
//  else '.'
//))
//tests.foreach(x =>
//  println("-----")
//  println(x)
//)