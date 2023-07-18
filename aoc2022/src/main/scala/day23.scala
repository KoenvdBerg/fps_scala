import aoc2022.Grid2D

import scala.io.*
import math.*
import scala.annotation.tailrec
import aoc2022.Grid2D.Point
import aoc2022.VectorUtils.rotateVector

/**
 * PART 01:
 *
 * Easy one. No special tricks just implementing the business rules for moving of the elves. 
 *
 * PART 02:
 * 
 * Reused part01, but made a new simulation function that runs until the end. 
 *
 */


object day23 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis
  private val elves: Set[Point] =
    def parseElve(s: Char, x: Int, y: Int): Option[Point] =
      s match
        case '#' => Some(Point(x, y))
        case _ => None

    val infile = Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList
      .zipWithIndex

      infile.flatMap((ss: String, y: Int) => ss.zipWithIndex.flatMap((cc: Char, x: Int) => parseElve(cc, x, y))).toSet

  
  enum Direction: 
    case North, South, West, East
    
  object Direction: 
    
    def getDirections(p: Point, dir: Direction): Vector[Point] = dir match
      case North => Vector(Point(p.x - 1, p.y - 1), Point(p.x, p.y - 1), Point(p.x + 1, p.y - 1))
      case South => Vector(Point(p.x - 1, p.y + 1), Point(p.x, p.y + 1), Point(p.x + 1, p.y + 1))
      case West  => Vector(Point(p.x - 1, p.y - 1), Point(p.x - 1, p.y), Point(p.x - 1, p.y + 1))
      case East  => Vector(Point(p.x + 1, p.y - 1), Point(p.x + 1, p.y), Point(p.x + 1, p.y + 1))
    
    def getMoves(elves: Set[Point], directions: Vector[Direction]): Map[Point, Point] =
      val allConsiderations: Map[Point, Point] = elves.map(e => e -> consider(e, elves, directions)).toMap
      val counts: Map[Point, Int] = allConsiderations
        .groupBy(_._2)
        .map(p => (p._1, p._2.size))
      allConsiderations
        .filterNot((k, v) => k == v)       // elve moving to same spot is removed
        .filter((_, v) => counts(v) == 1)  // elve moving to spot that other elve also considers is removed
      
    def consider(elve: Point, allElves: Set[Point], directions: Vector[Direction]): Point =
      val togo: Vector[Point] = directions.flatMap((dir: Direction) => 
        val options: Vector[Point] = getDirections(elve, dir)
        val canGo: Boolean = options.count(p => allElves(p)) == 0 // elve can go if all three spots are emtpy
        if canGo then Vector(options(1)) else Vector.empty
      )
      if togo.length == 4 then elve else togo.headOption.getOrElse(elve)  // if all surrounding spots are empty, elve doesn't move
      
    def simulate(nRounds: Int, elves: Set[Point], dirs: Vector[Direction]): Set[Point] =
      if nRounds <= 0 then elves
      else 
        val toMove: Map[Point, Point] = getMoves(elves, dirs)
        val next = toMove.foldLeft(elves)((op: Set[Point], in: (Point, Point)) => (op - in._1) + in._2)
        simulate(nRounds - 1, next, rotateVector(1, dirs))

    def simulateTillEnd(nRounds: Int, elves: Set[Point], dirs: Vector[Direction]): Int =
      if nRounds % 100 == 0 then println(s"round $nRounds")
      val toMove: Map[Point, Point] = getMoves(elves, dirs)
      if toMove.isEmpty then nRounds
      else
        val next = toMove.foldLeft(elves)((op: Set[Point], in: (Point, Point)) => (op - in._1) + in._2)
        simulateTillEnd(nRounds + 1, next, rotateVector(1, dirs))
        
    def score(elves: Set[Point]): Int =
      val width: Int = elves.maxBy(_.x).x - elves.minBy(_.x).x + 1
      val height: Int = elves.maxBy(_.y).y - elves.minBy(_.y).y + 1
      width * height - elves.size
    
  
  import Direction.*
  private val res1 = Direction.simulate(10, elves, Vector(North, South, West, East))
  private val answer1 = Direction.score(res1)
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  private val answer2 = Direction.simulateTillEnd(1, elves, Vector(North, South, West, East))
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
