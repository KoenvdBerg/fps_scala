import scala.io.*
import math.*
import scala.annotation.tailrec
import aoc2022.Grid2D.Point
import aoc2022.Algorithms.BFS.*
import jdk.javadoc.internal.doclets.toolkit.util.DocFinder.Input

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


object day24 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: Set[Wall] =

    def parse(c: Char, x: Int, y: Int): Option[Wall] = c match
      case '>' => Some(Wall(Point(x, y), Point(1, 0)))  // blizzard is a moving wall
      case '<' => Some(Wall(Point(x, y), Point(-1, 0)))
      case '^' => Some(Wall(Point(x, y), Point(0, -1)))
      case 'v' => Some(Wall(Point(x, y), Point(0, 1)))
      case '#' => Some(Wall(Point(x, y), Point(0, 0)))  // # is a non-moving wall
      case _   => None


    val infile = Source
      .fromResource(s"day$day.txt")
      .getLines
      .zipWithIndex
      .toList

    infile.flatMap((ss: String, y: Int) => ss.zipWithIndex.flatMap((cc: Char, x: Int) => parse(cc, x, y))).toSet

  case class Wall(loc: Point, dir: Point):

    def move(width: Int, height: Int): Wall =
      if dir == Point(0, 0) then this
      else
        val nextLoc: Point = loc + dir
        dir match
          case Point(0, -1) if nextLoc.y == 0 => copy(loc = Point(loc.x, height - 1))
          case Point(0, 1) if nextLoc.y == height => copy(loc = Point(loc.x, 1))
          case Point(1, 0) if nextLoc.x == width => copy(loc = Point(1, loc.y))
          case Point(-1, 0) if nextLoc.x == 0 => copy(loc = Point(width - 1, loc.y))
          case _ => copy(loc = nextLoc)

  case class Player(loc: Point, time: Int)

  object Player:

    val width: Int = input.maxBy(_.loc.x).loc.x
    val height: Int = input.maxBy(_.loc.y).loc.y

    def wallStates(maxT: Int, input: Set[Wall]): Map[Int, Set[Point]] =
      (1 to maxT).foldLeft((input, Map(0 -> input.map(_.loc)))) { (res, t) =>
        val updated = res._1.map(_.move(width, height))
        (updated, res._2 + (t -> updated.map(_.loc)))
      }._2

    def blizzardPath(allWalls: Map[Int, Set[Point]]): Path[Player] = (p: Player) =>
      val nextWalls: Set[Point] = allWalls.getOrElse(p.time + 1, sys.error("state not available"))
      val nextLocs: Set[Point] = for {
        n <- p.loc.adjacent + p.loc
        if !nextWalls(n) && n.x >= 0 && n.y >= 0 && n.x <= width && n.y <= height
      } yield n
      nextLocs.map((n: Point) => Player(n, p.time + 1))


  private val start: Player = Player(Point(1, 0), 0)
  private val states = Player.wallStates(1000, input)
  // TODO: the BFS algorithm can just track the amount of time it took to go to the path, no backtracing required (makes it simpler)
  // TODO: let BFS return an integer of steps
  private val res1: Option[List[Player]] = shortestPath(Player.blizzardPath(states))(start, (p: Player) => p.loc == Point(Player.width - 1, Player.height))
  private val answer1 = res1.get.length - 1
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")



  private val start2: Long =
    System.currentTimeMillis

  private val answer2 = None
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
