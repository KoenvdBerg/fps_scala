
/**
 * IDEAS:
 *
 * move order:
 * 1. scan and attack if possible
 * 2. no attack possible, looking to move
 *
 * round ends after all units have done something that round
 *
 * move function contains:
 * - [X] blocked by walls
 * - [ ] blocked by other enemies
 * - [ ] check if adjacent to an enemy as to skip expensive computation of pathfinding
 * - [ ] move respecting reading order if tied distances
 * - [X] no move possible --> just stay where it is and and turn
 *
 * attack function contains:
 * - select target with fewest HP, tied --> respect reading order
 * - target dies, square becomes Open and creature vanishes
 */

import scala.io.*
import math.*
import scala.collection.mutable
import scala.collection.mutable.Stack


object day15 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  case class Point(x: Int, y: Int):
    def adjacent: Set[Point] =
      Set(
        Point(x - 1, y),
        Point(x + 1, y),
        Point(x, y - 1),
        Point(x, y + 1)
      )

    def add(p2: Point): Point = Point(x + p2.x, y + p2.y)

  case class Path(len: Int, path: Vector[Point])

  // Flooding algorithm
  def pathFinding(source: Point, target: Point, obstacles: Set[Point]): Option[Path] =

    def go(current: Point, visited: Set[Point], path: Path): Option[Path] =
      if current == target then Some(Path(path.len + 1, (current +: path.path).reverse))
      else
        val next: Set[Point] = current
          .adjacent                  // compute the adjacent Points
          .diff(visited)             // filter out the points already visited
        if next.isEmpty then None    // if next is empty then this path is a dead end, thus None
        else
          // here the final path is computed by appending each current path to the path trajectory, increasing path
          // length with 1. Also, the current path is added to the list of visited, and thus cannot be reached anymore.
          val allPaths: Set[Path] = next
            .flatMap(cc => go(cc, Set(current) ++ visited, Path(path.len + 1, current +: path.path)))

          // here we select the shortest path to final destination
          if allPaths.isEmpty then None  // if no viable path found then no path is possible to target, thus None
          else
            Some(allPaths.minBy(p => p._1))  // select path with shortest path length

    go(source, obstacles, Path(0, Vector.empty))


  sealed trait Infantry
  case object Goblin extends Infantry
  case object Elve extends Infantry

  case class Soldier(unit: Infantry, loc: Point, hp: Int, pwr: Int):

    def enemyAdjacent(targets: Vector[Soldier]): Boolean =
      val adjacent: Set[Point] = this.loc.adjacent.intersect(targets.map(_.loc).toSet)
      adjacent.nonEmpty

    def nonSelfTargets(army: Vector[Soldier]): Vector[Soldier] =
      army.filter(s => s.unit != this.unit)

    def findClosestTarget(targets: Vector[Soldier], obstacles: Set[Point]): Option[Path] =
      val allPaths = targets.map(_.loc).flatMap(t => pathFinding(this.loc, t, obstacles))
      if allPaths.isEmpty then None
      else Some(allPaths.minBy(_.len))

    def move(army: Vector[Soldier], obstacles: Set[Point]): Soldier =
      val targets = nonSelfTargets(army)
      if enemyAdjacent(targets) then this    // skip expensive computation of pathFinding
      else
        val path = findClosestTarget(targets, obstacles)
        path match
          case Some(p) => Soldier(this.unit, p.path(1), this.hp, this.pwr)
          case None    => this


  private val (infantry, obstacles): (Vector[Soldier], Set[Point]) =

    def parseInfantry(s: Char, x: Int, y: Int): Option[Soldier] = s match
      case 'G' => Some(Soldier(Goblin, Point(x, y), 300, 3))
      case 'E' => Some(Soldier(Elve, Point(x, y), 300, 3))
      case _ => None

    def parseObstacles(s: Char, x: Int, y: Int): Option[Point] = s match
      case '#' => Some(Point(x, y))
      case _ => None

    val infile = Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector
      .zipWithIndex
    (
      infile.flatMap((ss, y) => ss.zipWithIndex.flatMap((cc, x) => parseInfantry(cc, x, y))),
      infile.flatMap((ss, y) => ss.zipWithIndex.flatMap((cc, x) => parseObstacles(cc, x, y))).toSet
    )

  def simulate(army: Vector[Soldier], obstacles: Set[Point]): Vector[Soldier] =

    def round(startArmy: Vector[Soldier], acc: Vector[Soldier] = Vector.empty): Vector[Soldier] =

    // TODO: make sure that soldiers are also objects

      startArmy match
        case s +: t =>
          val newS: Soldier = s.move(acc ++: t, obstacles)
          round(t, newS +: acc)
        case Vector() => acc.sortBy(p => (p.loc.x, p.loc.y))

    round(army.sortBy(p => (p.loc.x, p.loc.y)))


  println(infantry)
  println(simulate(infantry, obstacles))
  private val answer1 = None
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  private val answer2 = None
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")

// YARD:
//sealed trait Infantry:
//
//  def surrounding(field: Vector[Infantry]): Vector[Infantry] = this match
//    case Goblin(_, _, loc) => field.filter(_ == Elve).filter()
//
//case class Goblin(hp: Int, pwr: Int, loc: Point) extends Infantry
//
//case class Elve(hp: Int, pwr: Int, loc: Point) extends Infantry
