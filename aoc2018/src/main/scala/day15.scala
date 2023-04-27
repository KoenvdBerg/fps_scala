
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

    def +(p2: Point): Point = Point(x + p2.x, y + p2.y)

    // breadth first search algorithm
    def -->(target: Point, obstacles: Set[Point]): Option[Vector[Point]] =

      val seen: mutable.Set[Point] = obstacles.to(mutable.Set)
      var queue: mutable.Queue[Vector[Point]] = mutable.Queue[Vector[Point]](Vector(this))

      def loop(): Option[Vector[Point]] =
        if queue.isEmpty then None      // exit condition when no path can be found
        else
          val thisPath = queue.dequeue()
          val thisPoint = thisPath.head
          seen += thisPoint
          if thisPoint == target then Some(thisPath)  // exit condition when target is reached
          else
            val next: Set[Point] = thisPoint
              .adjacent             // compute the adjacent Points
              .diff(seen)           // filter out the points already visited
            queue = queue ++ next.map(n => n +: thisPath)
            loop()
      loop()

  sealed trait Infantry
  case object Goblin extends Infantry
  case object Elve extends Infantry

  case class Soldier(unit: Infantry, loc: Point, hp: Int, pwr: Int):

    def enemyAdjacent(targets: Vector[Soldier]): Boolean =
      val adjacent: Set[Point] = this.loc.adjacent.intersect(targets.map(_.loc).toSet)
      adjacent.nonEmpty

    def nonSelfTargets(army: Vector[Soldier]): Vector[Soldier] =
      army.filter(s => s.unit != this.unit)

    def findClosestTarget(targets: Vector[Soldier], obstacles: Set[Point]): Option[Vector[Point]] =
      val allPaths: Vector[Vector[Point]] = targets.map(_.loc).flatMap(t => this.loc --> (t, obstacles))
      if allPaths.isEmpty then None
      else Some(allPaths.minBy(_.length))

    def move(army: Vector[Soldier], obstacles: Set[Point]): Soldier =
      val targets: Vector[Soldier] = nonSelfTargets(army)
      if enemyAdjacent(targets) then this    // skip expensive computation of pathFinding
      else
        val path: Option[Vector[Point]] = findClosestTarget(targets, obstacles)
        path match
          case Some(p) => Soldier(this.unit, p(p.length-2), this.hp, this.pwr)
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

  def print2dGrid(army: Vector[Soldier], obstacles: Set[Point]): Unit =
    val map: Set[Point] = obstacles | army.map(_.loc).toSet
    val xMax: Int = map.maxBy(_.x).x
    val yMax: Int = map.maxBy(_.y).y

    val grid: Vector[Point] = (for {
      x <- Range(0, xMax+1).toList
      y <- Range(0, yMax+1).toList
    } yield Point(x, y)).toVector

    def go(grid: Vector[Point], n: Int = 0): Unit = grid match
      case c +: t =>
        val s: Option[Soldier] = army.find(_.loc == c)
        val w: Option[Point] = obstacles.find(_ == c)
        if n % (xMax+1) == 0 then println() else ()
        (s, w) match
          case (None, None) => print(" ")
          case (None, Some(_)) => print("#")
          case (Some(s), None) => s.unit match
            case Elve => print("E")
            case Goblin => print("G")
        go(t, n + 1)
      case Vector() => println()
    go(grid)

// TODO: then update the print function to only print using obstacles

  println(infantry)
//  println(simulate(infantry, obstacles))
  print2dGrid(infantry, obstacles)
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
//def pathFinding2(source: Point, target: Point, obstacles: Set[Point]): Option[Path] =
//
//  val queue: mutable.Queue[Path] = mutable.Queue[Path](Path(0, Vector(source)))
//  val seen: mutable.Set[Point] = obstacles.to(mutable.Set)
//
//  def loop(): Option[Path] =
//    if queue.isEmpty then None
//    else
//      val path: Path = queue.dequeue()
//      val thisPoint: Point = path.path.last
//      if thisPoint == target then Some(path)
//      else
//        val next
//
//        for {
//          cc <- thisPoint.adjacent.diff(seen)
//        } do {
//          seen += cc
//          queue.enqueue(Path(path.len + 1, thisPoint +: path.path))
//          loop()
//        }
//        None
//
//  loop()
// Flooding algorithm
//def pathFinding(source: Point, target: Point, obstacles: Set[Point]): Option[Path] =
//
//  def go(current: Point, visited: Set[Point], path: Path): Option[Path] =
//    if current == target then Some(Path(path.len + 1, (current +: path.path).reverse))
//    else
//      println(path)
//      val next: Set[Point] = current
//        .adjacent // compute the adjacent Points
//        .diff(visited) // filter out the points already visited
//      if next.isEmpty then None // if next is empty then this path is a dead end, thus None
//      else
//        // here the final path is computed by appending each current path to the path trajectory, increasing path
//        // length with 1. Also, the current path is added to the list of visited, and thus cannot be reached anymore.
//        val allPaths: Set[Path] = next
//          .flatMap(cc => go(cc, Set(current) ++ visited, Path(path.len + 1, current +: path.path)))
//
//        // here we select the shortest path to final destination
//        if allPaths.isEmpty then None // if no viable path found then no path is possible to target, thus None
//        else
//          Some(allPaths.minBy(p => p._1)) // select path with shortest path length
//
//  go(source, obstacles, Path(0, Vector.empty))