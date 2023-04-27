import scala.io.*
import math.*
import scala.collection.mutable
import scala.collection.mutable.Stack
import aoc2018.Grid2D.Point


object day15 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  case class Soldier(unit: Char, loc: Point, hp: Int, pwr: Int):

    def enemyAdjacent(targets: Vector[Soldier]): Boolean =
      val adjacent: Set[Point] = this.loc.adjacent().intersect(targets.map(_.loc).toSet)
      adjacent.nonEmpty

    def nonSelfTargets(army: Vector[Soldier]): Vector[Soldier] =
      army.filter(s => s.unit != this.unit)

    def findClosestTarget(targets: Vector[Soldier], obstacles: Vector[Point]): Option[Vector[Point]] =
      val allPaths: Vector[Vector[Point]] =
        targets
          .map(_.loc)
          .flatMap(t => this.loc --> (t, obstacles.filter(_ != t)))
      if allPaths.isEmpty then None
      else
        val minLen: Int = allPaths.minBy(_.length).length
        val selection: Vector[Point] = allPaths.filter(_.length == minLen).minBy(p => p.map(_.y).sum)
        Some(selection)

    def move(army: Vector[Soldier], obstacles: Vector[Point]): Soldier =
      val targets: Vector[Soldier] = nonSelfTargets(army)
      if enemyAdjacent(targets) then this    // skip expensive computation of pathFinding
      else
        val path: Option[Vector[Point]] = findClosestTarget(targets, obstacles)
        path match
          case Some(p) => Soldier(this.unit, p(p.length-2), this.hp, this.pwr)
          case None    => this


  private val (army, obstacles): (Vector[Soldier], Vector[(Point, Char)]) =

    def parseArmy(s: Char, x: Int, y: Int): Option[Soldier] = s match
      case 'G' => Some(Soldier('G', Point(x, y), 200, 3))
      case 'E' => Some(Soldier('E', Point(x, y), 200, 3))
      case _ => None

    def parseObstacles(s: Char, x: Int, y: Int): Option[(Point, Char)] = s match
      case '.' => None
      case _   => Some((Point(x, y), s))

    val infile = Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector
      .zipWithIndex
    (
      infile.flatMap((ss, y) => ss.zipWithIndex.flatMap((cc, x) => parseArmy(cc, x, y))),
      infile.flatMap((ss, y) => ss.zipWithIndex.flatMap((cc, x) => parseObstacles(cc, x, y)))
    )

  def updateObstacles(obs: Vector[(Point, Char)], old: Soldier, next: Soldier): Vector[(Point, Char)] =
    val deletedObs: Vector[(Point, Char)] = obs.filter(_._1 != old.loc)
    (next.loc, next.unit) +: deletedObs


  def simulate(army: Vector[Soldier], obstacles: Vector[(Point, Char)], nRounds: Int): Vector[Soldier] =

    def round(startArmy: Vector[Soldier], obs: Vector[(Point, Char)],
              acc: Vector[Soldier] = Vector.empty): (Vector[(Point, Char)], Vector[Soldier]) =

      startArmy match
        case s +: t =>
          val newS: Soldier = s.move(acc ++: t, obs.map(_._1))
          val newObs: Vector[(Point, Char)] = updateObstacles(obs, s, newS)
          round(t, newObs, newS +: acc)
        case Vector() => (obs, acc)
        case _ => sys.error("round couldn't be figured out, plz investigate")

    if nRounds <= 0 then army
    else
      Point.print2dGrid(obstacles)
      val (nextObs, nextArmy): (Vector[(Point, Char)], Vector[Soldier]) =
        round(army.sortBy(p => p.loc.toTuple.swap), obstacles)
      simulate(nextArmy, nextObs, nRounds - 1)



  simulate(army, obstacles, 4)


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

//def simulate(army: Vector[Soldier], obstacles: Vector[(Point, Char)], nRounds: Int): Vector[Soldier] =
//
//  def round(startArmy: Vector[Soldier], acc: Vector[Soldier] = Vector.empty): Vector[Soldier] =
//
//    startArmy match
//      case s +: t =>
//        val newS: Soldier = s.move(acc ++: t, obstacles.map(_._1))
//        round(t, newS +: acc)
//      case Vector() => acc
//
//  if nRounds <= 0 then army
//  else
//    Point.print2dGrid(obstacles)
//    val updated: Vector[Soldier] = round(army.sortBy(p => p.loc.toTuple))
//    val nextObstacles: Vector[(Point, Char)] = obstacles.filter(o => ".#".contains(o._2)) ++ updated.map(f => (f.loc, f.unit))
//    simulate(updated, nextObstacles, nRounds - 1)