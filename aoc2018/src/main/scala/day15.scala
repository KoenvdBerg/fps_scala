import scala.io.*
import math.*
import scala.collection.mutable
import scala.collection.mutable.Stack
import aoc2018.Grid2D.Point
import scala.annotation.tailrec


object day15 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  case class Soldier(unit: Char, loc: Point, hp: Int, pwr: Int):

    def adjacentEnemies(targets: Vector[Soldier]): Vector[Soldier] =
      val coordsEnemies: Set[Point] = this
        .loc
        .adjacent
        .intersect(targets.map(_.loc).toSet)
      targets.filter(t => coordsEnemies.contains(t.loc))

    def nonSelfTargets(army: Vector[Soldier]): Vector[Soldier] =
      army.filter(s => s.unit != this.unit)

    def findNextStep(targets: Vector[Soldier], obstacles: Vector[Point]): Option[Point] =
      val targetLocs: Vector[Point] = targets.map(_.loc)
      val paths: LazyList[Vector[Point]] = this.loc.bfsSearch(targetLocs, obstacles.filter(!targetLocs.contains(_)))
      if paths.isEmpty then None   // no path to any target possible
      else
        val foundPaths: Vector[Vector[Point]] = targetLocs.flatMap(t => paths.find(vp => vp.head == t))
        val minlen: Int = foundPaths.minBy(_.length).length    // due to order in bfs paths may have varying lengths
        val shortestPaths: Vector[Vector[Point]] = foundPaths.filter(_.length == minlen)     // selecting shortest path
        Some(handleMultipleShortPaths(shortestPaths).dropRight(1).last)  // penultimate point is step

    def handleMultipleShortPaths(paths: Vector[Vector[Point]]): Vector[Point] =
      if paths.length <= 1 then paths.head
      else
        val firstInOrder: Point = paths
          .map(i => i(1))         // 2nd elem is point adjacent to target
          .minBy(_.toTuple.swap)  // select the point that is first in reading order
        paths.filter(_(1) == firstInOrder).head

    def move(army: Vector[Soldier], obstacles: Vector[Point]): Soldier =
      val targets: Vector[Soldier] = nonSelfTargets(army)
      if adjacentEnemies(targets).nonEmpty then this    // skip expensive computation of pathFinding when already next to a target
      else
        val step: Option[Point] = findNextStep(targets, obstacles)
        step match
          case Some(p) => Soldier(this.unit, p, this.hp, this.pwr)
          case None    => this

    def attack(army: Vector[Soldier]): Option[Soldier] =
      val nearTargets: Vector[Soldier] = adjacentEnemies(nonSelfTargets(army))
      if nearTargets.isEmpty then None // no targets in reach, return None
      else
        val minHP: Int = nearTargets.minBy(_.hp).hp
        if nearTargets.count(_.hp == minHP) >= 2 then  // handle reading order
          val selS: Soldier = nearTargets.filter(_.hp == minHP).minBy(_.loc.toTuple.swap)
          Some(Soldier(selS.unit, selS.loc, selS.hp - this.pwr, selS.pwr))
        else
          val selS: Soldier = nearTargets.minBy(_.hp)
          Some(Soldier(selS.unit, selS.loc, selS.hp - this.pwr, selS.pwr))



  object Soldier:

    def updateObstacles(obs: Vector[(Point, Char)], old: Soldier, next: Soldier)(action: String): Vector[(Point, Char)] =
      val deletedObs: Vector[(Point, Char)] = obs.filter(_._1 != old.loc)
      action match
        case "remove" => deletedObs
        case "update" => (next.loc, next.unit) +: deletedObs

    def updateArmy(army: Vector[Soldier], hit: Soldier)(action: String): Vector[Soldier] =
      val hitIndex: Int = army.indexWhere(_.loc == hit.loc)  // search if hit soldier is present
      if hitIndex == -1 then army        // hit soldier not present then return unchanged army
      else                               // hit soldier is present, change soldier
        val (mem, t) = army.splitAt(hitIndex)  // hit soldier is in head, mem holds preceding elements
        val newt = (t, action) match                 // idea here is to update the tail with action on soldier
          case (_ +: t, "remove")    => t            // soldier is removed because it's dead
          case (_ +: t, "update")    => hit +: t     // soldier is changed to soldier with reduced HP in hit
          case (Vector(_), "remove") => Vector()     // soldier is removed because it's dead
          case (Vector(_), "update") => Vector(hit)  // soldier is changed to soldier with reduced HP in hit
          case (Vector(), _)         => Vector()
          case _                     => sys.error("please inspect updateArmy error")
        mem ++: newt


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



  def simulate(army: Vector[Soldier], obstacles: Vector[(Point, Char)], nRounds: Int = 0): (Int, Vector[Soldier]) =

    def round(startArmy: Vector[Soldier], obs: Vector[(Point, Char)],
              acc: Vector[Soldier] = Vector.empty): (Vector[(Point, Char)], Vector[Soldier]) =
      startArmy match
        case s +: t =>
          // making a move
          val newS: Soldier = s.move(acc ++: t, obs.map(_._1))
          val newObs: Vector[(Point, Char)] = Soldier.updateObstacles(obs, s, newS)("update")

          // attack
          val hitSoldier: Option[Soldier] = newS.attack(acc ++: t)

          hitSoldier match
            case Some(ss) if ss.hp <= 0 =>
              val newt: Vector[Soldier] = Soldier.updateArmy(t, ss)("remove")
              val newacc: Vector[Soldier] = Soldier.updateArmy(acc, ss)("remove")
              val newObsRem: Vector[(Point, Char)] = Soldier.updateObstacles(obs, ss, ss)("remove")
              round(newt, newObsRem, newS +: newacc)

            case Some(ss) =>
              val newt: Vector[Soldier] = Soldier.updateArmy(t, ss)("update")
              val newacc: Vector[Soldier] = Soldier.updateArmy(acc, ss)("update")
              round(newt, newObs, newS +: newacc)

            case _ =>
              round(t, newObs, newS +: acc)

        case Vector() => (obs, acc)
        case _ => sys.error("round couldn't be figured out, plz investigate")

    if army.count(_.unit == 'G') <= 0 || army.count(_.unit == 'E') <= 0  then (nRounds-1, army)  // exit condition, an army is wiped from existence
    else if army.count(_.unit == 'E') < elveAllies then (nRounds-1, army)  // added for optim part02
    else
      val (nextObs, nextArmy): (Vector[(Point, Char)], Vector[Soldier]) =
        round(army.sortBy(p => p.loc.toTuple.swap), obstacles)
      simulate(nextArmy, nextObs, nRounds + 1)


  private val (rounds, winningForces) = simulate(army, obstacles)
  private val answer1 = rounds * winningForces.map(_.hp).sum
  println(s"Answer day $day part 1: ${answer1} should be: 218272 [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  private val elveAllies = army.count(_.unit == 'E')
  @tailrec
  def sim2(elveAtk: Int = 4): (Int, Vector[Soldier]) =
    val nextArmy: Vector[Soldier] = army.map {
      case Soldier('E', l, _, _) => Soldier('E', l, 200, elveAtk)
      case s@Soldier('G', _, _, _) => s
    }
    val (rounds2, winningForces2) = simulate(nextArmy, obstacles)
    if winningForces2.count(_.unit == 'E') == elveAllies then (rounds2, winningForces2)
    else sim2(elveAtk + 1)

  private val (rounds2, winningForces2) = sim2()
  private val answer2 = rounds2 * winningForces2.map(_.hp).sum
  println(s"Answer day $day part 2: ${answer2} should be: 40861 [${System.currentTimeMillis - start2}ms]")

// storage for the input below as it's the smallest scenario
// that was problematic for me to handle.
//
// ######
// #.G..#
// #.##.#
// #.1.2#
// ######
//
// G to move, should move to Point(3,1) because the '.'
// adjacent to target 2 is first in reading order.
