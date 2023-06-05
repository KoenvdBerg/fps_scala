import scala.io.*
import aoc2018.Grid2D.Point
import day22.Gear.{ClimbingGear, Neither}

import scala.collection.mutable

/**
 *
 * PART 1:
 *
 * To solve this I implemented the requirements. The difficult part is to get the coordinates right that are not at
 * x=0 or y=0. The first impulse might be to simply implement a recursive solution for these coordinates, but that
 * leads to a big slowdown performance wise. Memoization might be an option, but I opted for a quick algorithm,
 * building the cave up in layers of x slices. First the layer at y=0, then the next at y=1, using the previous layer.
 *
 * This leads to an implementation that solves part 01 in 8ms.
 *
 * PART 2:
 *
 * For part02 I used the previously implemented Dijkstra algorithm (in the utils.scala file). This algorithm could be
 * used to quickly find the correct quickest path to the target. It needs a function that if you input a node, it
 * returns the next nodes to explore. This function is implemented in caveGraph() and works by finding the neighbours
 * for a specific node index. It works using a flat version of the riskMap, so no Vector of Vectors, but a singular
 * Vector with the index as the coordinate.
 *
 * The equipGear() function takes care of making nodes with switched gear while moving to a different area. This takes
 * 1 + 7 = 8 minutes. Also, a node to switch to a Torch in-place is always added to make sure that at the target location
 * this switch can be made, if needed, like in the example.
 *
 * Everything runs in under a second.
 *
 */

object day22 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val (depth, goal): (Int, Point) =
    val in: String = Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList
      .mkString(" ")

    in match
      case s"depth: $d target: $x,$y" => (d.toInt, Point(x.toInt, y.toInt))
      case _                          => sys.error("BOOM")

  def getErosion(geoIndex: Int): Int = (geoIndex + depth) % 20183

  def erosionMap(targetX: Int, targetY: Int)(extraBorder: Int): Vector[Vector[Int]] =

    /**
     * This function is essentially a State monad, with the state being the index of the previous layer
     * and the previous erosion.
     */
    def getSliceAtY(above: Vector[Int], acc: Vector[Int], y: Int, x: Int = 1): Vector[Int] =
      if x > (targetX + extraBorder) then
        acc.reverse
      else
        val e: Int = if y == targetY && x == targetX
        then getErosion(0)          // special case for target
        else getErosion(acc.head * above(x))  // normal case
        getSliceAtY(above, e +: acc, y, x + 1)

    def go(y: Int, acc: Vector[Vector[Int]] = Vector.empty[Vector[Int]]): Vector[Vector[Int]] = y match
      case yf if yf > (targetY + extraBorder) => acc.reverse
      case 0    =>
        val xSlice: Vector[Int] = Range(0, targetX + extraBorder + 1).map((i: Int) => getErosion(i * 16807)).toVector
        go(y + 1, xSlice +: acc)
      case _    =>
        val xSlice: Vector[Int] = getSliceAtY(acc.head, Vector(getErosion(y * 48271)), y)
        go(y + 1, xSlice +: acc)

      go(0)



  val erosionLevels: Int => Vector[Vector[Int]] = erosionMap(goal.x, goal.y)
  private val answer1 = erosionLevels(0).flatMap((layer: Vector[Int]) => layer.map((er: Int) => er % 3)).sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  enum Gear:
    case ClimbingGear, Torch, Neither

  import aoc2018.FlatGrid
  import aoc2018.Algorithms.Dijkstra.*
  import Gear.*
  case class Node(i: Int, gear: Gear)


  /**
   * Which gear traverse which terrain?
   * climbing gear => 0 | 1
   * torch         => 0 | 2
   * neither       => 1 | 2
   *
   * Which terrain which gear required?
   * rocky  0 => climbing gear | torch
   * wet    1 => climbing gear | neither
   * narrow 2 => torch | neither
   */
  def equipGear(equipped: Gear, currentTerrain: Int, nextTerrain: Int): (Gear, Int) =
    (equipped, currentTerrain, nextTerrain) match
      case (ClimbingGear, 0, 2) => Torch -> 8
      case (ClimbingGear, 1, 2) => Neither -> 8
      case (Torch, 0, 1)        => ClimbingGear -> 8
      case (Torch, 2, 1)        => Neither -> 8
      case (Neither, 1, 0)      => ClimbingGear -> 8
      case (Neither, 2, 0)      => Torch -> 8
      case _                    => equipped -> 1

  def caveGraph(cave: Vector[Int], width: Int): Graph[Node] =
    (n: Node) =>
      val neighbours: Vector[Int] = FlatGrid.neighbours4(n.i, width, cave.length)
      if neighbours.isEmpty then Map.empty
      else
        val next: Vector[(Node, Int)] = neighbours
          .map((i: Int) =>
            val (nextGear, time): (Gear, Int) = equipGear(n.gear, cave(n.i), cave(i))  // transform neighbours to nodes with correct gear
            Node(i, nextGear) -> time)
        next.toMap + (Node(n.i, Torch) -> 7)  // always add a node that makes it possible to switch inplace to a torch with time 7


  val extraSpace: Int = 50
  val riskMap = erosionLevels(extraSpace).flatMap((layer: Vector[Int]) => layer.map((er: Int) => er % 3))
  val width: Int = goal.x + extraSpace + 1
  val targetIndex: Int = width * goal.y + goal.x // computing target index of point T in flat riskMap

  // nice print of the cave system
  val printableCave: String = FlatGrid.printFlatGrid(riskMap.updated(targetIndex, 3).updated(0, 4), width) {
    case 0 => '.'
    case 1 => '='
    case 2 => '|'
    case 3 => 'T'
    case 4 => 'M'
  }
  // println(printableCave)

  val graph: Graph[Node] = caveGraph(riskMap, width)
  private val answer2 = shortestDistance(graph)(Node(0, Torch), Node(targetIndex, Torch))
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
