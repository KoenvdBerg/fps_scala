import scala.io.*
import scala.annotation.tailrec
import aoc2022.Algorithms.GraphTraversal
import aoc2022.Algorithms.GraphTraversal.Graph
import day16.Direction.*

object day16 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: Vector[String] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector

  enum Direction:
    case North, South, West, East

  case class Beam(x: Int, y: Int, dir: Direction):

    def neighbour: Beam = dir match
      case North => copy(y = y - 1)
      case South => copy(y = y + 1)
      case East => copy(x = x + 1)
      case West => copy(x = x - 1)

  class BeamHelper(in: Vector[String]):

    private val mirrors: Vector[String] = in
    private val maxX: Int = in.head.length
    private val maxY: Int = in.length

    def inBounds(x: Int, y: Int): Boolean = x >= 0 && x < maxX && y >= 0 && y < maxY

    def lightPath: Graph[Beam] =
      (b: Beam) =>
        val n = b.neighbour
        if inBounds(n.x, n.y) then
          (mirrors(n.y)(n.x), n.dir) match
            case ('|', East) | ('|', West)   => Map(n.copy(dir = North) -> 1, n.copy(dir = South) -> 1)
            case ('-', North) | ('-', South) => Map(n.copy(dir = East) -> 1, n.copy(dir = West) -> 1)
            case ('/', West)                 => Map(n.copy(dir = South) -> 1)
            case ('/', East)                 => Map(n.copy(dir = North) -> 1)
            case ('/', South)                => Map(n.copy(dir = West) -> 1)
            case ('/', North)                => Map(n.copy(dir = East) -> 1)
            case ('\\', West)                => Map(n.copy(dir = North) -> 1)
            case ('\\', East)                => Map(n.copy(dir = South) -> 1)
            case ('\\', South)               => Map(n.copy(dir = East) -> 1)
            case ('\\', North)               => Map(n.copy(dir = West) -> 1)
            case _                           => Map(n.copy(dir = b.dir) -> 1)
        else Map.empty

    def startingPoints: IndexedSeq[Beam] =
      val westSide = Range(0, maxY).map(i => Beam(-1, i, East))
      val eastSide = Range(0, maxY).map(i => Beam(maxX, i, West))
      val northSide = Range(0, maxX).map(i => Beam(i, -1, South))
      val southSide = Range(0, maxX).map(i => Beam(i, maxY, North))
      southSide ++ westSide ++ eastSide ++ northSide

  private val beamput: BeamHelper = BeamHelper(input)
  private val res1: Set[Beam] = GraphTraversal.dijkstra(beamput.lightPath)(Beam(-1, 0, East))._2.keySet
  private val answer1: Int = res1.map(b => (b.x, b.y)).size
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  private val answer2: Int = beamput.startingPoints.foldLeft(0) { (res: Int, b: Beam) =>
    val t = GraphTraversal.dijkstra(beamput.lightPath)(b)._2.keySet
    val nm = t.map(b => (b.x, b.y)).size
    if nm > res then nm else res
  }
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
