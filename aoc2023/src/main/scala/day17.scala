import scala.io.*
import scala.annotation.tailrec
import aoc2022.Algorithms.GraphTraversal
import aoc2022.Algorithms.GraphTraversal.Graph
import day16.Direction
import day16.Direction.*
import scala.math



object day17 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: Vector[Vector[Int]] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector
      .map(s => s.map(_.toString.toInt).toVector)

  case class Cart(x: Int, y: Int, dir: Direction, sameDir: Int):

    private def neighbours: Vector[Cart] =
      Vector(
        (y - 1, x, North),
        (y + 1, x, South),
        (y, x - 1, West),
        (y, x + 1, East)
      )
        .map((yy, xx, d) => if d == dir then Cart(xx, yy, d, sameDir + 1) else Cart(xx, yy, d, 1))
        .filter(c => c.sameDir <= 3)

    private def ultraNeighbours: Vector[Cart] =
      val ns = Vector(
        (y - 1, x, North),
        (y + 1, x, South),
        (y, x - 1, West),
        (y, x + 1, East)
      )
      if sameDir < 4 then ns.filter(_._3 == dir).map((yy, xx, d) => Cart(xx, yy, d, sameDir + 1))
      else
        ns.map((yy, xx, d) => if d == dir then Cart(xx, yy, d, sameDir + 1) else Cart(xx, yy, d, 1))
          .filter(c => c.sameDir <= 10)


    def currentNeighbours(model: String): Vector[Cart] =
      val ns: Vector[Cart] = if model == "ultra" then ultraNeighbours else neighbours
      dir match
        case North => ns.filter(_.dir != South)
        case South => ns.filter(_.dir != North)
        case East => ns.filter(_.dir != West)
        case West => ns.filter(_.dir != East)


  class LavaNavigator(in: Vector[Vector[Int]]):

    val lava: Vector[Vector[Int]] = in
    val maxX: Int = in.head.length
    val maxY: Int = in.length

    def inBounds(x: Int, y: Int): Boolean = x >= 0 && x < maxX && y >= 0 && y < maxY

    def cartPath(model: String): Graph[Cart] =
      (c: Cart) =>
        val next: Vector[Cart] = c.currentNeighbours(model)
        next
          .filter(c => inBounds(c.x, c.y))
          .map(c => c -> lava(c.y)(c.x)).toMap

    def targets: IndexedSeq[Cart] =
      for
        i <- 1 to 3
        d <- Vector(South, East)
      yield Cart(maxX-1, maxY-1, d, i)

    def ultraTargets: IndexedSeq[Cart] =
      for
        i <- 4 to 10
        d <- Vector(South, East)
      yield Cart(maxX - 1, maxY - 1, d, i)

  private val lavaField: LavaNavigator = LavaNavigator(input)
  private val map: Map[Cart, Int] = GraphTraversal.dijkstra(lavaField.cartPath("normal"))(Cart(0, 0, East, 0))._1
  private val res1: IndexedSeq[Int] = lavaField.targets.flatMap(map.get)
  private val answer1: Int = res1.min
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  // note I choose here for to begin south, but your input might require to start East. Just try them both
  private val map2: Map[Cart, Int] = GraphTraversal.dijkstra(lavaField.cartPath("ultra"))(Cart(0, 0, South, 0))._1
  private val res2: IndexedSeq[Int] = lavaField.ultraTargets.flatMap(map2.get)
  private val answer2: Int = res2.min
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
