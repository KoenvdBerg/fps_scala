import scala.io.*
import math.*
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import aoc2022.Algorithms.GraphTraversal.Graph
import aoc2022.Algorithms.GraphTraversal

object aday23 extends App:

  private val day: String =
    this.getClass.getName.drop(4).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: Vector[String] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector

  class PathFinder(maze: Vector[String], part: Int):
    private val maxX: Int = maze.head.length
    private val maxY: Int = maze.length
    val start: Path = Path(1, 0, (1, -1), 0)
    val target: (Int, Int) = (maxX - 2, maxY - 1)

    def inBounds(x: Int, y: Int): Boolean = x >= 0 && x < maxX && y >= 0 && y < maxY

    case class Path(x: Int, y: Int, prev: (Int, Int), distance: Int):
      def neighbours(curLoc: Char): Vector[Path] =
        val ns = Vector(
          (x + 1, y),
          (x - 1, y),
          (x, y - 1),
          (x, y + 1)
        ).map((xx, yy) => Path(xx, yy, (x, y), distance + 1))
        val logicalNext: Vector[Path] = curLoc match
          case '>' if part == 1 => ns.take(1)
          case 'v' if part == 1 => ns.takeRight(1)
          case _   => ns

        logicalNext.filter(pp => (pp.x, pp.y) != prev)

    def findPaths: Vector[Path] =

      def go(queue: Queue[Path], res: Vector[Path]): Vector[Path] =
        if queue.isEmpty then res
        else
          val (p, nextq) = queue.dequeue
          if p.x -> p.y == target then go(nextq, res.appended(p))
          else
            val cur: Char = maze(p.y)(p.x)
            val ns: Vector[Path] = p.neighbours(cur)
              .filter(pp => maze(pp.y)(pp.x) != '#' && inBounds(pp.x, pp.y))
            go(nextq.enqueueAll(ns), res)

      go(Queue(start), Vector.empty)


    @tailrec
    final def walkUntilWayPoint(p: Path): Path =
      if p.x -> p.y == target then p
      else if p.x -> p.y == (start.x, start.y) then p
      else
        val cur: Char = maze(p.y)(p.x)
        val ns: Vector[Path] = p.neighbours(cur)
          .filter(pp => maze(pp.y)(pp.x) != '#' && inBounds(pp.x, pp.y))
        if ns.length >= 2 then p
        else walkUntilWayPoint(ns.head)

    def mazeGraph: Graph[Path] =
      (p: Path) =>
        val np = p.copy(prev = (-1, -1), distance = 0)
        val next = np.neighbours(maze(np.y)(np.x))
        next
          .filter(pp => inBounds(pp.x, pp.y))
          .filter(pp => maze(pp.y)(pp.x) != '#')
          .map(pp => walkUntilWayPoint(pp))
          .map(pp => pp.copy(prev = (-1, -1)) -> pp.distance)
          .toMap






  private val pathFinder: PathFinder = PathFinder(input, 1)
  private val res1 = pathFinder.findPaths
  private val answer1 = res1.maxBy(_.distance).distance
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis


  val pathFinder2 = PathFinder(input, 2)
  private val res2 = GraphTraversal.dijkstra(pathFinder2.mazeGraph)(pathFinder2.start)

  //val test2 = GraphTraversal.shortestPath(pathFinder2.mazeGraph)(pathFinder2.start, pathFinder2.target)


  println(res2._2.toVector.map((k, v) => (v, k)).groupBy(_._1).map((k, v) => (k, v.map(_._2))).mkString("\n"))

//  private val answer2 = res2.maxBy(_.distance).distance
//  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
