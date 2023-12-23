import scala.io.*
import math.*
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable
import aoc2022.Algorithms.GraphTraversal.Graph
import aoc2022.Grid2D.Point

object day23 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: Vector[String] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector

  case class Path(x: Int, y: Int, prev: (Int, Int), distance: Int):

    def neighbours(curLoc: Char, part: Int = 1): Vector[Path] =
      val ns: Vector[Path] = Vector(
        (x + 1, y),
        (x - 1, y),
        (x, y - 1),
        (x, y + 1)
      ).map((xx, yy) => Path(xx, yy, (x, y), distance + 1))
      val logicalNext: Vector[Path] = curLoc match
        case '>' if part == 1 => ns.take(1)
        case 'v' if part == 1 => ns.takeRight(1)
        case _ => ns

      logicalNext.filter(pp => (pp.x, pp.y) != prev)

  class PathFinder(maze: Vector[String], part: Int):

    private val maxX: Int = maze.head.length
    private val maxY: Int = maze.length
    val start: Path = Path(1, 0, (1, -1), 0)
    val target: (Int, Int) = (maxX - 2, maxY - 1)

    def inBounds(x: Int, y: Int): Boolean = x >= 0 && x < maxX && y >= 0 && y < maxY

    def findPaths: Vector[Path] =

      @tailrec
      def go(queue: Queue[Path], res: Vector[Path]): Vector[Path] =
        if queue.isEmpty then res
        else
          val (p: Path, nextq: Queue[Path]) = queue.dequeue
          if p.x -> p.y == target then go(nextq, res.appended(p))
          else
            val cur: Char = maze(p.y)(p.x)
            val ns: Vector[Path] = p.neighbours(cur, part)
              .filter(pp => maze(pp.y)(pp.x) != '#' && inBounds(pp.x, pp.y))
            go(nextq.enqueueAll(ns), res)

      go(Queue(start), Vector.empty)


    val cache: mutable.Map[Point, Vector[(Point, Int)]] = new mutable.HashMap()
    def memWalkUntilWayPoint(from: Point): Vector[(Point, Int)] = cache.getOrElseUpdate(from, walkUntilWayPoint(from))
    def walkUntilWayPoint(from: Point): Vector[(Point, Int)] =

      @tailrec
      def recur(q: Queue[Path], res: Vector[Path]): Vector[Path] =

        if q.isEmpty then res
        else
          val (p, nextq) = q.dequeue
          if p.x -> p.y == target then recur(nextq, res.appended(p))
          else if p.x -> p.y == (start.x, start.y) && from != Point(start.x, start.y) then recur(nextq, res.appended(p))
          else
            val cur: Char = maze(p.y)(p.x)
            val ns: Vector[Path] = p.neighbours(cur, part)
              .filter(pp => inBounds(pp.x, pp.y))
              .filter(pp => maze(pp.y)(pp.x) != '#')
            if ns.length >= 2 && Point(p.x, p.y) != from then recur(nextq, res.appended(p))
            else
              recur(nextq.appendedAll(ns), res)

      val res = recur(Queue(Path(from.x, from.y, (-1, -1), 0)), Vector.empty)
      res.map(p => Point(p.x, p.y) -> p.distance)


    def wayPointDAG: Graph[(Point, Set[Point])] =
      (p: Point, visited: Set[Point]) =>
        memWalkUntilWayPoint(p)
          .filterNot((np, _) => visited(np))
          .map((np, dist) => (np, visited + p) -> dist).toMap


    def findLongestPath(g: Graph[(Point, Set[Point])])(source: Point, target: Point): Int =

      val active: mutable.PriorityQueue[((Point, Set[Point]), Int)] =
        mutable.PriorityQueue((source -> Set.empty[Point], 0))(Ordering.by((f: ((Point, Set[Point]), Int)) => f._2).reverse)

      @tailrec
      def go(res: Int): Int =
        if active.isEmpty then res
        else
          val (node, dist): ((Point, Set[Point]), Int) = active.dequeue
          if node._1 == target then
            go(res.max(dist))
          else
            val neighbours: Vector[((Point, Set[Point]), Int)] = g(node).map(iii => (iii._1, iii._2 + dist)).toVector
            val i: Int = neighbours.indexWhere(iii => iii._1._1 == target)
            val trueN: Vector[((Point, Set[Point]), Int)] = if i == -1 then neighbours else Vector(neighbours(i))
            trueN.foreach(n => active.enqueue(n))
            go(res)

      go(0)


  private val pathFinder: PathFinder = PathFinder(input, 1)
  private val res1: Vector[Path] = pathFinder.findPaths
  private val answer1: Int = res1.maxBy(_.distance).distance
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  private val pathFinder2 = PathFinder(input, 2)
  private val start: Point = Point(pathFinder2.start.x, pathFinder2.start.y)
  private val target: Point = Point(pathFinder2.target._1, pathFinder2.target._2)
  private val answer2 = pathFinder2.findLongestPath(pathFinder2.wayPointDAG)(start, target)
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
