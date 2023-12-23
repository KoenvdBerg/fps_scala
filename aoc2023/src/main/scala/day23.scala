import aday23.pathFinder2

import scala.io.*
import math.*
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable
import aoc2022.Algorithms.GraphTraversal.Graph
import aoc2022.Algorithms.GraphTraversal
import aoc2022.Grid2D.Point

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

  case class Path(x: Int, y: Int, prev: (Int, Int), distance: Int):
    def neighbours(curLoc: Char, part: Int = 1): Vector[Path] =
      val ns = Vector(
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

      def go(queue: Queue[Path], res: Vector[Path]): Vector[Path] =
        if queue.isEmpty then res
        else
          val (p, nextq) = queue.dequeue
          if p.x -> p.y == target then go(nextq, res.appended(p))
          else
            val cur: Char = maze(p.y)(p.x)
            val ns: Vector[Path] = p.neighbours(cur, part)
              .filter(pp => maze(pp.y)(pp.x) != '#' && inBounds(pp.x, pp.y))
            go(nextq.enqueueAll(ns), res)

      go(Queue(start), Vector.empty)


    val cache: mutable.Map[Point, Vector[(Point, Int)]] = new mutable.HashMap()
    def memWalkUntilWayPoint(from: Point) = cache.getOrElseUpdate(from, walkUntilWayPoint(from))
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


    def wayPointDAG: Graph[(Point, Vector[Point])] =
      (p: Point, visited: Vector[Point]) =>
        memWalkUntilWayPoint(p)
          .filterNot((np, _) => visited.contains(np))
          .map((np, dist) => (np, visited.appended(p)) -> dist).toMap


    def dijkstraMod(g: Graph[(Point, Vector[Point])])(source: Point, target: Point): Int =

      val active: mutable.PriorityQueue[((Point, Vector[Point]), Int)] =
        mutable.PriorityQueue((source -> Vector.empty[Point], 0))(Ordering.by((f: ((Point, Vector[Point]), Int)) => f._2).reverse)

      @tailrec
      def go(res: Int): Int =
        if active.isEmpty then res
        else
          val (node, dist): ((Point, Vector[Point]), Int) = active.dequeue
          if node._1 == target then
            go(res.max(dist))
          else
            val neighbours: Map[(Point, Vector[Point]), Int] = g(node).map(iii => (iii._1, iii._2 + dist))
            neighbours.foreach(n => active.enqueue(n))
            go(res)

      go(0)





  private val pathFinder: PathFinder = PathFinder(input, 1)
  private val res1 = pathFinder.findPaths
  private val answer1 = res1.maxBy(_.distance).distance
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis


  // too high: 6409
  // test: 6230

  private val pathFinder2 = PathFinder(input, 2)

  private val start: Point = Point(pathFinder2.start.x, pathFinder2.start.y)
  private val target: Point = Point(pathFinder2.target._1, pathFinder2.target._2)
  private val res2 = pathFinder2.dijkstraMod(pathFinder2.wayPointDAG)(start, target)
  private val answer2 = res2
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")


/**
 * Answer day 23 part 1: 1930 [199ms]
 * Answer day 23 part 2: ((Point(105,37),Vector(Point(1,0), Point(17,11), Point(15,31), Point(9,59), Point(41,61),
 * Point(41,83), Point(5,83), Point(11,105), Point(39,129), Point(37,103), Point(63,109), Point(55,135), Point(79,137),
 * Point(83,103), Point(113,109), Point(107,135), Point(131,127), Point(129,111), Point(135,89), Point(101,77), Point(113,55),
 * Point(137,67), Point(127,29), Point(105,15), Point(87,19), Point(61,11), Point(29,9), Point(41,39), Point(55,33),
 * Point(55,57), Point(57,87), Point(89,85), Point(77,57), Point(79,29))),6409) [192307ms]
 */



//
//case class Walk(x: Int, y: Int, seen: Set[(Int, Int)], dist: Int)
//
//def findDistances: Vector[Int] =
//
//  def go(queue: Queue[Walk], res: Vector[Int]): Vector[Int] =
//    if queue.isEmpty then res
//    else
//      val (w, nextq) = queue.dequeue
//      if w.x -> w.y == target then go(nextq, res.appended(w.dist))
//      else
//        val next = wayPointsMap((w.x, w.y))
//        val toEnqueue = next.filterNot((x, y, _) => w.seen.contains((x, y)))
//          .map((x, y, d) => Walk(x, y, w.seen + ((w.x, w.y)), w.dist + d))
//        go(nextq.enqueueAll(toEnqueue), res)
//
//  go(Queue(Walk(start.x, start.y, Set.empty, 0)), Vector.empty)
//def waypointGraph: Graph[Path] =
//  (p: Path) =>
//    val np = p.copy(distance = 0)
//    val next = np.neighbours(maze(np.y)(np.x), part)
//    next
//      .filter(pp => inBounds(pp.x, pp.y))
//      .filter(pp => maze(pp.y)(pp.x) != '#')
//      .map(pp => walkUntilWayPoint(pp))
//      .map(pp => pp -> pp.distance)
//      .toMap

//    lazy val wayPoints: (Map[Path, Int], Map[Path, Path]) = GraphTraversal.dijkstra(waypointGraph)(start)
//    lazy val wayPointsMap: Map[(Int, Int), Set[(Int, Int, Int)]] = wayPoints._2.groupBy(_._2).map((k, v) => (k, v.keySet))
//      .map((k, v) => (k.x, k.y) -> v.map(p => (p.x, p.y, p.distance)))io

//    def wayPointMap: Map[(Point, Point), Int] =
//
////      val seen = mutable.Set[(Point, Point)]()
////          seen.addAll(next.map(p => (p.x, p.y)))
//      //          val nextRes = cur.foldLeft(res) { (r, p) =>
//      //            r.updated((p.x, p.y), next.map(pp => (pp.x, pp.y, pp.distance)))
//      //          }
//
//
//      def go(cur: Set[Point], res: Map[(Point, Point), Int]): Map[(Point, Point), Int] =
//        if cur.isEmpty then res
//        else
//          val next = cur.map(c =>
//            val (found, dist) = walkUntilWayPoint(c)
//            (c, found) -> dist)
//            .filterNot(pp => res.contains((pp._1._1, pp._1._2)) | res.contains(pp._1._2, pp._1._1))
//          println(s"$cur --> $next")
//          val nextRes = res ++ next.toMap
//          go(next.map(_._1._2), nextRes)
//
//      go(Set(Point(start.x, start.y)), Map.empty)