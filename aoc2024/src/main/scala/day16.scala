import aoc2024.BoundedGrid
import aoc2024.BoundedGrid.*
import aoc2024.Algorithms.GraphTraversal
import aoc2024.Algorithms.GraphTraversal.Graph
import jdk.jshell.execution.Util

import scala.io.*
import math.*
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.util.matching.Regex

object day00 extends App:

  private val day: String = "16"

  private val start1: Long =
    System.currentTimeMillis

  private val grid: BoundedGrid =
    val in = Source
      .fromResource(s"day$day.txt")
      .getLines
      .toSeq
    BoundedGrid.fromString(in.mkString, in.head.length)

  case class R(p: (Int, Int), dir: (Int, Int), score: Int, path: Seq[(Int, Int)], steps: Int):
    def neighbours: Seq[R] = Seq(
      R(p + dir, dir, score + 1, path.appended(p), steps + 1),
      R(p + dir.rotate, dir.rotate, score + 1001, path.appended(p), steps + 1),
      R(p + dir.rotateI, dir.rotateI, score + 1001, path.appended(p), steps + 1)
    )

  def step(r: R): Seq[R] = r.neighbours.filterNot(rr => grid.get(rr.p) == '#' || r.path.contains(rr.p))

  def algorithm2(g: BoundedGrid, start: R) =
    case class S(p: (Int, Int), dir: (Int, Int))

    val q: mutable.PriorityQueue[R] = mutable.PriorityQueue(start)(Ordering.by((r: R) => (r.score, r.steps, r.p, r.dir)).reverse)
    val seen: mutable.Map[S, Int] = mutable.Map.empty

    def go(best: Int, acc: Seq[R]): Seq[R] =
      if q.isEmpty then acc
      else
        val cur = q.dequeue()
        if cur.score > seen.getOrElseUpdate(S(cur.p, cur.dir), Int.MaxValue) then go(best, acc)
        else if g.get(cur.p) == 'E' && cur.score <= best then
          go(cur.score, acc.appended(cur))
        else
          val next = step(cur)
          next.foreach(r => q.enqueue(r))
          go(best, acc)

    go(Int.MaxValue, Seq.empty)

  val s = grid.getPointsOf('S').head
  val start = R(s, (1, 0), 0, Seq(s), 0)

  private val answer1 = algorithm2(grid, start)
    .flatMap(_.path).distinct.length + 1
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis
//
//  def dijkstra[N](g: Graph[N])(source: N): (Seq[(N, Int)], Seq[(N, N)]) =
//
//    // unfortunately there isn't an immutable priority queue, so we've to use the mutable one.
//    val active: mutable.PriorityQueue[(N, Int)] = mutable.PriorityQueue((source, 0))(Ordering.by((f: (N, Int)) => f._2).reverse)
//
//    def go(res: Seq[(N, Int)], pred: Seq[(N, N)]): (Seq[(N, Int)], Seq[(N, N)]) =
//      if active.isEmpty then (res, pred)
//      else
//        val node: N = active.dequeue._1 // select the next node with lowest distance thus far
//        val cost: Int = res.find(_._1 == node).get._2
//        val neighbours: Seq[(N, Int)] =
//          (for
//            (n, c) <- g(node) if cost + c < res.find(_._1 == n).map(_._2).getOrElse(Int.MaxValue)
//          // res.getOrElse(n, Int.MaxValue)
//          yield n -> (cost + c)) // update distances
//            .toSeq
//        neighbours.foreach((n: (N, Int)) => active.enqueue(n)) // add next nodes to active nodes
//        val preds: Seq[(N, N)] = neighbours.map((f: (N, Int)) => (f._1, node)).toSeq
//        println(preds)
//        go(res ++ neighbours, pred ++ preds)
//
//    go(Seq(source -> 0), Seq.empty[(N, N)])
//
//  val paths = dijkstra(step)(start)._2
////  println(paths)
//
//  def backTrace(cur: R): Seq[R] =
//    val next = paths.filter(_._1 == cur).map(_._2)
//    println(next)
//    cur +: next.flatMap(backTrace)
//
//  val res2 = backTrace(end)
//  private val answer2 = res2.length
//  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")

//
//def go(q: Queue[R2], acc: Seq[R2]): Seq[R2] =
//  if q.isEmpty then acc
//  else
//    val (cur, nq) = q.dequeue
//    if cur.p == end then go(nq, acc.appended(cur))
//    else if cur.score > maxScore then go(nq, acc)
//    else if cur.path.length > 600 then go(nq, acc)
//    else
//      val next = step2(cur)
//        .map((r2, s) => r2.updateScore(s))
//        .toSeq
//      go(nq.appendedAll(next), acc)
//
//go(Queue(start), Seq.empty)

//
//case class R2(p: (Int, Int), dir: (Int, Int), path: Seq[(Int, Int)], score: Int):
//  def neighbours: Seq[R2] =
//    Seq(
//      R2(p._1 -> (p._2 - 1), (0, -1), path, score),
//      R2((p._1 - 1) -> p._2, (-1, 0), path, score),
//      R2((p._1 + 1) -> p._2, (1, 0), path, score),
//      R2(p._1 -> (p._2 + 1), (0, 1), path, score)
//    ).filterNot(r => r.dir == dir.negate)
//
//  def updateScore(s: Int): R2 = copy(score = this.score + s)
//
//  def compareDir(that: R2): Boolean = dir._1 == that.dir._1 && dir._2 == that.dir._2
//
//def step2(r: R2): Map[R2, Int] =
//  r.neighbours
//    .filterNot(rr => grid.get(rr.p) == '#')
//    .map(rr => rr.copy(path = rr.path.appended(r.p)))
//    .map(rr => if rr.compareDir(r) then rr -> 1 else rr -> 1001)
//    .toMap
//
//
//import scala.collection.mutable
//
//def algorithm2(g: BoundedGrid, start: R2, end: (Int, Int), maxScore: Int) =
//
//  val q: mutable.PriorityQueue[R2] = mutable.PriorityQueue(start)(Ordering.by((r: R2) => r.score).reverse)
//
//  def go(acc: Seq[R2]): Seq[R2] =
//    if q.isEmpty then acc
//    else
//      val cur = q.dequeue()
//      if cur.p == end then
//        println("HIT")
//        go(acc.appended(cur))
//      else if cur.score > maxScore then go(acc)
//      else if cur.path.length > 600 then go(acc)
//      else
//        val next = step2(cur)
//          .map((r2, s) => r2.updateScore(s))
//          .toSeq
//        next.foreach(r2 => q.enqueue(r2))
//        go(acc)
//
//  go(Seq.empty)
//
//val maxScore = answer1
//val res2 = algorithm2(grid, R2(s, (1, 0), Seq.empty, 0), end.p, maxScore)
//println(res2.minBy(_.score))
//
//private val answer2 = res2
//  .filter(_.score == maxScore)
//  .map(_.path)
//  .fold(Seq.empty[(Int, Int)])((r1, r2) => (r1 ++ r2).distinct)
//  .length + 1 // to account for start position
//println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
