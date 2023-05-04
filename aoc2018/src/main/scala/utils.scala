package aoc2018

import scala.annotation.tailrec
import scala.collection.mutable

object Grid2D:

  case class Point(x: Int, y: Int):
    def adjacent: Set[Point] =
        Set(
          Point(x, y - 1),
          Point(x - 1, y),
          Point(x + 1, y),
          Point(x, y + 1)
        )

    def adjacentDown(obstacles: Set[Point]): Set[Point] =
//      val below = obstacles.contains(Point(x, y+1))
//      if below then Set(
//        Point(x - 1, y),
//        Point(x + 1, y)
//      ) else Set(
//        Point(x, y + 1)
//      )

      Set(
        Point(x, y + 1),
        Point(x - 1, y),
        Point(x + 1, y)
      )



    def toTuple: (Int, Int) = (this.x, this.y)

    def +(p2: Point): Point = Point(x + p2.x, y + p2.y)

    def <(t: Point): Boolean = this.x < t.x && this.y < t.y

    def bfsSearch(targets: Vector[Point], obstacles: Vector[Point]): LazyList[Vector[Point]] =
      import Algorithms.bfs
      val seen: mutable.Set[Point] = obstacles.to(mutable.Set)

      def search: Vector[Point] => LazyList[Vector[Point]] =
        (p: Vector[Point]) =>
          val thisPoint: Point = p.head
          val directions: Set[Point] = thisPoint.adjacent.diff(seen)
          val next: Seq[Vector[Point]] = directions.map(n => n +: p).toSeq
          seen += thisPoint
          directions.map(seen += _)
          LazyList(next: _*)

      def earlyExit: Vector[Point] => Boolean = (p: Vector[Point]) => targets.contains(p.head)

      bfs(LazyList(Vector(this)))(search, earlyExit)


  object Point:

    def print2dGrid(obstacles: Vector[(Point, Char)], default: Char = '.'): Unit =
      val xMax: Int = obstacles.maxBy(_._1.x)._1.x

      def go(obs: Vector[(Point, Char)], x: Int = 0, y: Int = 0): Unit = obs match
        case ob +: t =>
          if x == xMax + 1 then {println(); go(obs, 0, y + 1)}
          else if x == ob._1.x && y == ob._1.y then {print(ob._2); go(t, x + 1, y)}
          else {print(default); go(obs, x + 1, y)}
        case Vector() => println()

      go(obstacles.sortBy(_._1.toTuple.swap))


object Algorithms:

  // Breath first search algorithm, generalized with early exit condition
  // Inspired from: https://stackoverflow.com/questions/41347337/how-to-implement-breadth-first-search-in-scala-with-fp
  @tailrec
  final def bfs[A](queue: LazyList[A])(f: A => LazyList[A], exit: A => Boolean): LazyList[A] =
    if queue.isEmpty then queue
    else if exit(queue.head) then queue
    else bfs(queue.tail ++ f(queue.head))(f, exit)

  @tailrec
  def bfsPriority[A](queue: LazyList[A])(f: A => LazyList[A], exit: A => Boolean): LazyList[A] =
    if queue.isEmpty then queue
    else if exit(queue.head) then queue
    else bfsPriority(f(queue.head) ++ queue.tail)(f, exit)

object VectorUtils:
  def dropWhileFun[A](as: Vector[A])(f: (A, A) => Boolean): Vector[A] =
    def go(ass: Vector[A], acc: Vector[A] = Vector.empty, n: Int = 0): Vector[A] =
      if n + 1 == as.length then as(n) +: acc
      else if f(as(n), as(n + 1)) then go(as, as(n) +: acc, n + 1)
      else as(n) +: acc

    go(as)

  def splitWhile[A](as: Vector[A])(f: (A, A) => Boolean): (Vector[A], Vector[A]) =
    def go(n: Int = 0): Int =
      if n + 1 >= as.length then as.length
      else if f(as(n), as(n + 1)) then go(n + 1)
      else n + 1

    as.splitAt(go(0))


  def rotateVector[A](n: Int, s: Vector[A]): Vector[A] =
    if s.isEmpty then s
    else
      val nbound = n % s.length // skipping the full rotation rounds
      if nbound < 0 then rotateVector(nbound + s.length, s)
      else s.drop(nbound) ++ s.take(nbound)