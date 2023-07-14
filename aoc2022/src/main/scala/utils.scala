package aoc2022

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.matching.Regex

object Grid2D:

  case class Point(x: Int, y: Int):
    def adjacent: Set[Point] =
      Set(
        Point(x, y - 1),
        Point(x - 1, y),
        Point(x + 1, y),
        Point(x, y + 1)
      )

    def adjacentDown: Set[Point] = Set(Point(x, y + 1))

    def adjacentSides(dir: String): Set[Point] = dir match
      case "left"  => Set(Point(x - 1, y))
      case "right" => Set(Point(x + 1, y))
      case _       => sys.error("cannot find adjacentSides")


    def toTuple: (Int, Int) = (this.x, this.y)

    def +(p2: Point): Point = Point(x + p2.x, y + p2.y)

    def -(p2: Point): Point = Point(x - p2.x, y - p2.y)

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

    def convertToFlatGrid[A](grid: Vector[Point], makeTo: A, default: A): (Vector[A], Int) =
      val minX: Point = grid.minBy(_.x)
      val minY: Point = grid.minBy(_.y)
      val normalized: Vector[Point] = grid.map((f: Point) => Point(f.x + -minX.x, f.y + -minY.y)).distinct.sortBy(_.toTuple.swap)
      val rowSize: Int = normalized.maxBy(_.x).x

      def go(in: Vector[Point], n: Int = 0, acc: Vector[A] = Vector.empty): Vector[A] = in match
        case h +: t =>
          val loc: Int = FlatGrid.pointToIndex(h.x, h.y, rowSize)
          if n == loc then go(t, n+1, makeTo +: acc)
          else go(in, n+1, default +: acc)
        case _      => acc

      (go(normalized), rowSize)

    def print2dGrid(obstacles: Vector[(Point, Char)], default: Char = '.'): Unit =
      val xMax: Int = obstacles.maxBy(_._1.x)._1.x

      def go(obs: Vector[(Point, Char)], x: Int = 0, y: Int = 0): Unit = obs match
        case ob +: t =>
          if x == xMax + 1 then {println(); go(obs, 0, y + 1)}
          else if x == ob._1.x && y == ob._1.y then {print(ob._2); go(t, x + 1, y)}
          else {print(default); go(obs, x + 1, y)}
        case Vector() => println()
        case _ => sys.error("print2dGrid ERROR")

      go(obstacles.sortBy(_._1.toTuple.swap).distinct)

object FlatGrid:

  /**
   * Takes the 4 neighbors non-diagonally from the target position i.e. left, right, above and below
   */
  def neighbours4(i: Int, rowSize: Int, nTiles: Int): Vector[Int] =
    val left: Int = if i % rowSize != 0 then i - 1 else -1
    val right: Int = if (i + 1) % rowSize != 0 then i + 1 else -1
    val vertical: Vector[Int] = Vector(i - rowSize, i + rowSize)
    (Vector(right, left) ++ vertical)
      .filter((pos: Int) => pos >= 0 && pos < nTiles && pos != i)


  def neighbours8(i: Int, rowSize: Int, nTiles: Int): Vector[Int] =
    def sides(pos: Int): Vector[Int] =
      val left: Int = if pos % rowSize != 0 then pos - 1 else -1
      val right: Int = if (pos + 1) % rowSize != 0 then pos + 1 else -1
      Vector(left, pos, right)

    def get(pos: Int): Vector[Int] =
      val vertical: Vector[Int] = Vector(pos - rowSize, pos, pos + rowSize)
      vertical
        .flatMap(sides)
        .filter(i => i >= 0 && i < nTiles && i != pos)

    get(i)

  def pointToIndex(x: Int, y: Int, rowSize: Int): Int =
    y * rowSize + x

  def printFlatGrid[A](grid: IndexedSeq[A], width: Int)(f: A => Char): String =
    def go(g: IndexedSeq[A], acc: String): String =
      if g.isEmpty then acc
      else
        val (head, next): (IndexedSeq[A], IndexedSeq[A]) = g.splitAt(width)
        val toPrint: String = head.map(f).mkString("") + "\n"
        go(next, acc + toPrint)

    go(grid, "")


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


  def lineSearch[A](as: Vector[A], start: A, initStep: Int)(f: (A, Vector[A]) => Int): A =

    @tailrec
    def go(current: A, step: Int): A =
      val curScore: Int = f(current, as)
      val next: Vector[A] = ???  // neighbours to current
      val scores: Vector[(A, Int)] = next.map((a: A) => (a, f(a, as)))
      val cont: Vector[(A, Int)] = scores.filter(_._2 > curScore)
      if cont.isEmpty && step == 1 then current
      else if cont.isEmpty then go(current, step / 2)
      else go(cont.maxBy(_._2)._1, step)

    go(start, initStep)


  object Dijkstra:

    import scala.collection.mutable.PriorityQueue

    // adapted from: https://ummels.de/2015/01/18/dijkstra-in-scala/

    type Graph[N] = N => Map[N, Int]

    def dijkstra[N](g: Graph[N])(source: N): (Map[N, Int], Map[N, N]) =

      // unfortunately there isn't an immutable priority queue, so we've to use the mutable one.
      val active: mutable.PriorityQueue[(N, Int)] = mutable.PriorityQueue((source, 0))(Ordering.by((f: (N, Int)) => f._2).reverse)

      def go(res: Map[N, Int], pred: Map[N, N]): (Map[N, Int], Map[N, N]) =
        if active.isEmpty then (res, pred)
        else
          val node: N = active.dequeue._1  // select the next node with lowest distance thus far
          val cost: Int = res(node)
          val neighbours: Map[N, Int] = for {
            (n, c) <- g(node) if cost + c < res.getOrElse(n, Int.MaxValue)
          } yield n -> (cost + c)          // update distances
          neighbours.foreach((n: (N, Int)) => active.enqueue(n))  // add next nodes to active nodes
          val preds: Map[N, N] = neighbours.map((f: (N, Int)) => (f._1, node))
          go(res ++ neighbours, pred ++ preds)

      go(Map(source -> 0), Map.empty[N, N])

    def shortestPath[N](g: Graph[N])(source: N, target: N): Option[List[N]] =
      val pred: Map[N, N] = dijkstra(g)(source)._2
      if pred.contains(target) || source == target then
        Some(iterateRight(target)(pred.get))
      else None

    def shortestDistance[N](g: Graph[N])(source: N, target: N): Option[Int] =
      val pred: Map[N, Int] = dijkstra(g)(source)._1
      if pred.contains(target) then pred.get(target)
      else if source == target then Some(0)
      else None

    def iterateRight[N](x: N)(f: N => Option[N]): List[N] =

      def go(xx: N, acc: List[N]): List[N] = f(xx) match
        case None    => xx :: acc
        case Some(v) => go(v, xx :: acc)

      go(x, List.empty[N])

    def tree(depth: Int): Graph[List[Boolean]] =
      (x: List[Boolean]) => x match
        case x if x.length < depth =>
          Map((true :: x) -> 1, (false :: x) -> 2)
        case x if x.length == depth => Map(Nil -> 1)
        case _ => Map.empty


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

  case class CircleVector[A](size: Int, v: Vector[A]):
    def moveN(i: Int, n: Int): CircleVector[A] =
      val dir: Int = (i + n) % size
      if dir < 0 then moveN(i, dir + size - 2)
      else if dir == 0 then moveN(i, size - i - 1)
      else
        val todo: Vector[(A, Double)] = v.zipWithIndex.map(x => (x._1, x._2.toDouble))
        val next: Vector[(A, Double)] = todo.filterNot(_._2 == i)
        CircleVector(size, ((v(i), dir + 0.1) +: next).sortBy(_._2).map(_._1))