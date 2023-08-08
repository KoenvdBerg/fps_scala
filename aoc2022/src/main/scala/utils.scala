package aoc2022

import scala.annotation.tailrec
import scala.collection.immutable.Queue
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


    def smear(that: Point): Vector[Point] =
      val sm: Vector[Point] = for {
        xs <- Range(x.min(that.x), that.x.max(x) + 1).toVector
        ys <- Range(y.min(that.y), that.y.max(y) + 1).toVector
      } yield Point(xs, ys)
      if sm.head == that then sm.reverse else sm

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
    def convertToFlatGrid[A](grid: Vector[Point], makeTo: Point => A): (Int, IndexedSeq[A]) =
      val width: Int = grid.maxBy(_.x).x - grid.minBy(_.x).x + 1
      val allPoints: Vector[Point] = Point(grid.minBy(_.x).x, grid.minBy(_.y).y)
        .smear(Point(grid.maxBy(_.x).x, grid.maxBy(_.y).y))
        .sortBy(_.toTuple.swap)
      val flat: IndexedSeq[A] = allPoints.map(makeTo)
      (width, flat)

    def gridPrintable(grid: Vector[Point])(f: Point => Char): String =
      val (width, flat) = convertToFlatGrid(grid, f)
      flat.mkString("").grouped(width).map(_.reverse).mkString("\n").reverse

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
    grid.map(f).mkString("").grouped(width).mkString("\n")


  
  case class Line(delta: Int, b: Int):
    val fx: Int => Int = (x: Int) => delta * x + b
    val fy: Int => Int = (y: Int) => (y - b) / delta
    def fyBounded(min: Int, max: Int): Int => Option[Int] = 
      (y: Int) => 
        val x: Int = fy(y)
        Option.when(x >= min && x <= max)(x)

    def intersect(that: Line): Option[Point] =
      if delta == that.delta then None // parallel (identical) lines no intersection possible
      else
        val x = (that.b - b) / (delta - that.delta)
        Some(Point(x, fx(x)))

  object Line:
    def makeLine(p1: Point, p2: Point): Line =
      val delta: Int = (p2.y - p1.y) / (p2.x - p1.x)
      val b: Int = p1.y - (delta * p1.x)
      Line(delta, b)

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


  def floodAlgorithm[N](g: N => Set[N])(source: N): Set[N] =
    import scala.collection.immutable.Queue

    @tailrec
    def go(res: Set[N], active: Queue[N]): Set[N] =
      if active.isEmpty then res
      else
        val (node, rem): (N, Queue[N]) = active.dequeue
        val neighbours: Set[N] = g(node).filter((n: N) => !res(n))  // node should not have been seen before
        val nextQueue: Queue[N] = rem.enqueueAll(neighbours)        // add all next nodes to queue
        go(res ++ neighbours, nextQueue)

    go(Set.empty[N], Queue(source))


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
  extension (c: Vector[Int])
    def -(that: Vector[Int]): Vector[Int] = c.zipWithIndex.map((v: Int, i: Int) => v - that(i))
    def +(that: Vector[Int]): Vector[Int] = c.zipWithIndex.map((v: Int, i: Int) => v + that(i))
    def *(i: Int): Vector[Int] = c.map((v: Int) => v * i)
    def >=(that: Vector[Int]): Boolean = c.zipWithIndex.forall((v: Int, i: Int) => v >= that(i))
    def <=(that: Vector[Int]): Boolean = c.zipWithIndex.forall((v: Int, i: Int) => v <= that(i))
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




object CycleFinder:
  case class Cycle[A](stemLength: Int, cycleLength: Int, first: A, last: A)

  object Cycle:
    def find[A, B](f: A => A, x0: A)(g: A => B): Cycle[A] =
      @annotation.tailrec
      def findCycleLength(tortoise: A, hare: A, cycleLength: Int, power: Int): Int =
        if g(tortoise) == g(hare) then
          cycleLength
        else if power == cycleLength then
          findCycleLength(hare, f(hare), 1, power * 2)
        else
          findCycleLength(tortoise, f(hare), cycleLength + 1, power)

      @annotation.tailrec
      def findStemLength(tortoise: A, hare: A, prevHare: A, stemLength: Int): (Int, A, A) =
        if g(tortoise) == g(hare) then
          (stemLength, tortoise, prevHare)
        else
          findStemLength(f(tortoise), f(hare), hare, stemLength + 1)

      val cycleLength =
        findCycleLength(x0, f(x0), 1, 1)

      val hare =
        (0 until cycleLength).foldLeft(x0)((acc, _) => f(acc))

      val (stemLength, first, last) =
        findStemLength(x0, hare, hare, 0)

      Cycle(stemLength, cycleLength, first, last)
end CycleFinder

object GameTree: 
  enum DecisionTree[+A]:
    case Result(value: A)
    case Decision(ds: List[DecisionTree[A]])

    def map[B](f: A => B): DecisionTree[B] = this match
      case Result(v) => Result(f(v))
      case Decision(ds) => Decision(ds.map((t: DecisionTree[A]) => t.map(f)))

    def flatMap[B](f: A => DecisionTree[B]): DecisionTree[B] = this match
      case Result(v) => f(v)
      case Decision(ds) => Decision(ds.map(_.flatMap(f)))

    def apply[B](gf: DecisionTree[A => B]): DecisionTree[B] = gf match
      case Result(v) => this.map(v)
      case Decision(ds) => this match
        case Result(v) => Decision(ds.map((f: DecisionTree[A => B]) => this.apply(f)))
        case Decision(vs) => Decision(ds.zip(vs).map((gs: DecisionTree[A => B], vss: DecisionTree[A]) => vss.apply(gs)))
        
    def map2[B, C](fb: DecisionTree[B])(f: (A, B) => C): DecisionTree[C] = fb.apply(this.map(f.curried))


  object DecisionTree:
  
    def treeToList[A](dt: DecisionTree[A]): List[A] = dt match
      case Result(v)    => List(v)
      case Decision(vs) => vs.flatMap((d: DecisionTree[A]) => treeToList(d))
      
    def pure[A](a: A): DecisionTree[A] = Result(a)
  
    def sequence[A](dtl: List[DecisionTree[A]]): DecisionTree[List[A]] = dtl match
      case h :: t => h.flatMap((a: A) => sequence(t).map((b: List[A]) => a :: b))
      case Nil => Result(Nil)


