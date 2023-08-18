package aoc2021

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

    def adjacentInclusive: Set[Point] =
      Set(
        Point(x, y - 1),
        Point(x - 1, y),
        Point(x + 1, y),
        Point(x, y + 1),
        Point(x, y)
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

    def manhattan(that: Point): Int =
      math.abs(this.x - that.x) + math.abs(this.y - that.y)


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

  def pointToIndex(p: Grid2D.Point, rowSize: Int): Int =
    p.y * rowSize + p.x

  import scala.math.Integral.Implicits.*
  def indexToPoint(i: Int, rowSize: Int): Grid2D.Point =
    val (y, x): (Int, Int) = i /% rowSize
    Grid2D.Point(x, y)

  def printFlatGrid[A](grid: IndexedSeq[A], width: Int)(f: A => Char): String =
    grid.map(f).mkString("").grouped(width).mkString("\n")


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


  object GraphTraversal:

    import scala.collection.mutable.PriorityQueue

    // Dijkstra adapted from: https://ummels.de/2015/01/18/dijkstra-in-scala/
    // A* adapted from: https://en.wikipedia.org/wiki/A*_search_algorithm

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

    def Astar[N](graph: Graph[N])(source: N, target: N, h: N => Int): (Map[N, Int], Map[N, N]) =

      val fScore: mutable.Map[N, Int] = mutable.Map(source -> h(source))
      val active: mutable.PriorityQueue[N] = mutable.PriorityQueue(source)(Ordering.by((n: N) => -fScore(n))) // todo: .reverse ?

      @tailrec
      def go(gScore: Map[N, Int], pred: Map[N, N]): (Map[N, Int], Map[N, N]) =
        val node: N = active.dequeue
        if node == target then (gScore, pred)  // select the next node with lowest fScore thus far
        else
          val cost: Int = gScore(node)
          val neighbours: Map[N, Int] = for {
            (n, d) <- graph(node) if cost + d < gScore.getOrElse(n, Int.MaxValue)
          } yield n -> (cost + d)  // update distances
          val preds: Map[N, N] = neighbours.map((f: (N, Int)) => (f._1, node))
          neighbours.foreach((n: N, s: Int) => fScore.addOne(n -> (s + h(n))))  // update for new fScores
          neighbours.foreach((n: N, _: Int) => active.enqueue(n))  // add next nodes to active nodes
          go(gScore ++ neighbours, pred ++ preds)

      go(Map(source -> 0), Map.empty[N, N])

    def shortestPath[N](g: Graph[N])(source: N, target: N): Option[List[N]] =
      // TODO: if multiple targets, then target: N => Boolean
      val pred: Map[N, N] = dijkstra(g)(source)._2
      if pred.contains(target) || source == target then
        Some(iterateRight(target)(pred.get))
      else None

    def shortestPath[N](g: Graph[N])(source: N, target: N, heuristic: N => Int): Option[List[N]] =
      val pred: Map[N, N] = Astar(g)(source, target, heuristic)._2
      if pred.contains(target) || source == target then
        Some(iterateRight(target)(pred.get))
      else None

    def shortestDistance[N](g: Graph[N])(source: N, target: N): Option[Int] =
      val pred: Map[N, Int] = dijkstra(g)(source)._1
      if pred.contains(target) then pred.get(target)
      else if source == target then Some(0)
      else None

    def shortestDistance[N](g: Graph[N])(source: N, target: N, heuristic: N => Int): Option[Int] =
      val pred: Map[N, Int] = Astar(g)(source, target, heuristic)._1
      if pred.contains(target) then pred.get(target)
      else if source == target then Some(0)
      else None

    def iterateRight[N](x: N)(f: N => Option[N]): List[N] =

      @tailrec
      def go(xx: N, acc: List[N]): List[N] = f(xx) match
        case None    => xx :: acc
        case Some(v) => go(v, xx :: acc)
      go(x, List.empty[N])

  end GraphTraversal


object MapUtils:
  def zero: Map[String, Long] = Map.empty[String, Long]

  def op(a1: Map[String, Long], a2: Map[String, Long]): Map[String, Long] =
    (a1.keySet ++ a2.keySet).foldLeft(zero) { (acc, k) =>
      acc.updated(k, a1.getOrElse(k, 0L) + a2.getOrElse(k, 0L))
    }

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

  def swap[A](in: Vector[A], a: Int, b: Int): Vector[A] =
    val todo: A = in(a)
    in.updated(a, in(b)).updated(b, todo)

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

  import scala.collection._

  case class Cycle[A](stemLength: Int, cycleLength: Int, cycleHead: A, cycleLast: A, cycleHeadRepeat: A)


  extension [A](it: Iterator[A]) def zipWithPrev: Iterator[(Option[A], A)] =
    new AbstractIterator[(Option[A], A)]:

      private var prevOption: Option[A] =
        None

      override def hasNext: Boolean =
        it.hasNext

      override def next: (Option[A], A) =
        val cur = it.next
        val ret = (prevOption, cur)
        prevOption = Some(cur)
        ret

  def find[A, B](coll: IterableOnce[A])(m: A => B): Option[Cycle[A]] =

    val trace: mutable.Map[B, (A, Int)] =
      mutable.Map[B, (A, Int)]()

    coll.iterator
      .zipWithPrev
      .zipWithIndex
      .map { case ((last, prev), idx) => (last, prev, trace.put(m(prev), (prev, idx)), idx) }
      .collectFirst { case (Some(last), repeat, Some((prev, prevIdx)), idx) =>
        Cycle(
          stemLength      = prevIdx,
          cycleLength     = idx - prevIdx,
          cycleHead       = prev,
          cycleLast       = last,
          cycleHeadRepeat = repeat
        )
      }

  def find[A, B](x0: A, f: A => A)(m: A => B): Cycle[A] =
    find(Iterator.iterate(x0)(f))(m).get

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


object NumberTheory:
  import math.Integral.Implicits.*
  @tailrec
  def toBinary(in: Int, acc: String = ""): String =
    val (div, rem) = in /% 2
    val bin: Char = "01"(rem)
    if div == 0 then (acc + bin).reverse else toBinary(div, acc + bin)

  def binaryToDec(binary: String): Int =
    val l: Int = binary.length
    binary.foldLeft((1, 0)) { (res: (Int, Int), in: Char) =>
      val toAdd: Int = math.pow(2, l - res._1).toInt * Map('1' -> 1, '0' -> 0)(in)
      (res._1 + 1, res._2 + toAdd)
    }._2
