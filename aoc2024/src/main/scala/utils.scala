package aoc2022

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.util.matching.Regex
import scala.math.Integral.Implicits.*


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
          LazyList(next*)

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

case class FlatGrid(gridLength: Int, width: Int, adj: Int => Int = identity[Int]):
  // todo: make everything a long at some point
  private val nRows: Int = gridLength / width

  private def isLeftBound(i: Int): Boolean = i % width == 0
  private def isRightBound(i: Int): Boolean = (i + 1) % width == 0

  private def checkValidGrid(grid: String): Unit =
    if grid.length != gridLength then sys.error(s"input grid is not of correct length [${grid.length} vs $gridLength]")
    else ()

  private def neighboursHorizontal(i: Int): Seq[Int] =
    val left: Int = if isLeftBound(i) then -1 else i - 1
    val right: Int = if isRightBound(i) then -1 else i + 1
    Vector(left, i, right)

  private def neighboursVertical(i: Int): Seq[Int] = Seq(i - width, i, i + width)

  def neighbours4(i: Int): Seq[Int] =
    (neighboursHorizontal(i) ++ neighboursVertical(i))
      .filter((pos: Int) => pos >= 0 && pos < gridLength && pos != i)

  def neighbours8(i: Int): Seq[Int] =
      neighboursVertical(i)
        .flatMap(neighboursHorizontal)
        .filter(pos => pos >= 0 && pos < gridLength && i != pos)

  private def pointToIndex(p: (Int, Int), w: Int = width): Int = p._2 * w + p._1

  private def indexToPoint(i: Int): (Int, Int) = i /% width // yields (y, x)

  def manhattan(i: Int, that: Int): Int =
    val (y1, x1) = indexToPoint(i)
    val (y2, x2) = indexToPoint(that)
    math.abs(y1 - y2) + math.abs(x1 - x2)

  def getGrid(grid: String): String =
    val newOrder: Map[Int, Char] = grid.zipWithIndex.map((c, i) => adj(i) -> c).toMap
    grid.indices.map(newOrder).mkString

  def getPrintableGrid(grid: String): String = "\n" + getGrid(grid).grouped(width).mkString("\n") + "\n"

  def rotateClockWise: FlatGrid =
    def adjust(i: Int): Int =
      val (y, x) = indexToPoint(i)
      width * (x + 1) - (y + 1)
    FlatGrid(gridLength, nRows, adj.andThen(adjust))

  def rotateCounterClockWise: FlatGrid =
    def adjust(i: Int): Int =
      val (y, x) = indexToPoint(i)
      width * (width - x) - (width - y)
    FlatGrid(gridLength, nRows, adj.andThen(adjust))

  def rows: Seq[Range] = Range(0, gridLength, width).map(getRowIndicesAt)

  def rowValues(grid: String): Seq[String] =
    checkValidGrid(grid)
    rows.map(_.map(grid).mkString)

  private def getRowIndicesAt(i: Int): Range =
    val offsetX = i % width
    val start = i - offsetX
    Range(start, start + width)

  def columns: Seq[Range] =
    Range(0, width).map(getColIndicesAt)

  def columnValues(grid: String): Seq[String] =
    checkValidGrid(grid)
    columns.map(_.map(grid).mkString)

  private def getColIndicesAt(i: Int): Range =
    val start = i % width
    Range(start, gridLength, width)

  def transpose: FlatGrid =
    def adjust(i: Int): Int =
      val (y, x) = indexToPoint(i)
      pointToIndex((y, x), nRows)

    FlatGrid(gridLength, nRows, adj.andThen(adjust))

  def diagonals: Seq[Range] =
    // right
    val firstRow = getRowIndicesAt(0)
    val firstCol = getColIndicesAt(0)
    val rightDiagonals = (firstRow ++ firstCol).distinct.map(i => getDiagonalIndicesAt(i, 1))

    val lastCol = getColIndicesAt(width - 1)
    val leftDiagonals = (firstRow ++ lastCol).distinct.map(i => getDiagonalIndicesAt(i, -1))
    // left
    rightDiagonals ++ leftDiagonals

  def diagonalValues(grid: String): Seq[String] =
    checkValidGrid(grid)
    diagonals.map(_.map(grid).mkString)

  private def getDiagonalIndicesAt(i: Int, direction: Int): Range =
    val stepSize = width + direction
    val (offsetY, offsetX) = indexToPoint(i)
    val ds = nRows - offsetY - 1 // down
    val us = offsetY // up
    val rs = width - offsetX - 1 // right
    val ls = offsetX // left
    // println(s"at pos=$i [${grid(i)}] can go up=$us, down=$ds, right=$rs, left=$ls")
    val end = if direction == 1 then i + ds.min(rs) * stepSize else i + ds.min(ls) * stepSize
    val start = if direction == 1 then i - us.min(ls) * stepSize else i - us.min(rs) * stepSize
    start to end by stepSize

  /**
   * 1 is to the right \
   * -1 is to the left /
   */
  private def checkBorder(i: Int, direction: Int): Boolean =
    if direction == 1 then isRightBound(i) else isLeftBound(i)

  private def neighboursSlash(i: Int, direction: Int): Seq[Int] =
    if checkBorder(i, direction) then Seq.empty[Int]
    else Seq(i - width + direction, i, i + width - direction).filter(i => i >= 0 && i < gridLength)

  def getCrossIndicesAt(i: Int): Seq[Seq[Int]] =
    val slashRight = neighboursSlash(i, 1)
    val slashLeft = neighboursSlash(i, -1)
    if slashRight.length != 3 || slashLeft.length != 3 then Seq.empty
    else Seq(slashRight, slashLeft)




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

  class LabelPropagationAlgorithm[N](graph: Map[N, List[N]], maxIterations: Int, rngCorrection: Int):

    import scala.util.Random

    // labels are updated each iteration and provide the result of this class. Local mutable state.
    private var labels: mutable.Map[N, N] = graph.keys.map(n => n -> n).to(mutable.Map)
    private val graphNodes: Vector[N] = graph.keys.toVector

    private def resetLabels: Unit =
      labels = graph.keys.map(n => n -> n).to(mutable.Map)

    private def receive(target: N): N =
      val neighbours = graph(target).map(labels).groupBy(identity).map((n, nn) => n -> nn.length).toMap
      neighbours.maxBy(_._2)._1

    private def randomize(nodes: Vector[N]): Vector[N] = Random.shuffle(nodes)

    @tailrec
    private def run(nodes: Vector[N], i: Int): Unit =
      if i >= maxIterations then ()
      else if labels.values.toSet.size == 2 then ()
      else
        nodes.foreach { (node: N) =>
          val received: N = receive(node)
          labels.update(node, received)
        }
        run(randomize(nodes), i + 1)

    def runAndCorrectRNG: Map[N, Int] =

      val ret: mutable.ListBuffer[Map[N, Int]] = mutable.ListBuffer[Map[N, Int]]()

      // happy side effects everywhere
      for (_ <- Range(0, rngCorrection)) {
        run(graphNodes, 0)
        val toAdd = labels.values.groupBy(identity).map((lbl, nn) => lbl -> nn.size).toMap
        ret.addOne(toAdd)
        resetLabels
      }

      ret.groupBy(identity).maxBy(_._2.length)._1

  end LabelPropagationAlgorithm

object SequenceUtils:

  extension [A](seq: Seq[A])
    def identityMap: Map[A, Int] = seq.foldLeft(Map.empty) { (res, c) => res.get(c) match
      case None => res.updated(c, 1)
      case Some(v) => res.updated(c, v + 1)
    }

  case class CircularQueue[A](start: Seq[A]):

    private val mq: scala.collection.mutable.Queue[A] = scala.collection.mutable.Queue.from(start)
    def dequeue: A =
      if mq.isEmpty then
        mq.enqueueAll(start)
        dequeue
      else mq.dequeue

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


object NumberTheory:
  import math.Integral.Implicits.*
  def toBinary(in: Int, acc: String = ""): String =
    val (div, rem) = in /% 2
    val bin: Char = "01"(rem)
    if div == 0 then (acc + bin).reverse else toBinary(div, acc + bin)


object RangeUtil:

  case class R(min: Long, max: Long):

    def overlap(that: R): Option[R] =
      if max < that.min then None
      else if min > that.max then None
      else Some(R(Vector(min, that.min).max, Vector(max, that.max).min))

    def nonOverlap(that: R): Vector[R] =
      if min >= that.min && max <= that.max then Vector.empty
      else if min < that.min && max > that.max then Vector(R(min, that.min - 1), R(that.max + 1, max))
      else if min < that.min then Vector(R(min, that.min - 1))
      else Vector(R(that.max + 1, max))

    def filter(hits: Vector[R]): Vector[R] =
      if hits.isEmpty then Vector(this)
      else if hits.length == 1 then nonOverlap(hits.head)
      else
        val before = if min < hits.head.min then Some(R(min, hits.head.min - 1)) else None
        val after = if max > hits.last.max then Some(R(hits.last.max + 1, max)) else None
        val between = hits.sliding(2).toVector.flatMap(v =>
          val diff = v.last.min - v.head.max
          if diff <= 1 then None
          else Some(R(v.head.max + 1, v.last.min - 1))
        )

        Vector(before, after).flatten ++ between

    def overlappingRanges(those: Vector[R]): Vector[(R, Boolean)] =
      val hits: Vector[R] = those.flatMap{ (that: R) => overlap(that) }
      val noHits: Vector[R] = filter(hits)
      hits.map(r => (r, true)) ++ noHits.map(r => (r, false))

object Optim:
  case class Memoize[I, O](f: I => O):

    val cache: mutable.Map[I, O] = new mutable.HashMap[I, O]
    def getMemoizedf: I => O = (in: I) => cache.getOrElseUpdate(in, f(in))
