package aoc2024

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.io.Source
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
          LazyList(next *)

      def earlyExit: Vector[Point] => Boolean = (p: Vector[Point]) => targets.contains(p.head)

      bfs(LazyList(Vector(this)))(search, earlyExit)

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


case class BoundedGrid(xLim: Int, yLim: Int, grid: Seq[Seq[Char]]):

  import BoundedGrid.*

  def withinBounds(p: (Int, Int)): Boolean =
    p._1 >= 0 && p._1 < xLim && p._2 >= 0 && p._2 < yLim

  def patchOne(p: (Int, Int), r: Char): BoundedGrid =
    val row: Seq[Char] = getRowAt(p._2).map(p => grid(p._2)(p._1))
    val newGrid = grid.updated(p._2, row.updated(p._1, r))
    copy(grid = newGrid)

  def getPointsOf(c: Char): Seq[(Int, Int)] =
    for
      x <- Range(0, xLim)
      y <- Range(0, yLim)
      if grid(y)(x) == c
    yield x -> y

  def smear(p1: (Int, Int), p2: (Int, Int)): Seq[(Int, Int)] =
    for
      xs <- Range(p1._1.min(p2._1), p1._1.max(p2._1))
      ys <- Range(p1._2.min(p2._2), p1._2.max(p2._2))
      if withinBounds(xs -> ys)
    yield xs -> ys

  private def getRowAt(y: Int): Seq[(Int, Int)] = Range(0, xLim).map(x => x -> y)

  def rows: Seq[Seq[(Int, Int)]] = Range(0, yLim).map(getRowAt)

  def rowValues: Seq[String] = rows.map(_.map(p => grid(p._2)(p._1)).mkString)

  private def getColAt(x: Int): Seq[(Int, Int)] = Range(0, yLim).map(y => x -> y)

  def columns: Seq[Seq[(Int, Int)]] = Range(0, xLim).map(getColAt)

  def columnValues: Seq[String] = columns.map(_.map(p => grid(p._2)(p._1)).mkString)

  def diagonals: Seq[Seq[(Int, Int)]] =
    val firstRow = getRowAt(0)
    val firstCol = getColAt(0)
    val rightDiagonals = (firstRow ++ firstCol).distinct.map(p => lineForward(p, (1, 1)))

    val lastCol = getColAt(xLim - 1)
    val leftDiagonals = (firstRow ++ lastCol).distinct.map(p => lineForward(p, (-1, 1)))
    rightDiagonals ++ leftDiagonals

  def diagonalValues: Seq[String] = diagonals.map(_.map(p => grid(p._2)(p._1)).mkString)

  def transpose: BoundedGrid = BoundedGrid(yLim, xLim, grid.transpose)

  def rotateClockWise: BoundedGrid = BoundedGrid(yLim, xLim, columns.map(_.map(p => grid(p._2)(p._1)).reverse))

  def rotateCounterClockWise: BoundedGrid = BoundedGrid(yLim, xLim, columns.map(_.map(p => grid(p._2)(p._1))).reverse)

  def printGrid: Unit = grid.foreach(s => println(s.mkString))

  def createGrid(points: Seq[(Int, Int)]): BoundedGrid =
    val s = for
      x <- Range(0, xLim)
      y <- Range(0, yLim)
    yield if points.contains(x -> y) then '#' else '.'
    copy(grid = s.grouped(xLim).toSeq)

  def coefficient(p1: (Int, Int), p2: (Int, Int)): (Int, Int) =
    val xd = p2._1 - p1._1
    val yd = p2._2 - p1._2
    (xd, yd)

  def step(p: (Int, Int), coefficient: (Int, Int)): Option[(Int, Int)] =
    val n = p + coefficient
    Some(n).filter(withinBounds)

  def stepNeighbour(p: (Int, Int), coefficient: (Int, Int)): Seq[(Int, Int)] =
    Seq(p + coefficient, p - coefficient).filter(withinBounds)

  def crossNeighbour(p: (Int, Int)): Seq[Seq[(Int, Int)]] =
    Seq(stepNeighbour(p, (1, 1)).appended(p).sorted, stepNeighbour(p, (-1, 1)).appended(p).sorted)

  @tailrec
  private def lineForward(p: (Int, Int), coefficient: (Int, Int), acc: Seq[(Int, Int)] = Seq.empty): Seq[(Int, Int)] =
    if !withinBounds(p) then acc
    else lineForward(p + coefficient, coefficient, acc.appended(p))

  def line(p: (Int, Int), coefficient: (Int, Int)): Seq[(Int, Int)] =
    val f = lineForward(p, coefficient)
    val b = lineForward(p - coefficient, coefficient.negate)
    (f ++ b).sorted

  def pointToIndex(p: (Int, Int)): Int = p._2 * xLim + p._1

  def indexToPoint(i: Int): (Int, Int) = i /% xLim

  def neighbours4(p: (Int, Int)): Seq[(Int, Int)] =
    Seq(
      (p._1, p._2 - 1),
      (p._1 - 1, p._2),
      (p._1 + 1, p._2),
      (p._1, p._2 + 1)
    ).filter(withinBounds)

  def neighbours4Unbounded(p: (Int, Int)): Seq[(Int, Int)] =
    Seq(
      (p._1, p._2 - 1),
      (p._1 - 1, p._2),
      (p._1 + 1, p._2),
      (p._1, p._2 + 1)
    )

  def neighbours8(p: (Int, Int)): Seq[(Int, Int)] =
    Seq(
      (p._1, p._2 - 1),
      (p._1 - 1, p._2),
      (p._1 + 1, p._2),
      (p._1, p._2 + 1),
      (p._1 + 1, p._2 + 1),
      (p._1 - 1, p._2 + 1),
      (p._1 - 1, p._2 - 1),
      (p._1 + 1, p._2 - 1),
    ).filter(withinBounds)

  def get(p: (Int, Int)): Char = grid(p._2)(p._1)

  def neighbourAbove(p: (Int, Int)): (Int, Int) = (p._1, p._2 - 1)
  def neighbourBelow(p: (Int, Int)): (Int, Int) = (p._1, p._2 + 1)
  def neighbourRight(p: (Int, Int)): (Int, Int) = (p._1 + 1, p._2)
  def neighbourLeft(p: (Int, Int)): (Int, Int) = (p._1 - 1, p._2)

  def wrapMove(p: (Int, Int), v: (Int, Int), time: Int): (Int, Int) =
    val (x, y) = p + v * time
    val modX = x % xLim
    val modY = y % yLim
    val nx = if modX < 0 then modX + xLim else modX
    val ny = if modY < 0 then modY + yLim else modY
    nx -> ny

  /**
   * grid
   * ...
   * ...
   * ...
   * becomes
   * . .
   *
   * . .
   */
  def classifyQuadrant(p: (Int, Int)): Int =
    val middleX = xLim / 2
    val middleY = yLim / 2
    val (x, y) = p
    if x < middleX && y < middleY then 1
    else if x > middleX  && y < middleY then 2
    else if x < middleX && y > middleY then 3
    else if x > middleX && y > middleY then 4
    else -1

object BoundedGrid:

  def fromResource(resource: String): BoundedGrid =
    val s = Source
      .fromResource(resource)
    val v = s.getLines.toSeq
    s.close()
    BoundedGrid.fromString(v.mkString, v.head.length)

  def fromString(input: String, width: Int): BoundedGrid =
    BoundedGrid(width, input.length / width, input.grouped(width).map(_.toSeq).toSeq)

  extension (p: (Int, Int))
    def negate: (Int, Int) = (-p._1, -p._2)
    def rotate: (Int, Int) = (-p._2, p._1)
    def +(that: (Int, Int)): (Int, Int) = (p._1 + that._1, p._2 + that._2)
    def *(that: Int): (Int, Int) = (p._1 * that, p._2 * that)
    def -(that: (Int, Int)): (Int, Int) = (p._1 - that._1, p._2 - that._2)
    def manhattan(that: (Int, Int)): Int = math.abs(p._1 - that._1) + math.abs(p._2 - that._2)

case class FlatGrid(gridLength: Int, width: Int, adj: Int => Int = identity[Int]):
  // todo: make everything a long at some point
  private val nRows: Int = gridLength / width

  def isLeftBound(i: Int): Boolean = i % width == 0

  def isRightBound(i: Int): Boolean = (i + 1) % width == 0

  private def checkValidGrid(grid: String): Unit =
    if grid.length != gridLength then sys.error(s"input grid is not of correct length [${grid.length} vs $gridLength]")
    else ()

  private def checkOnGrid(i: Int): Unit =
    if i < 0 || i >= gridLength then sys.error(s"index [$i] is out of bounds of grid")
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

  def pointToIndex(p: (Int, Int), w: Int = width): Int = p._2 * w + p._1

  def indexToPoint(i: Int): (Int, Int) = i /% width // yields (y, x)

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

  def getLineIndices(i: Int, that: Int): Range =
    checkOnGrid(i)
    checkOnGrid(that)
    val stepSize = math.abs(i - that)
    val (y, x) = indexToPoint(i)
    val (thaty, thatx) = indexToPoint(that)
    val offsetX = math.abs(thatx - x)
    val offsetY = math.abs(thaty - y)
    //    println(s"points: [$x, $y] vs. [$thatx, $thaty] for [$i, $that] and width=$width")
    println(s"$stepSize and offsets $x [x=$offsetX, y=$offsetY]")

    // before
    val timesXStep = x / offsetX
    val timesYStep = y / offsetY

    println(s"before: $timesXStep $timesYStep")

    // after
    val afterXStep = (width - x) / offsetX
    val afterYStep = (nRows - y) / offsetY

    val start = i - timesXStep.min(timesYStep) * stepSize
    val end = i + afterXStep.min(afterYStep) * stepSize

    start to end by stepSize

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
      val next: Vector[A] = ??? // neighbours to current
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
        val neighbours: Set[N] = g(node).filter((n: N) => !res(n)) // node should not have been seen before
        val nextQueue: Queue[N] = rem.enqueueAll(neighbours) // add all next nodes to queue
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
          val node: N = active.dequeue._1 // select the next node with lowest distance thus far
          val cost: Int = res(node)
          val neighbours: Map[N, Int] = for {
            (n, c) <- g(node) if cost + c < res.getOrElse(n, Int.MaxValue)
          } yield n -> (cost + c) // update distances
          neighbours.foreach((n: (N, Int)) => active.enqueue(n)) // add next nodes to active nodes
          val preds: Map[N, N] = neighbours.map((f: (N, Int)) => (f._1, node))
          go(res ++ neighbours, pred ++ preds)

      go(Map(source -> 0), Map.empty[N, N])

    def Astar[N](graph: Graph[N])(source: N, target: N, h: N => Int): (Map[N, Int], Map[N, N]) =

      val fScore: mutable.Map[N, Int] = mutable.Map(source -> h(source))
      val active: mutable.PriorityQueue[N] = mutable.PriorityQueue(source)(Ordering.by((n: N) => -fScore(n))) // todo: .reverse ?

      @tailrec
      def go(gScore: Map[N, Int], pred: Map[N, N]): (Map[N, Int], Map[N, N]) =
        val node: N = active.dequeue
        if node == target then (gScore, pred) // select the next node with lowest fScore thus far
        else
          val cost: Int = gScore(node)
          val neighbours: Map[N, Int] = for {
            (n, d) <- graph(node) if cost + d < gScore.getOrElse(n, Int.MaxValue)
          } yield n -> (cost + d) // update distances
          val preds: Map[N, N] = neighbours.map((f: (N, Int)) => (f._1, node))
          neighbours.foreach((n: N, s: Int) => fScore.addOne(n -> (s + h(n)))) // update for new fScores
          neighbours.foreach((n: N, _: Int) => active.enqueue(n)) // add next nodes to active nodes
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
        case None => xx :: acc
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
    def identityMap: Map[A, Long] = seq.foldLeft(Map.empty) { (res, c) =>
      res.get(c) match
        case None => res.updated(c, 1L)
        case Some(v) => res.updated(c, v + 1L)
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
      case Result(v) => List(v)
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
      val hits: Vector[R] = those.flatMap { (that: R) => overlap(that) }
      val noHits: Vector[R] = filter(hits)
      hits.map(r => (r, true)) ++ noHits.map(r => (r, false))

object Optim:
  case class Memoize[I, O](f: I => O):

    val cache: mutable.Map[I, O] = new mutable.HashMap[I, O]

    def getMemoizedf: I => O = (in: I) => cache.getOrElseUpdate(in, f(in))
