package aoc2018

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

    def toTuple: (Int, Int) = (this.x, this.y)

    def +(p2: Point): Point = Point(x + p2.x, y + p2.y)

    def <(t: Point): Boolean = this.x < t.x && this.y < t.y

    // breadth first search algorithm
    def -->(targets: Vector[Point], obstacles: Vector[Point]): Option[Vector[Point]] =
      val seen: mutable.Set[Point] = obstacles.to(mutable.Set)
      var queue: mutable.Queue[Vector[Point]] = mutable.Queue[Vector[Point]](Vector(this))

      def loop(): Option[Vector[Point]] =
        if queue.isEmpty then None // exit condition when no path can be found
        else
          val thisPath = queue.dequeue()
          val thisPoint = thisPath.head
          seen += thisPoint
          if targets.contains(thisPoint) then Some(thisPath) // exit condition when target is reached
          else
            val next: Set[Point] = thisPoint
              .adjacent // compute the adjacent Points
              .diff(seen) // filter out the points already visited
            next.map(seen += _)
            queue = queue ++ next.map(n => n +: thisPath)
            loop()

      loop()

    // from: https://stackoverflow.com/questions/41347337/how-to-implement-breadth-first-search-in-scala-with-fp
    def bfs[A](s: LazyList[A], f: A => LazyList[A]): LazyList[A] =
      if (s.isEmpty) s
      else s.head #:: bfs(s.tail ++ f(s.head), f)


    def pathsToTargets(targets: Vector[Point], obstacles: Vector[Point]): Vector[Vector[Point]] =
      val seen: mutable.Set[Point] = obstacles.to(mutable.Set)
      var found: Boolean = false
      val begin = Vector(this)

      def bfsAlgorithm: Vector[Point] => LazyList[Vector[Point]] =
        (p: Vector[Point]) =>
          val thisPoint: Point = p.head
          seen += thisPoint
          if targets.contains(thisPoint) then
            found = true
            LazyList()
          else
            if found then
              LazyList()
            else
              val viableDirs: Set[Point] = thisPoint.adjacent.diff(seen)
              viableDirs.map(seen += _)
              val next: Seq[Vector[Point]] = viableDirs.map(n => n +: p).toSeq
              LazyList(next: _*)

      val possiblePaths: LazyList[Vector[Point]] = bfs(LazyList(begin), bfsAlgorithm)
      val pathsToTargets: Vector[Vector[Point]] = targets.flatMap(t => possiblePaths.find(vp => vp.head == t))
      if pathsToTargets.isEmpty then
        pathsToTargets
      else
        val shortestPath: Int = pathsToTargets.map(_.length).min
        pathsToTargets.filter(_.length == shortestPath)



  object Point:

    def print2dGrid(obstacles: Vector[(Point, Char)]): Unit =
      val xMax: Int = obstacles.maxBy(_._1.x)._1.x

      def go(obs: Vector[(Point, Char)], n: Int = 0): Unit = obs match
        case ob +: t =>
          if n % (xMax + 1) == 0 then println() else ()
          if n % (xMax + 1) == ob._1.x then {print(ob._2); go(t, n + 1)}
          else {print('.'); go(obs, n + 1)}
        case Vector() => println()

      go(obstacles.sortBy(_._1.toTuple.swap))

    def smallest(ps: Seq[Point]): Point =
      def go(pps: Seq[Point], acc: Point = Point(Int.MaxValue, Int.MaxValue)): Point = pps match
        case h +: t =>
          if h < acc then go(t, h) else go(t, acc)
        case Seq() => acc
      go(ps)


// YARD:
//def nearTarget(start: Point, target: Point, obstacles: Vector[Point]): LazyList[Point] =
//  val seen: mutable.Set[Point] = obstacles.to(mutable.Set)
//
//  def bfsAlgorithm: Point => LazyList[Point] =
//    (p: Point) =>
//      seen += p
//      val next = p.adjacent.diff(seen)
//      LazyList(next.toSeq: _*)
//
//  val begin = Vector(start)
//
//  bfs(LazyList(begin), bfsAlgorithm)
//def nearTarget(start: Point, target: Point, obstacles: Vector[Point]): LazyList[Vector[Point]] =
//  val seen: mutable.Set[Point] = obstacles.to(mutable.Set)
//  var found: Boolean = false
//  val begin = Vector(start)
//
//  def bfsAlgorithm: Vector[Point] => LazyList[Vector[Point]] =
//    (p: Vector[Point]) =>
//      val thisPoint: Point = p.head
//      if thisPoint == target then
//        found = true
//        LazyList()
//      else if found then
//        LazyList()
//      else
//        seen += thisPoint
//        val viableDirs: Set[Point] = thisPoint.adjacent.diff(seen)
//        val next: Seq[Vector[Point]] = viableDirs.map(n => n +: p).toSeq
//        LazyList(next: _*)
//
//  bfs(LazyList(begin), bfsAlgorithm)