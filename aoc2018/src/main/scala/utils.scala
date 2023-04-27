package aoc2018

import scala.collection.mutable

object Grid2D:

  case class Point(x: Int, y: Int):
    def adjacent: Set[Point] =
      Set(
        Point(x - 1, y),
        Point(x + 1, y),
        Point(x, y - 1),
        Point(x, y + 1)
      )

    def toTuple: (Int, Int) = (this.x, this.y)

    def +(p2: Point): Point = Point(x + p2.x, y + p2.y)

    // breadth first search algorithm
    def -->(target: Point, obstacles: Vector[Point]): Option[Vector[Point]] =
      val seen: mutable.Set[Point] = obstacles.to(mutable.Set)
      var queue: mutable.Queue[Vector[Point]] = mutable.Queue[Vector[Point]](Vector(this))

      def loop(): Option[Vector[Point]] =
        if queue.isEmpty then None // exit condition when no path can be found
        else
          val thisPath = queue.dequeue()
          val thisPoint = thisPath.head
          seen += thisPoint
          if thisPoint == target then Some(thisPath) // exit condition when target is reached
          else
            val next: Set[Point] = thisPoint
              .adjacent // compute the adjacent Points
              .diff(seen) // filter out the points already visited
            queue = queue ++ next.map(n => n +: thisPath)
            loop()

      loop()

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