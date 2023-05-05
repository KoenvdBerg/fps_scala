import scala.io.*
import math.*
import aoc2018.Grid2D.Point
import scala.collection.mutable
import scala.collection.immutable.Queue

object day17 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val clay: Vector[(Point, Char)] =

    def parseClay(s: String): Vector[(Point, Char)] = s match
      case s"x=$x, y=${y1}..${y2}" => Range(y1.toInt, y2.toInt+1).map(y => (Point(x.toInt, y), '#')).toVector
      case s"y=$y, x=${x1}..${x2}" => Range(x1.toInt, x2.toInt+1).map(x => (Point(x, y.toInt), '#')).toVector
      case _ => sys.error("BOOM!!")

    val parsed: Vector[(Point, Char)] = Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector
      .flatMap(parseClay)

    val xMin: Int = parsed.minBy(_._1.x)._1.x - 1
    val yMin: Int = parsed.minBy(_._1.y)._1.y - 1
    val offset: Int = 500 - xMin
    val ground: Vector[(Point, Char)] = parsed.map(f => (Point(f._1.x - xMin, f._1.y), f._2)).distinct
    ground ++ Vector((Point(offset, yMin), '+'))  // add source at lowest clay y coordinate with offset


  enum Movement:
    case Down, Side, Still, Halt

  def waterfall(start: Point, clay: Vector[(Point, Char)]): Vector[(Point, Char)] =

    import Movement.*
    val ground: mutable.Set[Point] = clay.map(_._1).to(mutable.Set)
    val maxY: Int = ground.maxBy(_.y).y + 1

    def pourDown(c: Point): Point =
      val next: Point = c.adjacentDown.head
      next

    def pourSide(c: Point): ((Movement, Point), (Movement, Point)) =
      def go(q: Queue[Point], dir: String): (Movement, Point) =
        val (p, rem): (Point, Queue[Point]) = q.dequeue
        val next: Option[Point] = p.adjacentSides(dir).diff(ground).headOption
        next match
          case Some(pp) =>
            if ground.contains(Point(pp.x, pp.y + 1)) then go(rem.enqueue(pp), dir) else (Down, pp)
          case None     => (Still, p)

      val left: (Movement, Point) = go(Queue[Point](c), "left")
      val right: (Movement, Point) = go(Queue[Point](c), "right")
      (left, right)

    def updateWaterXRange(water: Vector[(Point, Char)], left: Point, right: Point, aqua: Char): Vector[(Point, Char)] =
      val toChange: Vector[Point] = Range(left.x, right.x + 1).map(f => Point(f, left.y)).toVector
      toChange.map((_, aqua)) ++ water.filterNot(a => toChange.contains(a._1))

    def updateGroundXRange(left: Point, right: Point): Unit =
      val toChange: Vector[Point] = Range(left.x, right.x + 1).map(f => Point(f, left.y)).toVector
      toChange.foreach(p => ground.add(p))

    def updateWaterAtPoint(water: Vector[(Point, Char)], loc: Point): Vector[(Point, Char)] =
      ((loc, '|') +: water).distinct

    def simulate(queue: Queue[Point], water: Vector[(Point, Char)] = Vector.empty): Vector[(Point, Char)] =
      if queue.isEmpty then water
      else
        val (p, rem): (Point, Queue[Point]) = queue.distinct.dequeue

        // determine move
        val nextMove: Movement = if p.y >= maxY then Halt else if p.adjacentDown.diff(ground).isEmpty then Side else Down

        if p.y % 50 == 0 then println(s"Reached depth away from maxDepth: ${p.y} / $maxY")


        // run water
        nextMove match
          case Down => simulate(rem.enqueue(pourDown(p)), updateWaterAtPoint(water, p))
          case Side =>
            val sides: ((Movement, Point), (Movement, Point)) = pourSide(p)
            (sides._1._1, sides._2._1) match
              case (Down, Down)    =>
                val nextWater: Vector[(Point, Char)] = updateWaterXRange(water, sides._1._2, sides._2._2, '|')
                simulate(rem.enqueueAll(List(sides._1._2, sides._2._2)), nextWater)
              case (Still, Down)   =>
                val nextWater: Vector[(Point, Char)] = updateWaterXRange(water, sides._1._2, sides._2._2, '|')
                simulate(rem.enqueue(sides._2._2), nextWater)
              case (Down, Still)   =>
                val nextWater: Vector[(Point, Char)] = updateWaterXRange(water, sides._1._2, sides._2._2, '|')
                simulate(rem.enqueue(sides._1._2), nextWater)
              case (Still, Still)  =>
                val nextWater: Vector[(Point, Char)] = updateWaterXRange(water, sides._1._2, sides._2._2, '~')
                updateGroundXRange(sides._1._2, sides._2._2)
                simulate(rem.enqueue(Point(p.x, p.y-1)), nextWater)
              case _ => sys.error("cannot simulate further from here")
          case Halt => simulate(rem, water)
          case _ => sys.error("NO MOVEMENT POSSIBLE")


    simulate(Queue[Point](Point(start.x, start.y + 1)))


  val start: Point = clay.filter(_._2 == '+').head._1
  val res1 = waterfall(start, clay)
  // Point.print2dGrid(res1 ++ clay)

  private val answer1: Int = res1.length
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  private val answer2: Int = res1.count(c => c._2 == '~')
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
