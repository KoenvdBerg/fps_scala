import scala.io.*
import math.*
import aoc2018.Grid2D.Point
import scala.collection.mutable
import scala.collection.immutable.Queue

/**
 *
 * PART 01:
 *
 * The difficult part was to get the movement right. I originally started trying to reuse the breath first search
 * algorithm from day15, but the performance on the final input was an absolute disaster. So I changed plans to
 * work with just 1 coordinate, no path tracking. The revelation was to make a autonomous function that could take
 * care of pouring water to the sides. It scans into a direction (left or right) and then returns the movement
 * possible at that end: Still (for still water), Down (for waterfall). Then by pattern matching on the result of
 * the left and right end of a split, the continuation points in the simulation could be determined. Also, based on
 * that pattern match, water is added to the water collection; ~ when both ends are Still, | otherwise. In case of
 * Still on both ends, then make sure that the ground Set is updated as well (i.e. still water is now also an obstacle).
 *
 * Another minor issue was the arranging of the offset of x-coordinate. I didn't want to deal with coordinates around
 * 500, so I subtracted that from every x-coordinate.
 *
 * Another issue was that in my specific final input, the water left the leftmost part of the render at the very last
 * reservoir before reaching the bottom. To work with this, I changed the offset and shifted everything one x-coord
 * to the right.
 *
 * The final issue was that the source was 8 tiles higher than the topmost clay. I needed to make sure that the source
 * was at the level of the topmost clay to prevent counting these 8 water tiles. I solved this by computing an yMin
 * and set the source to that y-coordinate.
 *
 *
 * PART 02:
 * due to the way part 1 was implemented, the solution to part 2 was a plain and simple count of the character ~.
 *
 *
 */

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
    val maxY: Int = clay.map(_._1).maxBy(_.y).y + 1  // the y-coordinate that the water should not pass to prevent infinity

    def pourDown(c: Point): Point = Point(c.x, c.y + 1)

    def pourSide(c: Point, ground: Set[Point]): ((Movement, Point), (Movement, Point)) =

      def go(q: Queue[Point], dir: String): (Movement, Point) =
        val (p, rem): (Point, Queue[Point]) = q.dequeue
        val nextP: Point = dir match
          case "left"  => Point(p.x - 1, p.y)
          case "right" => Point(p.x + 1, p.y)
        if ground.contains(nextP) then (Still, p)  // the water at this side is blocked and thus movement becomes Still
        else
          if ground.contains(Point(nextP.x, nextP.y + 1)) then go(rem.enqueue(nextP), dir)  // the water cannot go down thus go further in dir
          else (Down, nextP)  // the water can go down here, so this sides new movement is Down

      val left: (Movement, Point) = go(Queue[Point](c), "left")
      val right: (Movement, Point) = go(Queue[Point](c), "right")
      (left, right)

    def updateWaterXRange(water: Vector[(Point, Char)], left: Point, right: Point)(aqua: Char): Vector[(Point, Char)] =
      val toChange: Vector[Point] = Range(left.x, right.x + 1).map(f => Point(f, left.y)).toVector
      toChange.map((_, aqua)) ++ water.filterNot(a => toChange.contains(a._1))

    def updateGroundXRange(ground: Set[Point], left: Point, right: Point): Set[Point] =
      val toAdd: Set[Point] = Range(left.x, right.x + 1).map(f => Point(f, left.y)).toSet
      ground.union(toAdd)

    def updateWaterAtPoint(water: Vector[(Point, Char)], loc: Point): Vector[(Point, Char)] =
      ((loc, '|') +: water).distinct

    def simulate(queue: Queue[Point], ground: Set[Point], water: Vector[(Point, Char)] = Vector.empty): Vector[(Point, Char)] =
      if queue.isEmpty then water
      else
        val (p, rem): (Point, Queue[Point]) = queue.distinct.dequeue

        // loading bar print, comment to disable
        if p.y % 500 == 0 then println(s"Reached depth: ${p.y} / $maxY")

        // determine next movement
        val nextMove: Movement = if p.y >= maxY then Halt else if ground.contains(pourDown(p)) then Side else Down

        // move water
        nextMove match
          case Down => simulate(rem.enqueue(pourDown(p)), ground, updateWaterAtPoint(water, p))
          case Side =>
            val sides: ((Movement, Point), (Movement, Point)) = pourSide(p, ground)
            val nextWater: Char => Vector[(Point, Char)] = updateWaterXRange(water, sides._1._2, sides._2._2)
            (sides._1._1, sides._2._1) match  // match the left and right movement of the water after splitting
              case (Down, Down)    =>  simulate(rem.enqueueAll(List(sides._1._2, sides._2._2)), ground, nextWater('|'))
              case (Still, Down)   =>  simulate(rem.enqueue(sides._2._2), ground, nextWater('|'))
              case (Down, Still)   =>  simulate(rem.enqueue(sides._1._2), ground, nextWater('|'))
              case (Still, Still)  =>
                // both ends blocked, add still water to ground, then go 1 up and continue from there
                val nextGround: Set[Point] = updateGroundXRange(ground, sides._1._2, sides._2._2)
                simulate(rem.enqueue(Point(p.x, p.y-1)), nextGround, nextWater('~'))
              case _ => sys.error("cannot simulate further from here")
          case Halt => simulate(rem, ground, water)
          case _ => sys.error("NO MOVEMENT POSSIBLE")


    simulate(Queue[Point](Point(start.x, start.y + 1)), clay.map(_._1).toSet)


  val start: Point = clay.filter(_._2 == '+').head._1
  val res1 = waterfall(start, clay)

  // print the filled clay ground, (un)comment to disable/enable
  // Point.print2dGrid(res1 ++ clay)

  private val answer1: Int = res1.length
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  private val answer2: Int = res1.count(c => c._2 == '~')
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
