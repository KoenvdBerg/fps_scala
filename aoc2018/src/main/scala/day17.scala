import scala.io.*
import math.*
import aoc2018.Grid2D.Point
import scala.collection.mutable

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

    val xMin: Int = parsed.minBy(_._1.x)._1.x
    val offset: Int = 500 - xMin
    val ground: Vector[(Point, Char)] = parsed.map(f => (Point(f._1.x - xMin, f._1.y), f._2)).distinct
    ground ++ Vector((Point(offset, 0), '+'))


  enum Movement:
    case Down, Side

  def waterfall(start: Point, clay: Vector[(Point, Char)]): Vector[(Point, Char)] =

    import Movement.*
    val seen: mutable.Set[Point] = clay.map(_._1).to(mutable.Set)
    val water: mutable.ArrayBuffer[(Point, Char)] = mutable.ArrayBuffer()
    val queue: mutable.Queue[Point] = mutable.Queue()

    def pourDown(c: Point): Option[Point] =
      c.adjacentDown.diff(seen).headOption match
        case None => None
        case Some(p) =>
          water.addOne((p, '|'))
          seen.add(p)
          Some(p)

    // TODO: let pourside be it's own recursive function that determines if it can pour further down at either end or if it hit a bassin
    // based on findings adds either | or ~
    def pourSide(c: Point): Option[Set[Point]] =
      val sides: Set[Point] = c.adjacentSides.diff(seen)
      if sides.isEmpty then None
      else
        sides.foreach(p => water.addOne((p, '|')))
        sides.foreach(p => seen.add(p))
        Some(sides)

    def stopSide: Unit = ??? // also changes | --> ~
    def stopDown: Unit = ??? // also changes | --> ~

    def simulate(move: Movement): Unit =
      if queue.isEmpty then ()
      else
        val p = queue.dequeue()
        // TODO: make pourDown go up 1 when pourside hits wals on both ends
        // determine move
        val nextMove: Movement = if seen.contains(p.adjacentDown.head) then Side else Down


        // run water
        nextMove match
          case Down => pourDown(p) match
            case None       => ()
            case Some(next) => queue.enqueue(next)
          case Side => pourSide(p) match
            case None       => ()
            case Some(next) => next.foreach(n => queue.enqueue(n))

        simulate(move)

    queue.enqueue(start)
    simulate(Movement.Down)
    water.toVector





  val start: Point = clay.filter(_._2 == '+').head._1

  val res1 = waterfall(start, clay)
  println(res1)
  Point.print2dGrid(clay ++ res1)



  private val answer1 = None
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis


  private val answer2 = None
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")

// YARD
// TODO: select a x from openEnds and find the minY for that
// TODO: for that minY, see if the x coordinate is the same as the rightmost x in agua. If yes, then nothing, else continue from that rightmost x coordinate
// TODO: make sure to remove the correct elements from the mutable Set, in case of duplicate x with jumps in y. Implement dropWhileFun
//
//def waterfallSearch(start: Point, obstacles: Vector[Point]): Vector[(Point, Char)] =
//  val maxY: Int = obstacles.maxBy(_.y).y
//  val seen: mutable.Set[Point] = obstacles.to(mutable.Set)
//  val openEnds: mutable.Set[Point] = mutable.Set[Point](start)
//  val agua: mutable.Set[(Point, Char)] = mutable.Set[(Point, Char)]()
//
//  def search: Point => LazyList[Point] =
//    (p: Point) =>
//      val downwards: Set[Point] = p.adjacentDown(seen.to(Set)).diff(seen)
//      if downwards.contains(Point(p.x, p.y + 1)) then openEnds.add(p) else ()
//      seen += p
//      downwards.foreach(seen += _)
//      agua += ((p, '~'))
//      LazyList(downwards.toSeq: _*)
//
//  def exitCondition: Point => Boolean =
//    (p: Point) =>
//      p.y >= maxY + 1
//
//  def findNextOpenEnd(ends: Set[Point]): Option[(Point, Set[Point])] =
//    if ends.isEmpty then None
//    else
//      val sortedEnds: Vector[Point] = ends.toVector.sortBy(_.toTuple)
//      // The waterfall is identified, and the remaining open ends are stored in rem
//      val (waterfall, rem): (Vector[Point], Vector[Point]) =
//        splitWhile(sortedEnds)((x, y) => y.y - x.y == 1 && x.x == y.x)
//      val manantial: Point = waterfall.minBy(_.y)
//      val rightmost = agua
//        .map(_._1)
//        .filter(_.y == manantial.y)
//        .maxBy(_.x) // this is the point that was not yet traveled, a true open end
//      if rightmost == manantial then
//        findNextOpenEnd(rem.toSet) // here the start of the waterfall was the open end, thus skip
//      else
//        Some((Point(rightmost.x + 1, rightmost.y), rem.toSet)) // return the new true open end and the remainder
//
//  def waterStream(source: Point): Unit =
//    bfsPriority(LazyList(source))(search, exitCondition)
//    findNextOpenEnd(openEnds.toSet) match
//      case None => () // no open ends remain, the waterstream is complete
//      case Some(next) => // continue with the next true open end
//        val (nextSource, nextEnds): (Point, Set[Point]) = next
//        openEnds.clear
//        nextEnds.foreach(p => openEnds.add(p))
//        waterStream(nextSource)
//
//  // run a waterstream that keeps track of the agua, then return all the agua tiles
//  waterStream(start)
//  agua.toVector