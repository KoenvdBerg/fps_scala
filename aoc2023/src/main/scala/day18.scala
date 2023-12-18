import scala.io.*
import math.*
import scala.annotation.tailrec
import aoc2022.Grid2D.Point

object aday18 extends App:

  private val day: String =
    this.getClass.getName.drop(4).init

  private val start1: Long =
    System.currentTimeMillis

  case class Plan(dir: Char, depth: Int, color: String)

  private val input: Vector[Plan] =

    def parse(s: String): Plan = s match
      case s"$dir $n $color" => Plan(dir.head, n.toInt, color)
      case _ => sys.error(s"cannot parse $s")

    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector
      .map(parse)


  object Lagoon:
    def getLagoon(plans: Vector[Plan]): Vector[Point] =

      plans.foldLeft(Vector(Point(0, 0))) { (res: Vector[Point], pl: Plan) =>
        val end = res.last
        val next = pl.dir match
          case 'U' => end.smear(Point(end.x, end.y - pl.depth))
          case 'D' => end.smear(Point(end.x, end.y + pl.depth))
          case 'L' => end.smear(Point(end.x - pl.depth, end.y))
          case 'R' => end.smear(Point(end.x + pl.depth, end.y))
        res ++ next
      }.distinct


    def wellSize(polygon: Vector[Point]) =
      val maxX = polygon.maxBy(_.x).x
      val maxY = polygon.maxBy(_.y).y
      for
        yy <- 0 to maxY
        slice = polygon.filter(_.y == yy).map(_.x).sorted
        xx <- 0 to maxX
      yield
        val p = xx
        // println(slice)
        slice.contains(p) || scanRight(slice, p)

  def scanRight(slice: Vector[Int], p: Int): Boolean =
    val trueEdges = slice.filter(_ > p).sliding(2).toVector
    println(trueEdges)
    val res = trueEdges.foldLeft((0, false)) {(c: (Int, Boolean), in: Vector[Int]) =>
      val diff = math.abs(in.head - in.last)
      if diff > 2 then
        if c._2 then (c._1 + 2, false) else (c._1 + 1, false)
      else
        (c._1, true)
    }
    val total = if res._2 then res._1 + 1 else res._1
    res._1 % 2 == 1

  // not right: 15636




  val test = Lagoon.getLagoon(input)
  println(Point.gridPrintable(test)(p => if test.contains(p) then 'X' else '.'))
  private val res1 = Lagoon.wellSize(test)


  private val answer1 = res1.count(identity)
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  private val answer2 = "l;eks"
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")





//    def wellSize(polygon: Vector[Point]) =
//      val maxY = polygon.maxBy(_.y).y
//      val maxX = polygon.maxBy(_.x).x
//      for
//        yy <- 0 to maxY
//      yield
//        val slice = polygon.filter(_.y == y).sortBy(_.x).map(_.x)
//        scanRight2(slice, yy, maxX)
//
//    def scanRight2(slice: Vector[Int], y: Int, mx: Int): Boolean =
//
//      def go(slice: Vector[Int], i: Int, count: Int, within: Boolean): Int =
//        if i > mx then count
//        else
//          if slice.contains(i) then go(slice, i+1, count+1, true)
//          else




//  def scanDown(polyEdges: Vector[Point], p: Point): Boolean =
//    if polyEdges.contains(p) then true
//    else
//      val xs = polyEdges.filter(_.x == p.x).filter(_.y > p.y)
//      if xs.isEmpty then true
//      else if xs.length == 1 then false
//      else
//        val trueEdges = xs.sliding(2).toVector.foldLeft(0) {(res: Int, in: Vector[Point]) =>
//          if math.abs(in.head.y - in(1).y) > 2 then res + 1
//          else res
//        }
//      // println(trueEdges.toVector.map(f => math.abs(f.head.y - f.last.y)))
//        println(trueEdges)
//        trueEdges % 2 == 0

//def getLagoonEdges(plans: Vector[Plan]): Vector[Point] =
//
//  plans.foldLeft(Vector(Point(0, 0))) { (res: Vector[Point], pl: Plan) =>
//    val end = res.last
//    val next = pl.dir match
//      case 'U' => Point(end.x, end.y - pl.depth)
//      case 'D' => Point(end.x, end.y + pl.depth)
//      case 'L' => Point(end.x - pl.depth, end.y)
//      case 'R' => Point(end.x + pl.depth, end.y)
//    res.appended(next)
//  }
//
//def wellSize(polygonEdges: Vector[Point]): Double =
//  // https://www.baeldung.com/cs/2d-polygon-area
//  println(polygonEdges.sliding(2).toVector)
//  val area = polygonEdges
//    .map(p => Point(p.x, p.y)).sliding(2).toVector
//    .map(f =>
//      val a1 = f.head
//      val a2 = f.last
//      a1.x * a2.y - a1.y * a2.x
//    )
//  println(area)
//  println(area.sum)
//  area.sum * 0.5