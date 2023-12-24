import scala.io.*
import math.*
import scala.annotation.tailrec

object aday24 extends App:

  private val day: String =
    this.getClass.getName.drop(4).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: List[Hail] =

    def parse(s: String): Hail = s match
      case s"$x, $y, $z @ $vx, $vy, $vz" =>
        val p1 = Point(x.toDouble, y.toDouble, z.toDouble)
        Hail(p1, vx.toDouble, vy.toDouble, vz.toDouble)
      case _ => sys.error(s"Cannot parse $s")

    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList
      .map(parse)

  case class Hail(loc: Point, dx: Double, dy: Double, dz: Double):

    def getLine: Line = Line.makeLine(loc, loc.copy(x = loc.x + dx, y = loc.y + dy, z = loc.z + dz))

    def atTime(t: Int): Point = Point(loc.x + dx * t, loc.y + dy * t, loc.z + dz * t)

  case class Point(x: Double, y: Double, z: Double)
  case class Line(delta: Double, b: Double):
    val fx: Double => Double = (x: Double) => delta * x + b
    val fy: Double => Double = (y: Double) => (y - b) / delta

    def fyBounded(min: Double, max: Double): Double => Option[Double] =
      (y: Double) =>
        val x: Double = fy(y)
        Option.when(x >= min && x <= max)(x)

    def intersect (that: Line): Option[Point] =
      if delta == that.delta then None // parallel (identical) lines no intersection possible
      else
        val x = (that.b - b) / (delta - that.delta)
        Some(Point(x, fx(x), -1))

  object Line:
    def makeLine(p1: Point, p2: Point): Line =
      val delta: Double = (p2.y - p1.y) / (p2.x - p1.x)
      val b: Double = p1.y - (delta * p1.x)
      Line(delta, b)


  def makeIntersections(lh: List[Hail]): Map[(Hail, Hail), Option[Point]] =

    @tailrec
    def go(q: List[Hail], res: Map[(Hail, Hail), Option[Point]]): Map[(Hail, Hail), Option[Point]] =
      if q.isEmpty then res
      else
        val (h, nq) = (q.head, q.tail)
        val hline = h.getLine
        val intersections = nq.map{ (th: Hail) =>
          val intersection = th.getLine.intersect(hline)
          (th, h) -> intersection
        }.toMap
        go(nq, res ++ intersections)

    go(lh, Map.empty)

  def isFutureIntersection(h: Hail, pi: Point): Boolean =
    val loc = h.loc
    (h.dx.sign.toInt, h.dy.sign.toInt) match
      case(1, 1) => pi.x >= loc.x && pi.y >= loc.y
      case(-1, 1) => pi.x <= loc.x && pi.y >= loc.y
      case(1, -1) => pi.x >= loc.x && pi.y <= loc.y
      case(-1, -1) => pi.x <= loc.x && pi.y <= loc.y


  private val upperBound = 27 // 400000000000000L
  private val lowerBound = 7 // 200000000000000L

  private val intersections: Map[(Hail, Hail), Option[Point]] = makeIntersections(input)
  private val res1 = intersections
    .flatMap((h, pi) => pi.map(pii => (h, pii)))
    .filter((h, pi) => isFutureIntersection(h._1, pi) && isFutureIntersection(h._2, pi))
    .filter((_, pi) => pi.x >= lowerBound && pi.x <= upperBound && pi.y >= lowerBound && pi.y <= upperBound)
  private val answer1 = res1.size
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis


  def doesIntersectWith(check: Line, ll: List[Line]): Boolean =

    val l = ll.length

    def go(i: Int, res: Boolean): Boolean =
      if i >= l then res
      else if !res then res
      else
        val cur = ll(i)
        val intersection = check.intersect(cur)
        println(intersection)
        go(i+1, intersection.isDefined)

    go(0, true)



  val lines = input.map(_.getLine)
  val h1 = input.head
  val h2 = input(1)

  val rock = Line.makeLine(h1.atTime(5), h2.atTime(3))
  println(rock)

  val test = doesIntersectWith(rock, lines)
  println(test)

  val ht1 = h1.atTime(5)
  val ht2 = h2.atTime(3)
  val yline = Line.makeLine(ht1, ht2)
  val zline = Line.makeLine(Point(ht1.y, ht1.z, -1), Point(ht2.y, ht2.z, -1))
  val dx = math.abs(1 / yline.delta)

  println(s"startPoint hail: ${h2.atTime(3)}")
  println(s"yline: $yline")
  println(s"zline: $zline")



  val startRock = Point(ht2.x + 3 * dx, ht2.y + 3 * rock.delta * dx, ht2.z - 3 * zline.delta)
  println(startRock)




  // https://www.codeproject.com/Articles/990452/Interception-of-Two-Moving-Objects-in-D-Space
  private val answer2 = "kdlsjf"
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
