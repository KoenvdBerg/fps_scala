import scala.io.*
import math.*
import scala.annotation.tailrec

object day24 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

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
    def newV(tx: Double, ty: Double, tz: Double): Hail = copy(dx = dx - tx, dy = dy - ty, dz = dz - tz)

  case class Point(x: Double, y: Double, z: Double)
  case class Line(delta: Double, b: Double):
    val fx: Double => Double = (x: Double) => delta * x + b
    val fy: Double => Double = (y: Double) => (y - b) / delta

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

  private val upperBound = 400000000000000L
  private val lowerBound = 200000000000000L

  private val intersections: Map[(Hail, Hail), Option[Point]] = makeIntersections(input)
  private val res1 = intersections
    .flatMap((h, pi) => pi.map(pii => (h, pii)))
    .filter((h, pi) => isFutureIntersection(h._1, pi) && isFutureIntersection(h._2, pi))
    .filter((_, pi) => pi.x >= lowerBound && pi.x <= upperBound && pi.y >= lowerBound && pi.y <= upperBound)
  private val answer1 = res1.size
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  def makePairs(lh: List[Hail], axis: Char): Map[Hail, Hail] =

    @tailrec
    def go(q: List[Hail], res: Map[Hail, Hail]): Map[Hail, Hail] =
      if q.isEmpty then res
      else
        val (h1, nq) = (q.head, q.tail)
        val pairs =
          if axis == 'x' then nq.filter(h2 => h2.dx == h1.dx)
          else if axis == 'y' then nq.filter(h2 => h2.dy == h1.dy)
          else nq.filter(h2 => h2.dz == h1.dz)
        go(nq, res ++ pairs.map(h2 => h1 -> h2).toMap)

    go(lh, Map.empty)

  def bruteForce(h1: Hail, h2: Hail, axis: Char): Set[Int] =

    val vrMin = -543
    val vrMax = 543

    @tailrec
    def go(vr: Int, res: Set[Int]): Set[Int] =
      if vr >= vrMax then res
      else
        val (dist, vhr) =
          if axis == 'x' then (h1.loc.x - h2.loc.x, h1.dx - vr)
          else if axis == 'y' then (h1.loc.y - h2.loc.y, h1.dy - vr)
          else (h1.loc.z - h2.loc.z, h1.dz - vr)
        if dist % vhr == 0 then go(vr + 1, res + vr)
        else go(vr + 1, res)

    go(vrMin, Set.empty)

  val vr = "xyz".foldLeft(Vector.empty) { (res: Vector[Int], in: Char) =>
    val parallel: Map[Hail, Hail] = makePairs(input, in)
    val search: Vector[Set[Int]] = parallel.toVector.map((h1, h2) => bruteForce(h1, h2, in))
    val vr: Set[Int] = search.reduce(_ intersect _)
    if vr.size > 1 then sys.error(s"not found for axis: $in")
    else res.appended(vr.head)
  }
  val h1: Hail = input(1).newV(vr(0), vr(1), vr(2))
  val h2: Hail = input(2).newV(vr(0), vr(1), vr(2))
  val xy: Point = h1.getLine.intersect(h2.getLine).get
  val time: Double = (xy.x - h1.loc.x) / h1.dx
  val z: Double = h1.loc.z + h1.dz * time
  val r0: Point = Point(xy.x, xy.y, z)
  println(s"Stone: $r0 @ ${vr.mkString(", ")}")
  private val answer2: Long = (r0.x + r0.y + r0.z).toLong
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
