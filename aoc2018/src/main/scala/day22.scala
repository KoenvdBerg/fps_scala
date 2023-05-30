import scala.io.*
import aoc2018.Grid2D.Point
import scala.collection.mutable

/**
 *
 * PART 1:
 *
 * PART 2:
 *
 *
 */

object day22 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val (depth, goal): (Int, Point) =
    val in: String = Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList
      .mkString(" ")

    in match
      case s"depth: $d target: $x,$y" => (d.toInt, Point(x.toInt, y.toInt))
      case _                          => sys.error("BOOM")

  def getErosion(geoIndex: Int): Int = (geoIndex + depth) % 20183

  def erosionMap(maxX: Int, maxY: Int): Vector[Vector[Int]] =

    def getSliceAtY(above: Vector[Int], acc: Vector[Int], x: Int = 1): Vector[Int] =
      if x > maxX then
        acc.reverse
      else
        val e: Int = getErosion(acc.head * above(x))
        getSliceAtY(above, e +: acc, x + 1)

    def go(y: Int, acc: Vector[Vector[Int]] = Vector.empty[Vector[Int]]): Vector[Vector[Int]] = y match
      case yf if yf > maxY => acc.reverse
      case 0    =>
        val xSlice: Vector[Int] = Range(0, maxX + 1).map((i: Int) => getErosion(i * 16807)).toVector
        go(y + 1, xSlice +: acc)
      case _    =>
        val xSlice: Vector[Int] = getSliceAtY(acc.head, Vector(getErosion(y * 48271)))
        go(y + 1, xSlice +: acc)

      go(0)



  // target value not taken into account, but part02 reveals it's always 0 (rocky)
  val erosionLevels: Vector[Vector[Int]] = erosionMap(goal.x, goal.y)
  val res1: Vector[Int] = erosionLevels.flatMap((layer: Vector[Int]) => layer.map((er: Int) => er % 3))
  private val answer1 = res1.sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  private val answer2 = None
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")


//  def erosion(p: Point, t: Point, depth: Int): Long =
//
//    def loop(pp: Point): Long = pp match
//      case Point(t.x, t.y) => 0L
//      case Point(x, 0) => (x * 16807L + depth) % 20183
//      case Point(0, y) => (y * 48271L + depth) % 20183
//      case Point(x, y) => (erosion(Point(x - 1, y), t, depth) * erosion(Point(x, y - 1), t, depth) + depth) % 20183
//
//    memoize(loop).apply(p)