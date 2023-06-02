import scala.io.*
import math.*
import scala.annotation.tailrec


/**
 *
 */

object day25 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: Vector[Coord] =

    def parse(s: String): Coord = s match
      case s"$x,$y,$z,$q" => (x.trim.toInt, y.toInt, z.toInt, q.toInt)
      case _ => sys.error("BOOM")

    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector
      .map(parse)

  type Coord = (Int, Int, Int, Int)
  type Const = Vector[Coord]
  type Galaxy = Vector[Const]

  def distance(c1: Coord, c2: Coord): Int =
    math.abs(c1._1 - c2._1) + math.abs(c1._2 - c2._2) + math.abs(c1._3 - c2._3) + math.abs(c1._4 - c2._4)

  object Galaxy:
    def canMerge(x: Const, y: Const): Boolean =

      def check(m: Coord, search: Const): Boolean =
        search.map((c: Coord) => distance(c, m)).count(_ <= 3) > 0

      @tailrec
      def loop(xx: Const): Boolean = xx match
        case h +: t =>
          if check(h, y) then true  // loop over x and search against y with check()
          else loop(t)
        case _      => false

      loop(x)

    def createGalaxy(in: Galaxy): Galaxy =

      @tailrec
      def goMatch(matchTo: Const, search: Galaxy, i: Int = 0): Option[Int] = search match
        case h +: t =>
          if canMerge(h, matchTo) then Some(i)
          else goMatch(matchTo, t, i + 1)
        case _      => None

      @tailrec
      def go(c: Galaxy, acc: Galaxy): Galaxy = c match
        case h +: t  =>
          goMatch(h, t) match
          case Some(i) => go(t.updated(i, h ++  t(i)), acc)  // append h to its match
          case None    => go(t, h +: acc)  // append unmatchable to acc and continue building new System
        case _       => acc

      go(in, Vector.empty[Const])





  val test = Galaxy.createGalaxy(input.map(Vector(_)))
  println(test)

  private val answer1 = test.length
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  private val answer2 = None
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")