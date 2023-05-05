import scala.io.*
import math.*
import scala.collection.mutable

object day18 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val area: Vector[String] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector

  private val rowSize: Int = area.head.length
  private val nTiles: Int = area.flatten.length

  // https://stackoverflow.com/questions/16257378/is-there-a-generic-way-to-memoize-in-scala
  def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]():
    override def apply(key: I) = getOrElseUpdate(key, f(key))

  def neighbours8(i: Int): Vector[Int] =

    def sides(pos: Int): Vector[Int] =
      val left: Int = if pos % rowSize != 0 then pos - 1 else -1
      val right: Int = if (pos + 1) % rowSize != 0 then pos + 1 else -1
      Vector(left, pos, right)

    def get(pos: Int): Vector[Int] =
      val vertical: Vector[Int] = Vector(pos - rowSize, pos, pos + rowSize)
      vertical
        .flatMap(sides)
        .filter(i => i >= 0 && i < nTiles && i != pos)

    memoize(get).apply(i)

  def sim(in: Vector[Char], exit: Int, min: Int = 0): Vector[Char] =

    def go(area: Vector[Char], next: Vector[Char], tile: Int = 0): Vector[Char] =
        if tile >= area.length then next
        else
          val nbrs: Vector[Char] = neighbours8(tile).map(area(_))
          area(tile) match
          case '.' if nbrs.count(_ == '|') >= 3 => go(area, next.patch(tile, Vector('|'), 1), tile + 1)
          case '|' if nbrs.count(_ == '#') >= 3 => go(area, next.patch(tile, Vector('#'), 1), tile + 1)
          case '#' if nbrs.count(_ == '#') < 1 || nbrs.count(_ == '|') < 1 => go(area, next.patch(tile, Vector('.'), 1), tile + 1)
          case _ => go(area, next, tile + 1)

    println(min)
    printArea(in)
    if min >= exit then in
    else
      val next: Vector[Char] = go(in, in)
      sim(next, exit, min + 1)

  def printArea(area: Vector[Char]): Unit =
    val (p, next) = area.splitAt(rowSize)
    if p.isEmpty then ()
    else
      p.foreach(print(_))
      println()
      printArea(next)

  private val res1: Vector[Char] = sim(area.flatten, 10)
  private val answer1 = res1.count(_ == '|') * res1.count(_ == '#')
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")



  private val start2: Long =
    System.currentTimeMillis


  /**
   * this map is build up from the moment that the score kept repeating itself in a cycle of length 28.
   * I build this by setting the simulation exit to a 1000 and copying the first repeat and making it like below
   * using a multiline cursor from the output with below pasted in the sim function:
   *
   * println(s" | = ${in.count(_ == '|')}, # = ${in.count(_ == '#')}, min: $min, score = ${in.count(_ == '|') * in.count(_ == '#')}")
   *
   */

  private val sustainableScore: Map[Int, Int] = Map(
    0 -> 192100, 1 -> 196080, 2 -> 198831, 3 -> 202176, 4 -> 202650, 5 -> 205545, 6 -> 205900,
    7-> 208080, 8 -> 207000, 9 -> 202842, 10 -> 200992, 11 -> 196836, 12 -> 192792, 13 -> 188916, 14 -> 184877,
    15 -> 180295, 16 -> 177885, 17 -> 175824, 18 -> 173712, 19 -> 172900, 20 -> 173988, 21 -> 175932, 22 -> 178100,
    23 -> 181608, 24 -> 184592, 25 -> 187488, 26 -> 190179, 27 -> 194106)

  // at minute 489 the score starts repeating itself
  private val res2: Int = (1000000000 - 489) % sustainableScore.size
  println(res2)
  private val answer2: Int = sustainableScore(res2)
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
