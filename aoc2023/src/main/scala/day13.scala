import scala.io.*
import math.*
import scala.annotation.tailrec

object day13 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: Vector[Vector[String]] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .mkString("\n")
      .split("\n\n")
      .toVector
      .map(_.split("\n").toVector)

  enum Orient:
    case H, V

  def scan(field: Vector[String], lineCoord: Int, orient: Orient): Boolean =
    val toScan: Vector[String] = if orient == Orient.H then field.transpose.map(_.mkString) else field
    val sliceB: Vector[String] = toScan.slice(0, lineCoord).reverse
    val sliceF: Vector[String] = toScan.slice(lineCoord, toScan.length)
    sliceB.zip(sliceF).forall((s1, s2) => s1 == s2)

  def findReflection(field: Vector[String]): Vector[(Int, Orient)] =

    def reflect(is: Range, orient: Orient): Vector[(Int, Orient)] =
      // removing first and last column/row because they don't make sense to reflect
      is.drop(1).dropRight(1).filter(f => scan(field, f, orient)).map(i => (i, orient)).toVector

    Vector(
      reflect(0 to field.head.length, Orient.H),
      reflect(0 to field.length, Orient.V),
    ).flatten

  def score(in: (Int, Orient)): Long = in._2 match
    case Orient.H => in._1.toLong
    case Orient.V => in._1 * 100L

  private val res1: Vector[(Int, Orient)] = input.flatMap(findReflection)
  private val answer1: Long = res1.map(score).sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  def findNewReflection(field: Vector[String]): Long =
    val old: Vector[(Int, Orient)] = findReflection(field)
    val xmax: Int = field.head.length
    val ymax: Int = field.length

    @tailrec
    def go(x: Int, y: Int): Long =
      if y >= ymax then sys.error("Cannot find")
      if x >= xmax then go(0, y + 1)
      else
        val smutch: Char = field(y)(x)
        val repl: Char = if smutch == '.' then '#' else '.'
        val patchedField: Vector[String] = field.updated(y, field(y).updated(x, repl))
        val newReflection: Vector[(Int, Orient)] = findReflection(patchedField)
        if newReflection == old then go(x + 1, y)
        else if newReflection.isEmpty then go(x + 1, y)
        else
          val res: (Int, Orient) = newReflection.diff(old).head
          score(res)
    go(0, 0)

  private val res2: Vector[Long] = input.map(findNewReflection)
  private val answer2: Long = res2.sum
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")