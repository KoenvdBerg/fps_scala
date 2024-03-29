import scala.io.*
import math.*
import scala.annotation.tailrec
import aoc2022.Algorithms.GraphTraversal
import aoc2022.Algorithms.GraphTraversal.Graph

object day21 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: Vector[String] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector

  class GardenNavigator(in: Vector[String]):

    val field: Vector[String] = in
    val maxX: Int = in.head.length
    val maxY: Int = in.length

    private def inBounds(x: Int, y: Int): Boolean = x >= 0 && x < maxX && y >= 0 && y < maxY

    private def neighbours(x: Int, y: Int): Vector[(Int, Int)] =
      Vector(
        (x, y-1),
        (x,y+1),
        (x-1,y),
        (x+1,y))

    def gardenPath: Graph[(Int, Int)] =
      (x: Int, y: Int) =>
        val ns: Vector[(Int, Int)] = neighbours(x, y)
        ns
          .filter((x, y) => inBounds(x, y))
          .filter((x, y) => field(y)(x) != '#')
          .map(c => c -> 1).toMap

    def startPoint: (Int, Int) = (for {
      y <- Range(0, maxY)
      x <- Range(0, maxX)
      if field(y)(x) == 'S'
    } yield (x, y)).head

    def getPrintable(interest: Vector[(Int, Int)]): String =
      interest.foldLeft(field) {(res: Vector[String], in: (Int, Int)) =>
        res.updated(in._2, res(in._2).updated(in._1, 'O'))
      }.mkString("\n")


  private val stepNum = 64
  private val fieldHelper = GardenNavigator(input)
  private val res1 = GraphTraversal.dijkstra(fieldHelper.gardenPath)(fieldHelper.startPoint)._1
  private val answer1 = res1.count(i => i._2 <= stepNum && i._2 % 2 == 0)
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  val stepSize: Long = 26501365L
  val span: Long = fieldHelper.maxX.toLong
  val spanStart: Long = fieldHelper.startPoint._1.toLong
  val radius: Long = (stepSize - spanStart) / span
  val left: Long = (stepSize - spanStart) % span

  val pA = 1
  val Acorners = res1.count(i => i._2 > spanStart && i._2 % 2 == pA).toLong
  val Asize = res1.count(i => i._2 % 2 == pA)

  val pB = 0
  val Bcorners = res1.count(i => i._2 > spanStart && i._2 % 2 == pB).toLong
  val Bsize = res1.count(i => i._2 % 2 == pB).toLong

  val sA = math.pow(radius + 1, 2).toLong
  val sB = math.pow(radius, 2).toLong

  println(fieldHelper.getPrintable(res1.filter(i => i._2 > spanStart && i._2 % 2 == pB).keys.toVector))
  println(fieldHelper.getPrintable(res1.filter(i => i._2 % 2 == pB && i._2 <= 65).keys.toVector))

  val answer2 = sB * Bsize + sA * Asize + (radius * Bcorners) - ((radius + 1) * Acorners) - radius

  println(s"span: $span")
  println(s"spanStart: $spanStart")
  println(s"radius: $radius")
  println(s"ac: $Acorners")
  println(s"bc: $Bcorners")
  println(s"blocksize A: $Asize")
  println(s"blocksize B: $Bsize")
  println(s"blocks A: $sA")
  println(s"blocks B: $sB")
  println(s"left: ${ ((stepSize - spanStart) % span)}")
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
