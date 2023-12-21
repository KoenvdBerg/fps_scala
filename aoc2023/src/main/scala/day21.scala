import scala.io.*
import math.*
import scala.annotation.tailrec
import aoc2022.Algorithms.GraphTraversal
import aoc2022.Algorithms.GraphTraversal.Graph

object aday21 extends App:

  private val day: String =
    this.getClass.getName.drop(4).init

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

    // upl, upr, downl, downr
    val starts: Vector[(Int, Int)] = Vector((0, 0), (maxX-1, 0), (maxX-1, maxY-1), (0, maxY - 1))

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

  val stepSize = 26501365L
  val span = fieldHelper.maxX
  val spanStart = fieldHelper.startPoint._1
  val radius = ((stepSize - spanStart) / span).toLong

  // A
  val ac = fieldHelper.starts.map(st =>
    val g = GraphTraversal.dijkstra(fieldHelper.gardenPath)(st)._1
    g.count(i => i._2 <= spanStart && i._2 % 2 == 0).toLong
  )
  val centerA = res1.count(i => i._2 % 2 == 0 && i._2 <= spanStart).toLong
  val blockSizeA = ac.sum + centerA

  // B
  val bc = fieldHelper.starts.map(st =>
    val g = GraphTraversal.dijkstra(fieldHelper.gardenPath)(st)._1
    g.count(i => i._2 < spanStart && i._2 % 2 == 1).toLong
  )
  val centerB = res1.count(i => i._2 % 2 == 1 && i._2 <= spanStart).toLong
  val blockSizeB = centerB + bc.sum

  val interest = res1.filter(i => i._2 % 2 == 0 && i._2 <= spanStart).keys.toVector
  println(fieldHelper.getPrintable(interest))

  val conesA = 4 * blockSizeA - ac.sum * 2L
  val bigA = ac.map(cc => (blockSizeA - cc) * (radius - 1L)).sum
  val smallB = bc.map(cc => cc * radius).sum


  val testA = res1.count(i => i._2 % 2 == 0 && i._2 <= spanStart)
  println(s"assertion for A: $testA + ${ac.sum} == $blockSizeA")

  val testB = res1.count(i => i._2 % 2 == 1 && i._2 <= spanStart)
  println(s"assertion for B: $testB + ${bc.sum} == $blockSizeB")

  val sa = math.pow(radius - 1L, 2).toLong
  val sb = math.pow(radius, 2).toLong


  println(s"blocks A: $sa")
  println(s"blocks B: $sb")
  println(s"conesA: $conesA")
  println(s"bigA: $bigA")
  println(s"smallB: $smallB")

  // too low: 616538230149620

  private val answer2 = conesA + bigA + sa * blockSizeA + sb * blockSizeB + smallB
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")

