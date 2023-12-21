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

    private def loopAround(x: Int, y: Int): (Int, Int) =
      if inBounds(x, y) then (x, y)
      else if x < 0 then (maxX - 1, y)
      else if x >= maxX then (0, y)
      else if y < 0 then (x, maxY - 1)
      else if y >= maxY then (x, 0)
      else sys.error(s"cannot loop for $x and $y")


    def gardenPath(part: Int): Graph[(Int, Int)] =
      (x: Int, y: Int) =>
        val ns: Vector[(Int, Int)] = neighbours(x, y)
        if part == 1 then
          ns
            .filter((x, y) => inBounds(x, y))
            .filter((x, y) => field(y)(x) != '#')
            .map(c => c -> 1).toMap
        else
          ns
            .map((x, y) => loopAround(x, y))
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
  private val res1 = GraphTraversal.dijkstra(fieldHelper.gardenPath(1))(fieldHelper.startPoint)._1
  private val answer1 = res1.count(i => i._2 <= stepNum && i._2 % 2 == 0)
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  val stepSize = 26501365
  val span = fieldHelper.maxX
  val spanStart = fieldHelper.startPoint._1
  println(s"span: $span")
  println(s"spanStart: $spanStart")

  val amountNormal = (stepSize - spanStart) / span
  val amountLeft = (stepSize - spanStart) % span
  val blockSize = res1.count(i => i._2 % 2 == 0)
  val sides = (amountNormal - 2) * 2
  val cones = 2

  def size(l: Int) = Range(1, l*2, 2).foldLeft(0L)(_ + _)
  val s = size(amountNormal - 1) + size(amountNormal - 2)
  val totalBlocks = sides + cones + s



  println(s"block: $blockSize")
  println(s"amount of blocks to side: $amountNormal")
  println(s"amount of blocks left: $amountLeft")
  println(s"amount of sides: $sides")
  println(s"amount of cones: $cones")
  println(s"amount of blocks within: $s")
  println(s"amount of total blocks: $totalBlocks")

  println(size(5))
  println(size(4))

  // todo: add the left over edges
  //   - 4 cones
  //   - 4 different diagonals

  // todo: try differentiating block-sizes, perhaps that helps

  // too low: 309801382485570 (duh because still have to add cones and diagonals)
  // too low: 309804445292430
  // toohigh: 619602764978710




  println(totalBlocks)

  private val answer2 = totalBlocks * blockSize
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")


//  val interest = res1.filter(i => i._2 % 2 == 0 && i._2 <= stepNum2).keys.toVector
//  println(fieldHelper.getPrintable(res1.filter(i => i._2 % 2 == 0 && i._2 <= stepNum2).keys.toVector))






