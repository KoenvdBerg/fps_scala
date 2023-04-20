import scala.io.*
import math.*
import scala.collection.mutable
import scala.collection.mutable.Stack

object day11 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: Int =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList
      .head
      .toInt

  case class Point(x: Int, y: Int)

  object FuelCells:

    private val width: Int = 300
    private val height: Int = 300

    def computePower(x: Int, y: Int, serialNumber: Int): Int =
      ((x + 10) * y + serialNumber) * (x + 10) / 100 % 10 - 5

    def square(pos: Point, size: Int): Vector[Point] =
      (for {
        x <- Range(pos.x, pos.x + size)
        y <- Range(pos.y, pos.y + size)
      } yield Point(x, y)).toVector

    def mostPower(size: Int, serialNumber: Int): (Point, Int) =
      val allPowers: IndexedSeq[(Point, Int)] =
        for {
          x <- Range(0, width-size)
          y <- Range(0, height-size)
        } yield (Point(x,y), square(Point(x, y), size)
          .map(f => computePower(f.x, f.y, serialNumber))
          .sum)
      allPowers.toVector.maxBy(f => f._2)

    def mostPowerForAllSizes(serialNumber: Int): ((Point, Int), Int) =
      val powerForAllSizes: IndexedSeq[((Point, Int), Int)] =
        for {
          s <- Range(0, 20)
        } yield (mostPower(s, serialNumber), s)
      powerForAllSizes.toVector.maxBy(f => f._1._2)

  private val answer1 = FuelCells.mostPower(3, input)
  println(s"Answer day $day part 1: ${answer1._1.x},${answer1._1.y} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  private val answer2 = FuelCells.mostPowerForAllSizes(input)
  println(s"Answer day $day part 2: ${answer2._1._1.x},${answer2._1._1.y},${answer2._2} [${System.currentTimeMillis - start2}ms]")
