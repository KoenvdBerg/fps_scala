import scala.io.*
import math.*
import scala.collection.mutable
import scala.collection.mutable.Stack

/**
 *
 * Solving this day consisted of the following steps:
 *
 *  1. The computePower function --> computes the power for any coordinate in the 300x300 fuel grid
 *
 *  2. The square function --> creates a square sub-grid starting from any coordinate with a variable size. This function
 *  creates the square sub-grid downwards and to the right of the starting coordinate.
 *
 *  3. The mostPower function --> computes the fuel sub grid of size 3 that has the highest cumulative power in its cells.
 *  Interesting in this function is that it nicely tracks the coordinates and that it doesn't create out-of-bound square
 *  sub-grids through subtracting the 300 main grid by the subgrid size.
 *
 *  4. The mostPowerForAllSizes function --> utilizes the mostPower function to compute multiple highest power sub-grids
 *  for subgrid sizes between 1 and 20. Higher than 20 the algorithm started slowing down drastically. However, seeing
 *  that all given examples used grids below 20, I figured that my grid would probably also be below 20. This turned out
 *  to be right. I think this can be mathematically proven some way somehow, but that's out of scope here.
 *
 */

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
