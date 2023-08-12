import scala.annotation.tailrec
import scala.io.*

object day07 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: List[Int] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList.head
      .split(",").toList
      .map(_.toInt)

  def triangleNum(i: Int): Int = (0 to i).sum

  private val range = (input.min to input.max)
  private val answer1: Int = range.map((i: Int) => input.map((j: Int) => math.abs(i - j)).sum).min

  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  private val answer2: Int = range.map((i: Int) => input.map((j: Int) => triangleNum(math.abs(i - j))).sum).min
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")