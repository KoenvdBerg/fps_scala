import scala.annotation.tailrec
import scala.io.*

object day01 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: List[Int] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList
      .map(_.toInt)

  private val answer1: Int = input.sliding(2).map(f => f(1) > f.head).count(_ == true)
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  private val answer2: Int = input.sliding(3).map(_.sum).toList.sliding(2).map(f => f(1) > f.head).count(_ == true)
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")