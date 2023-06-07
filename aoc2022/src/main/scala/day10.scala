import scala.io.*
import math.*
import scala.annotation.tailrec

/**
 * PART 01:
 *
 * PART 02:
 *
 */

object day10 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: List[String] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList


  def interpretProgram(s: String, c: Int, x: Int): List[(Int, Int)] =
    if s.contains("noop") then List((c + 1, x + 0))
    else
      val value = s.split(" ").last.toInt
      List((c + 1, x), (c + 2, x + value))

  def readAllProgram(s: List[String]): List[(Int, Int)] =
    @tailrec
    def loop(s: List[String], acc: List[(Int, Int)], c: Int, x: Int): List[(Int, Int)] = s match
      case Nil => acc
      case h :: tail =>
        val res = interpretProgram(h, c, x)
        loop(tail, acc ::: res, res.last.head, res.last.last)

    loop(s, Nil, 1, 1)


  val interesting: List[Int] = List(20, 60, 100, 140, 180, 220)
  val res: List[(Int, Int)] = readAllProgram(input)
  private val answer1: Int = interesting.map(i => res(i - 2)).map(x => x(0) * x(1)).sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  val answer2 = None
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
