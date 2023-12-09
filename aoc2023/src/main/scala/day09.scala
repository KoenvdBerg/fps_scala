import scala.io.*
import math.*
import scala.annotation.tailrec

object day09 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: Vector[Vector[Long]] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector
      .map(_.trim.split(" ").toVector.map(_.trim.toLong))

  def goDownIn(history: Vector[Long]): Vector[Vector[Long]] =

    def go(cur: Vector[Long], res: Vector[Vector[Long]]): Vector[Vector[Long]] =
      if cur.forall(_ == 0) then cur +: res
      else
        val next = cur.sliding(2).map(f => f(1) - f(0)).toVector
        go(next, cur +: res)

    go(history, Vector.empty)

  def extrapolate(history: Vector[Long], part: Int): Long =
    val allTheWayDown: Vector[Vector[Long]] = goDownIn(history)

    allTheWayDown.foldLeft(0L) { (res: Long, layer: Vector[Long]) =>
      if part == 2 then layer.head - res else res + layer.last
    }

  private val answer1 = input.map(hist => extrapolate(hist, 1)).sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  private val answer2 = input.map(hist => extrapolate(hist, 2)).sum
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
