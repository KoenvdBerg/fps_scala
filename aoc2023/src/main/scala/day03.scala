import scala.io.*
import math.*
import scala.annotation.tailrec
import aoc2022.FlatGrid

import scala.util.matching.Regex

object day03 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val (rowL, input): (Int, Vector[Char]) =
    val rows = Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector

    (rows.head.length, rows.fold("")(_ + _).toVector)

  def aintPart(search: Vector[Int]): Boolean =
    val allNeighbours = search.flatMap { (i: Int) => FlatGrid.neighbours8(i, rowL, input.length)}
    allNeighbours.map(input).forall { (c: Char) => c == '.' | c.isDigit }

  def findNumberIndices(search: Vector[Char]): Vector[Vector[Int]] =
    search.indices.foldLeft(Vector.empty, Vector.empty){ (res: (Vector[Vector[Int]], Vector[Int]), i: Int) =>
      if search(i).isDigit then
        if (i + 1) % rowL == 0 then (res._1.appended(res._2.appended(i)), Vector.empty)
        else (res._1, res._2.appended(i))
      else (res._1.appended(res._2), Vector.empty)
    }._1

  def asLong(vi: Vector[Int]): Long = vi.foldLeft(""){ (res: String, i: Int) => res + s"${input(i)}" }.toLong

  private val nums: Seq[Vector[Int]] = findNumberIndices(input).filter(_.nonEmpty)
  private val aintParts: Long = nums.filter(aintPart).map(asLong).sum
  private val answer1: Long = nums.map(asLong).sum - aintParts
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  private val gearNrs: Seq[Long] = input
    .zipWithIndex.filter(_._1 == '*').map(_._2)
    .map(i => FlatGrid.neighbours8(i, rowL, input.length))
    .map(gs => nums.filter(ns => ns.exists(gs.contains)))
    .filter(_.length >= 2)
    .map(_.map(asLong).product)

  private val answer2: Long = gearNrs.sum
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
