import scala.annotation.tailrec
import scala.io.*
import aoc2021.Algorithms.GraphTraversal.*
import aoc2021.FlatGrid.neighbours4

object day15 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val in: Int => (Int, Vector[Int]) =

    def wrap(i: Int): Int =
      if i <= 9 then i else i - 9

    def times(n: Int, s: String, width: Int): Vector[Int] =
      (s * n).foldLeft((0, Vector.empty[Int])){ (i: (Int, Vector[Int]), ss: Char) =>
        val updated: Int = s"$ss".toInt + i._1 / width
        (i._1 + 1, wrap(updated) +: i._2)
      }._2.reverse

    val infile = Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector

    (n: Int) =>
      val width: Int = infile.head.length
      val horizontal: String = infile.flatMap(s => times(n, s, width)).mkString("")
      val vertical: Vector[Int] = times(n, horizontal, horizontal.length)
      (width * n, vertical)

  def riskMap(risk: Vector[Int], width: Int): Graph[Int] =
    (i: Int) =>
      neighbours4(i, width, risk.length)
        .map((f: Int) => (f, risk(f)))
        .toMap

  private val (width, field): (Int, Vector[Int]) = in(1)
  private val res1: Option[Int] = shortestDistance(riskMap(field, width))(0, field.length - 1)
  private val answer1: Int = res1.get
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  private val (width2, field2): (Int, Vector[Int]) = in(5)
  private val res2: Option[Int] = shortestDistance(riskMap(field2, width2))(0, field2.length - 1)
  private val answer2: Int = res2.get
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")