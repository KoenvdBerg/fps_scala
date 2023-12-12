import scala.io.*
import math.*
import scala.annotation.tailrec
import aoc2022.Optim.Memoize
import scala.collection.mutable

object day12 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  case class Arrangement(unknown: String, known: Vector[Int])

  object Arrangement:

    val solveMemoized: Arrangement => Long = Memoize[Arrangement, Long](solve).getMemoizedf

    // https://towardsdatascience.com/solving-nonograms-with-120-lines-of-code-a7c6e0f627e4
    // https://stackoverflow.com/questions/813366/solving-nonograms-picross

    /**
     * (1) Start with an empty board, place the first row
     * (2) Now, with that board, place a second row, check it against the column constraints. If it passes,
     * recursively try the next row against that state; if it doesn't pass, then try the next possible placement of that row.
     * (3) If at any time you run out of possible placements of a row that satisfy the constraints, then the puzzle
     * has no solution. Otherwise, when you run out of rowns, you've solved the problem.
     */
    def solve(arr: Arrangement): Long =
      if arr.known.isEmpty then
        if arr.unknown.forall(p => ".?".contains(p)) then 1L
        else 0L

      else
        val currentGroup: Int = arr.known.head
        val other: Vector[Int] = arr.known.tail
        val lotLeft: Int = other.sum + other.length  // require other.length '.' + other.sum of '#'
        val expectations: Vector[String] = (0 to arr.unknown.length - lotLeft - currentGroup)
          .map(b => "." * b + "#" * currentGroup + ".").toVector
        val total: Vector[Long] =
          for
            exp <- expectations
            if exp.zip(arr.unknown).forall((c1, c2) => c1 == c2 | c2 == '?')
          yield solveMemoized(Arrangement(arr.unknown.drop(exp.length), other))

        total.sum

  private val input: Vector[Arrangement] =

    def parse(s: String): Arrangement =
      val a = s.split(" ")
      val known = a.last.split(",").toVector.map(_.toInt)
      Arrangement(a.head, known)

    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector
      .map(parse)

  private val res1: Vector[Long] = input.map(Arrangement.solve)
  private val answer1: Long = res1.sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  private val newIn: Vector[Arrangement] = input.map(arr =>
    Arrangement(Vector.fill(5)(arr.unknown).mkString("?"), Vector.fill(5)(arr.known).flatten)
  )
  private val res2: Vector[Long] = newIn.map(Arrangement.solve)
  private val answer2: Long = res2.sum
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")

