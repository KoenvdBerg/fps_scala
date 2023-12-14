import scala.io.*
import math.*
import scala.annotation.tailrec
import aoc2022.FlatGrid
import aoc2022.CycleFinder.Cycle

object day14 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: Field =
    val in = Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector

    Field(in.map(_.trim).mkString.toVector, in.head.length)

  case class Field(rocks: Vector[Char], rowSize: Int):

    @tailrec
    private def rollNorth(i: Int, cur: Int): Field =
      val northi = i - rowSize
      if northi >= 0 && rocks(northi) == '.' then rollNorth(northi, cur)
      else if cur == i then this
      else this.copy(rocks = rocks.updated(cur, '.').updated(i, 'O'))

    def next: Field =
      rocks.zipWithIndex.filter(_._1 == 'O').map(_._2).foldLeft(this) { (res: Field, i: Int) =>
        res.rollNorth(i, i)
      }

    def rotate: Field =
      val rotated = FlatGrid.rotateClockWise(rocks, rowSize)
      Field(rotated._1.toVector, rotated._2)

    def cycle: Field =
      (0 to 3).foldLeft(this) { (res: Field, _: Int) =>
        res.next.rotate
      }

    def score: Int =
      val totalRows = rocks.length / rowSize
      rocks.zipWithIndex.filter(_._1 == 'O').map((_, i) => totalRows - (i / rowSize)).sum


  private val answer1: Int = input.next.score
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  private val cycleSpecs: Cycle[Field] = Cycle.find((f: Field) => f.cycle, input)(_.score)
  private val cn: Int = (1000000000 - cycleSpecs.stemLength) % cycleSpecs.cycleLength
  println(s"cycleLength = ${cycleSpecs.cycleLength}")
  println(s"stemlength = ${cycleSpecs.stemLength}")
  private val answer2: Int = Iterator.iterate(cycleSpecs.first)(_.cycle).drop(cn).next.score
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
