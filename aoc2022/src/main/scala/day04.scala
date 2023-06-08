import scala.io.*
import math.*
import scala.annotation.tailrec

/**
 * PART 01:
 *
 * Easy fold
 *
 * PART 02:
 *
 * change the logic a bit and then fold
 *
 */


object day04 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  case class Section(s: Int, e: Int)
  private val input: List[(Section, Section)] =

    def parser(s: String): (Section, Section) = s match
      case s"$x-$y,$z-$q" => (Section(x.toInt, y.toInt), Section(z.toInt, q.toInt))
      case _              => sys.error("BOOM")

    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList
      .map(parser)

  def logic(i: Int, sections: (Section, Section)): Int =
    if sections._1.s >= sections._2.s && sections._1.e <= sections._2.e then i + 1
    else if sections._2.s >= sections._1.s && sections._2.e <= sections._1.e then i + 1
    else i

  private val answer1 = input.foldLeft(0)(logic)
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  def overlappingSections(i: Int, sections: (Section, Section)): Int =
    val range1: Set[Int] = Range(sections._1.s, sections._1.e + 1).toSet
    val range2: Set[Int] = Range(sections._2.s, sections._2.e + 1).toSet
    if range1.intersect(range2).nonEmpty then i + 1 else i

  val answer2 = input.foldLeft(0)(overlappingSections)
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
