import scala.io.*
import math.*
import scala.annotation.tailrec
import scala.util.matching.Regex

object day01 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: Vector[String] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector

  private val translate: Map[Regex, String] = Map(
    "one".r -> "1",
    "two".r -> "2",
    "three".r -> "3",
    "four".r -> "4",
    "five".r -> "5",
    "six".r -> "6",
    "seven".r -> "7",
    "eight".r -> "8",
    "nine".r -> "9"
  )

  private def findWrittenDigits(s: String): Vector[(Int, String)] =
    translate.foldLeft(Vector.empty) { (res: Vector[(Int, String)], in: (Regex, String)) =>

      val hits: Vector[Int] = in._1.findAllMatchIn(s).map(_.start).toVector
      hits.map(i => (i, in._2)) ++ res
    }

  private def findDigits(s: String): Vector[(Int, String)] =
    s.zipWithIndex.foldLeft(Vector.empty[(Int, String)]) { (res: Vector[(Int, String)], in: (Char, Int)) =>
      if in._1.isDigit then (in._2, s"${in._1}") +: res
      else res
    }
    
  private def resolve(ins: Vector[(Int, String)]): Int =
    val res: Seq[(Int, String)] = ins.filterNot(_._1 == -1).sortBy(_._1)
    (res.head._2 + res.last._2).toInt 
  
  private val answer1 = input.map(findDigits).map(resolve).sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  private val answer2 = input.map(s => findWrittenDigits(s) ++ findDigits(s)).map(resolve).sum
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
