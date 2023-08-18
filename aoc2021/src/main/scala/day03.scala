import scala.annotation.tailrec
import scala.io.*
import aoc2021.NumberTheory.binaryToDec

object day03 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: Vector[String] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector
    
  private val g = input.transpose.map(f => f.groupBy(identity).maxBy(_._2.length)._1).mkString("")
  private val e = input.transpose.map(f => f.groupBy(identity).minBy(_._2.length)._1).mkString("")
  private val answer1 = binaryToDec(g) * binaryToDec(e)
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis
    
    
  def findRating(in: Vector[String], default: Char)(f: Map[Char, Int] => Char): String =

    @tailrec
    def go(bits: Vector[String], i: Int): String =
      if bits.length == 1 then bits.head
      else 
        val t: Map[Char, Int] = bits.map(x => x(i)).groupBy(identity).map(x => (x._1, x._2.length))
        val c: Char = if t('0') == t('1') then default else f(t)
        go(bits.filter(s => s(i) == c), i + 1)
        
    go(in, 0)

  private val oxygen = findRating(input, '1')(f => f.maxBy(_._2)._1)
  private val co2 = findRating(input, '0')(f => f.minBy(_._2)._1)
  private val answer2 = binaryToDec(oxygen) * binaryToDec(co2)
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")