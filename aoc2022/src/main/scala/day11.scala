import scala.io.*
import math.*
import scala.annotation.tailrec

/**
 * PART 01:
 *
 * PART 02:
 *
 */


object day11 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: List[String] =
    
    def parseMonkey(s: String): Monkey = s match
      case s"$id:Startingitems:${items}Operation:new=old*19Test:divisibleby23Iftrue:throwtomonkey2Iffalse:throwtomonkey3"
    
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .mkString("")
      .split("Monkey")
      .map(_.filterNot(_.isWhitespace))
      .toList.drop(1)

  case class Monkey(id: Int, items: List[Int], op: Int => Int, test: Int => Boolean, todo: (Int, Int))

  println(input)
  private val answer1 = None
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  private val answer2 = None
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
