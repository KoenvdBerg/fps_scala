import scala.io.*
import math.*
import scala.annotation.tailrec
import aoc2022.FlatGrid.printFlatGrid

/**
 * PART 01:
 *
 * The difficulty was in adding two cycles for any addx program part. This is solved by appending to the list
 * that tracks the program state twice. See implementation below. 
 *
 * PART 02:
 *
 * Perfect puzzle to reuse my code for printing flatGrids. See code below. 
 *
 */

object day10 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  case class Instruction(ex: String, value: Int)

  private val input: List[Instruction] =

    def parser(s: String): Instruction = s match
      case s"noop"    => Instruction("noop", 0)
      case s"addx $i" => Instruction("addx", i.toInt)

    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList
      .map(parser)

  type Reg = Int
  type Cycle = Int
  def readProgram(s: List[(Reg, Cycle)], instruction: Instruction): List[(Reg, Cycle)] =
    val state: (Reg, Cycle) = s.head
    instruction.ex match
      case "noop" => (state._1, state._2 + 1) :: s
      case "addx" => (state._1 + instruction.value, state._2 + 2) :: (state._1, state._2 + 1) :: s
      case _      => sys.error(s"Couldn't process instruction $instruction")


  private val interesting: List[Int] = List(20, 60, 100, 140, 180, 220)
  private val res1: List[(Int, Int)] = input.foldLeft(List((1, 1)))(readProgram)
  private val answer1: Int = res1
    .filter((r: (Reg, Cycle)) => interesting.contains(r._2))
    .map((r: (Reg, Cycle)) => r._1 * r._2)
    .sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  private val width: Int = 40
  type Image = String
  def drawProgram(s: (Reg, Image), instruction: Instruction): (Reg, Image) =
    val pos: Int = s._2.length % width
    val next1: Char = if pos >= s._1 - 1 && pos <= s._1 + 1 then '#' else '.'
    instruction.ex match
      case "noop" => (s._1, s._2 + next1)
      case "addx" =>
        val next2: Char = if pos+1 >= s._1 - 1 && pos+1 <= s._1 + 1 then '#' else '.'
        (s._1 + instruction.value, s._2 + next1 + next2)
      case _ => sys.error(s"Couldn't process instruction $instruction")


  private val res2: (Int, String) = input.foldLeft((1, ""))(drawProgram)
  println(printFlatGrid(res2._2, width)(identity))
  println(s"Answer day $day part 2: ^^^^^^^^^ [${System.currentTimeMillis - start2}ms]")
