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
    val (x, cycle): (Reg, Cycle) = s.head
    instruction.ex match
      case "noop" => (x, cycle + 1) :: s
      case "addx" => (x + instruction.value, cycle + 2) :: (x, cycle + 1) :: s
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
    
    def nextPixel(i: Int, regX: Int): Char = if i >= regX - 1 && i <= regX + 1 then '#' else '.'
    
    val (x, image): (Reg, Image) = s
    val pos: Int = image.length % width
    instruction.ex match
      case "noop" => (x, image + nextPixel(pos, x))
      case "addx" =>
        (x + instruction.value, image + nextPixel(pos, x) + nextPixel(pos + 1, x))
      case _ => sys.error(s"Couldn't process instruction $instruction")


  private val res2: (Int, String) = input.foldLeft((1, ""))(drawProgram)
  println(printFlatGrid(res2._2, width)(identity))
  println(s"Answer day $day part 2: ^^^^^^^^^ [${System.currentTimeMillis - start2}ms]")
