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

  private val input: List[Instruction] =

    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList
      .map(Instruction.parser)

  enum Instruction: 
    case Noop
    case Addx(value: Int)
    
  object Instruction:

    type Reg = Int
    type Cycle = Int
    type Image = String
    
    def parser(s: String): Instruction = s match
      case s"noop" => Noop
      case s"addx $i" => Addx(i.toInt)
    
    def readProgram(s: List[(Reg, Cycle)], instruction: Instruction): List[(Reg, Cycle)] =
      val (x, cycle): (Reg, Cycle) = s.head
      instruction match
        case Noop        => (x, cycle + 1) :: s
        case Addx(value) => (x + value, cycle + 2) :: (x, cycle + 1) :: s
 
    def drawProgram(width: Int)(s: (Reg, Image), instruction: Instruction): (Reg, Image) =

      def nextPixel(i: Int, regX: Int): Char = if i >= regX - 1 && i <= regX + 1 then '#' else '.'

      val (x, image): (Reg, Image) = s
      val pos: Int = image.length % width
      instruction match
        case Noop        => (x, image + nextPixel(pos, x))
        case Addx(value) => (x + value, image + nextPixel(pos, x) + nextPixel(pos + 1, x))

  import Instruction.*
  
  private val interesting: List[Int] = (20 to 220 by 40).toList
  private val res1: List[(Int, Int)] = input.foldLeft(List((1, 1)))(readProgram)
  private val answer1: Int = res1
    .filter((r: (Reg, Cycle)) => interesting.contains(r._2))
    .map((r: (Reg, Cycle)) => r._1 * r._2)
    .sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  private val res2: (Int, String) = input.foldLeft((1, ""))(drawProgram(40))
  println(printFlatGrid(res2._2, 40)(identity))
  println(s"Answer day $day part 2: ^^^^^^^^^ [${System.currentTimeMillis - start2}ms]")
