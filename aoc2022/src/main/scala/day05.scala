import scala.io.*
import math.*
import scala.annotation.tailrec

/**
 * PART 01:
 *
 * The tricky part of this day was to correctly parse in the input file. Especially the stacks are hard to parse. I 
 * ended up using the .grouped(4) method on strings, which groupes the strings each 4 characters. Then I transposed 
 * that like this: 
 * 
 * Vector(List("", [D], ""), Vector([N], [C], ""), Vector([Z], [M], [P]), List(1, 2, 3))
 * 
 * to:
 * 
 * Vector(Vector("", [N], [Z], 1), Vector([D], [C], [M], 2), Vector("", "", [P], 3)) 
 * 
 * Then I converted it in a Stacks like this: Vector(Vector([N], [Z]), Vector([D], [C], [M]), Vector([P]))
 * The indexation of the vector determines the positions of each stack.
 * 
 * After parsing the input stack, it was trivial to create the logic of changing a stack state to the next stated 
 * based on a move. Result is obtained by foldLeft on every move with the stack as fold result. 
 *
 * PART 02:
 *
 * Changed the exectuteMove function to also work with the non-reversed order for part02, moving everything at once. 
 *
 */


object day05 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  case class Move(qty: Int, src: Int, tgt: Int)
  type Stacks = Vector[Vector[String]]
  
  private val (stack, moves): (Stacks, Vector[Move]) =
    
    def parseStacks(s: String): Vector[String] =
      s.grouped(4).map(_.trim).toVector

    def parseMoves(s: String): Option[Move] = s match
      case s"move $qty from $src to $tgt" => Some(Move(qty.toInt, src.toInt - 1, tgt.toInt - 1))
      case _ => None

    val infile: Vector[String] = Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector
    
    val stacks: Stacks = infile
      .takeWhile(_ != "")   // take the stack lines i.e. up until the first move line
      .map(parseStacks)     // convert to a Vector of containers in stack
      .transpose
      .sortBy((s: Vector[String]) => s.last.toInt)
      .map((s: Vector[String]) => s.dropRight(1).filter(_ != ""))
    
    (stacks, infile.flatMap(parseMoves))

  def exectuteMove(atOnce: Boolean)(stacks: Stacks, m: Move): Stacks =
    val (toMove, remainder): (Vector[String], Vector[String]) = stacks(m.src).splitAt(m.qty)
    val toUpdate: Vector[String] = if atOnce then toMove else toMove.reverse
    stacks
      .updated(m.src, remainder) // remove from source stack
      .updated(m.tgt, toUpdate ++ stacks(m.tgt)) // append to head of target stack
  
  def topContainerString(stacks: Stacks): String =
    stacks
      .map(_.head)  // take top container from each stack
      .mkString("")
      .replace("[", "")  // remove the square brackets
      .replace("]", "")  

  private val res1: Stacks = moves.foldLeft(stack)(exectuteMove(false))
  private val answer1: String = topContainerString(res1)
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  private val res2: Stacks = moves.foldLeft(stack)(exectuteMove(true))
  private val answer2: String = topContainerString(res2)
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
