import scala.io.*
import math.*
import scala.annotation.tailrec

/**
 * PART 01:
 *
 * The tricky part of this day was to correctly parse in the input file. Especially the stacks are hard to parse. I 
 * ended up replacing the spaces between containers with empty containers, like this "[]". Then the string was easy
 * to parse. To link each container stack to its correct ID, I transposed the List of containers, like so: 
 * 
 * List(List(, D, ), List(N, C, ), List(Z, M, P), List(1, 2, 3))
 * 
 * to:
 * 
 * List(List(, N, Z, 1), List(D, C, M, 2), List(, , P, 3)) 
 * 
 * Then I converted it in a map like this: Map(1 -> List(N, Z), 2 -> List(D, C, M), 3 -> List(P))
 * 
 * After parsing the input stack, it was trivial to create the logic of changing a stack state to the next stated 
 * based on a move. Result is obtained by foldLeft on every move with the stack as fold result. 
 *
 * PART 02:
 *
 * Changed the exectuteMove function to also work with the non-reversed order for part02. 
 *
 */


object day05 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  case class Move(qty: Int, src: Int, tgt: Int)
  
  private val (stack, moves): (Map[Int, List[Char]], List[Move]) =

    def parseStacks(s: String): List[String] =
      if s.contains('[') then
        val stackRow: String = s.replace("    ", " []")
        stackRow.split(" ").map(_.replace("[", "").replace("]", "")).toList
      else s.split(" ").filterNot(_ == "").toList

    def parseMoves(s: String): Option[Move] = s match
      case s"move $qty from $src to $tgt" => Some(Move(qty.toInt, src.toInt, tgt.toInt))
      case _ => None

    val infile: List[String] = Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList
    
    val stacks: Map[Int, List[Char]] = infile
      .takeWhile(_ != "")   // take the stack lines i.e. up until the first move line
      .map(parseStacks)     // convert to a list of containers in stack, including empty filler containers
      .transpose
      .map((s: List[String]) => (s.takeRight(1).head.toInt, s.dropRight(1).filter(_ != "").map(_.head)))
      .toMap

    (stacks, infile.flatMap(parseMoves))

  def exectuteMove(order: String)(stacks: Map[Int, List[Char]], m: Move): Map[Int, List[Char]] =
    if order == "part01" then 
      stacks
        .updated(m.src, stacks(m.src).drop(m.qty))                           // remove from source stack
        .updated(m.tgt, stacks(m.src).take(m.qty).reverse ++ stacks(m.tgt))  // append to head of target stack in reverse order
    else
      stacks
        .updated(m.src, stacks(m.src).drop(m.qty))                  // remove from source stack
        .updated(m.tgt, stacks(m.src).take(m.qty) ++ stacks(m.tgt)) // append to head of target stack
  
  def topContainerString(stacks: Map[Int, List[Char]]): String =
    stacks.toVector.sortBy(_._1).map(_._2.head).mkString("")  // always sort first on stack id

  
  private val res1: Map[Int, List[Char]] = moves.foldLeft(stack)(exectuteMove("part01"))
  private val answer1: String = topContainerString(res1)
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  private val res2: Map[Int, List[Char]] = moves.foldLeft(stack)(exectuteMove("part02"))
  private val answer2: String = topContainerString(res2)
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
