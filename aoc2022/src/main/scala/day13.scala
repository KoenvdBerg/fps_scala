import scala.io.*
import math.*
import scala.annotation.tailrec
import aoc2022.Combinator.Parser
import aoc2022.Combinator.Parser.*
import day13.Packet.parsePacket

/**
 * PART 01:
 *
 * Heavily inspired from the solution of Marco Borst, as this was part of the curriculum. 
 * 
 * Basically reused my parser combinator that I created while going through the book "functional programming in Scala". 
 * With this parser combinator, it was easy to parse in the packets.
 * 
 * Next, I defined an ordering for the packets. This was used to get to the solution of part 01.
 *
 * PART 02:
 *
 * Involved adding the divider packets to the input, and then sorting and retrieving back their indices. The sorting 
 * was easy since I defined the sorting already in part01. 
 *
 */


object day13 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: List[Packet] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .filterNot(_.isBlank)
      .toList
      .map(parsePacket)
    
  enum Packet: 
    case N(n: Int)
    case L(l: List[Packet])
    
  object Packet: 
    
    val n: Parser[N] = Parser.number.map((s: String) => N(s.toInt))
    val l: Parser[L] = (for {
      _ <- Parser.char('[')
      s <- Parser.sequence(Parser.char(','), packet)
      _ <- Parser.char(']')
    } yield s).map(L.apply)
    val packet: Parser[Packet] = n | l
    def parsePacket(s: String): Packet = packet.run(s) match
      case Right(v) => v
      case Left(e) => sys.error(e)
    
    given ordering: Ordering[Packet] with
      override def compare(x: Packet, y: Packet): Int = (x, y) match
        case (N(i1), N(i2))       => i1.compare(i2)
        case (n@N(_), l@L(_))     => compare(L(List(n)), l)
        case (l@L(_), n@N(_))     => compare(l, L(List(n)))
        case (L(Nil), L(_ :: _))  => -1 
        case (L(_ :: _), L(Nil))  => 1 
        case (L(Nil), L(Nil))     => 0
        case (L(h1 :: t1), L(h2 :: t2)) => compare(h1, h2) match
          case 0 => compare(L(t1), L(t2))
          case x => x

  import Packet.*
  import math.Ordered.orderingToOrdered
  private val answer1: Int = input
    .grouped(2)
    .zipWithIndex
    .map((ps: List[Packet], idx: Int) => if ps.head <= ps(1) then idx + 1 else 0)
    .sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  private val div1: Packet = Packet.parsePacket("[[2]]")  
  private val div2: Packet = Packet.parsePacket("[[6]]")  
  
  private val newInput: List[Packet] = (div1 :: div2 :: input).sorted
  private val answer2: Int = (newInput.indexWhere(_ == div1) + 1) * (newInput.indexWhere(_ == div2) + 1) 
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")

