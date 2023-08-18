import scala.annotation.tailrec
import scala.io.*
import aoc2021.NumberTheory.binaryToDec
import scala.collection.immutable.Queue

object day16 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: String =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList.head
    
  val hex: Map[Char, String] = Map(
    '0' -> "0000",
    '1' -> "0001",
    '2' -> "0010",
    '3' -> "0011",
    '4' -> "0100",
    '5' -> "0101",
    '6' -> "0110",
    '7' -> "0111",
    '8' -> "1000",
    '9' -> "1001",
    'A' -> "1010",
    'B' -> "1011",
    'C' -> "1100",
    'D' -> "1101",
    'E' -> "1110",
    'F' -> "1111")
  
  enum Tree:
    case Terminal(version: Int, typeID: Int, value: Long)
    case Packet(version: Int, typeID: Int, sub: List[Tree])
    
    def sumVersion: Int = this match
      case Terminal(v, _, _) => v
      case Packet(v, _, li)  => v + li.map((t: Tree) => t.sumVersion).sum
      
    def expression: Long = this match
      case Terminal(_, _, v) => v
      case Packet(_, 0, li) => li.map((t: Tree) => t.expression).sum
      case Packet(_, 1, li) => li.map((t: Tree) => t.expression).product
      case Packet(_, 2, li) => li.map((t: Tree) => t.expression).min
      case Packet(_, 3, li) => li.map((t: Tree) => t.expression).max
      case Packet(_, 5, li) => if li(0).expression > li(1).expression then 1L else 0L 
      case Packet(_, 6, li) => if li(0).expression < li(1).expression then 1L else 0L
      case Packet(_, 7, li) => if li(0).expression == li(1).expression then 1L else 0L
      case _                => -1L


  object Tree:

    def hexToBin(in: String): String = in.flatMap(hex)

    def binToPacket(binary: String): (Tree, Int) =
      val version: Int = binaryToDec(binary.slice(0, 3)).toInt
      val typeID: Int = binaryToDec(binary.slice(3, 6)).toInt
      if typeID == 4 then
        val g: List[String] = binary.drop(6).grouped(5).toList
        val until: Int = g.indexWhere(p => p(0) == '0') + 1
        val number: String = g.slice(0, until).flatMap(s => s.slice(1, 5)).mkString("")
        (Terminal(version, typeID, binaryToDec(number)), number.length + until + 6)
      else
        binary.slice(6, 7) match
          case "0" =>
            val subLen: Int = binaryToDec(binary.slice(7, 22)).toInt
            val next: String = binary.slice(22, 22 + subLen)
            (Packet(version, typeID, helper0(next, Nil)), 22 + subLen)
          case "1" =>
            val subPackets: Int = binaryToDec(binary.slice(7, 18)).toInt
            val next: String = binary.drop(18)
            val packets: (List[Tree], Int) = helper1(next, Nil, subPackets, 0)
            (Packet(version, typeID, packets._1), 18 + packets._2)
          case _  => sys.error("HAHAH I cannot parse your packets")

    @tailrec
    def helper0(binary: String, acc: List[Tree]): List[Tree] =
      val (tree, i): (Tree, Int) = binToPacket(binary)
      val next: String = binary.drop(i)
      val nAcc: List[Tree] = tree :: acc
      if next.isEmpty then nAcc.reverse else helper0(next, nAcc)

    @tailrec
    def helper1(binary: String, acc: List[Tree], nMax: Int, chars: Int): (List[Tree], Int) =
      val (tree, i): (Tree, Int) = binToPacket(binary)
      val next: String = binary.drop(i)
      val nAcc: List[Tree] = tree :: acc
      if nAcc.length >= nMax then (nAcc.reverse, chars + i) else helper1(next, nAcc, nMax, chars + i)
      
  private val res1: Tree = Tree.binToPacket(Tree.hexToBin(input))._1
  private val answer1: Int = res1.sumVersion
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  private val answer2: Long = res1.expression
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
