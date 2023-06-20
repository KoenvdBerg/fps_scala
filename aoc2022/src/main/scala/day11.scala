import scala.io.*
import math.*
import scala.annotation.tailrec

/**
 * PART 01:
 *
 * Involves following the instructions and implementing them. 
 *
 * PART 02:
 *
 * Uses the Chinese Remainder Theorem. Had to look that up based on a hint.
 * 
 * https://en.wikipedia.org/wiki/Chinese_remainder_theorem 
 *
 */


object day11 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: Vector[Monkey] =

    def parseMonkey(s: String): Monkey = s match
      case s"Monkey$id:Startingitems:${items}Operation:${op}Test:divisibleby${test}Iftrue:throwtomonkey${t}Iffalse:throwtomonkey$f" =>
        Monkey(id.toInt, items.split(",").map(_.toLong).toVector, parseOp(op), (i: Long) => i % test.toInt == 0, test.toInt, (t.toInt, f.toInt), 0L)
      case _ => sys.error(s"error, couldn't parse monkey string: $s")

    def parseOp(s: String): Long => Long = s match
      case s"new=old*old" => (in: Long) => in * in
      case s"new=old*$i" => (in: Long) => in * i.toInt
      case s"new=old+$i" => (in: Long) => in + i.toInt
      case _ => sys.error(s"couldn't parse operation: $s")

    Source
      .fromResource(s"day$day.txt")
      .getLines
      .grouped(7)
      .map(_.mkString("").filterNot(_.isWhitespace))
      .map(parseMonkey)
      .toVector

  case class Monkey(id: Int, items: Vector[Long], op: Long => Long, test: Long => Boolean, div: Int, todo: (Int, Int), times: Long):
    def inspect(reliefVal: Int, CRT: Int): (Monkey, Vector[(Long, Int)]) =
      val nextItems: Vector[(Long, Int)] = items
        .map((i: Long) => op(i) / reliefVal % CRT)  // CRT = Chinese Remainder Theorem
        .map((i: Long) => (i, if test(i) then todo._1 else todo._2))
      (copy(items = Vector.empty[Long], times = times + items.length), nextItems)

    def receive(inbound: Long): Monkey =
      copy(items = items.appended(inbound))

  object Monkey:
    @tailrec
    def round(monkeys: Vector[Monkey], relief: (Int, Int), n: Int = 0): Vector[Monkey] =
      if n >= monkeys.size then monkeys
      else
        val (newM, inspecting): (Monkey, Vector[(Long, Int)]) = monkeys(n).inspect(relief._1, relief._2)
        val nextMonkeys: Vector[Monkey] = inspecting.foldLeft(monkeys)((ms: Vector[Monkey], u: (Long, Int)) =>
          ms.updated(u._2, ms(u._2).receive(u._1)))
        round(nextMonkeys.updated(n, newM), relief, n + 1)

    @tailrec
    def simulate(monkeys: Vector[Monkey], relief: (Int, Int), maxRounds: Int, n: Int = 0): Vector[Monkey] =
      if n >= maxRounds then monkeys
      else
        val nextMonkeys: Vector[Monkey] = round(monkeys, relief)
        simulate(nextMonkeys, relief, maxRounds, n + 1)

  private val res1: Vector[Monkey] = Monkey.simulate(input, (3, Int.MaxValue), 20)
  private val answer1: Long = res1.map(_.times).sorted.takeRight(2).product
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  private val allDivs: Int = input.map(_.div).product
  private val res2: Vector[Monkey] = Monkey.simulate(input, (1, allDivs), 10000)
  private val answer2: Long = res2.map(_.times).sorted.takeRight(2).product
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
