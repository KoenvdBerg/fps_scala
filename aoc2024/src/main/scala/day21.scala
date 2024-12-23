import aoc2024.BoundedGrid
import aoc2024.BoundedGrid.*
import jdk.jshell.execution.Util
import aoc2024.SequenceUtils.*
import aoc2024.Optim.Memoize

import scala.io.*
import math.*
import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.matching.Regex

object day21 extends App:

  private val day: String = "21"

  private val start1: Long =
    System.currentTimeMillis

  private val codes: Seq[String] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toSeq

  val keypad = BoundedGrid.fromString("789456123#0A", 3)
  val keypadA = (2,3)
  val control = BoundedGrid.fromString("#^A<v>", 3)
  val controlA = (2,0)

  def createMovement(route: Seq[(Int, Int)]): String =
    if route.length <= 1 then "A"
    else
      route.sliding(2).map(t =>
        val t1 = t.head
        val t2 = t.last
        val dir = t2 - t1
        dir match
          case (1, 0) => '>'
          case (-1, 0) => '<'
          case (0, 1) => 'v'
          case (0, -1) => '^'
      ).mkString + "A"

  def navigateKeypad(start: (Int, Int), end: (Int, Int), grid: BoundedGrid): Seq[String]  =

    val q: mutable.Queue[Seq[(Int, Int)]] = mutable.Queue(Seq(start))

    @tailrec
    def go(acc: Seq[Seq[(Int, Int)]]): Seq[Seq[(Int, Int)]] =
      if q.isEmpty then acc
      else
        val cur: Seq[(Int, Int)] = q.dequeue()
        val p = cur.last
        if p == end then go(acc.appended(cur))
        else
          val m = p.manhattan(end)
          val n = grid.neighbours4(p)
            .filterNot(pp => grid.get(pp) == '#')
            .filter(pp => m - pp.manhattan(end) == 1)
          n.foreach(nn => q.enqueue(cur.appended(nn)))
          go(acc)

    go(Seq.empty).map(createMovement)

  def typeSomething(start: (Int, Int), number: String, pad: BoundedGrid): Seq[String] =

    @tailrec
    def go(s: (Int, Int), sequence: String, acc: Seq[String]): Seq[String] =
      if sequence.isEmpty then acc
      else
        val end = pad.getPointsOf(sequence.head).head
        val r: Seq[String] = navigateKeypad(s, end, pad)
        val nextAcc =
          for
            cur <- acc
            rr <- r
          yield cur ++ rr
        go(end, sequence.drop(1), nextAcc)

    go(start, number, Seq(""))

  case class Solver(maxRobot: Int):
    private val mem = Memoize[(String, Int), Long](controlNumber)
    private val memoizef = mem.getMemoizedf

    def controlNumber(in: String, r: Int): Long =
      if r >= maxRobot then in.length.toLong
      else if in.isEmpty then 0L
      else
        val iA = in.indexOf('A')
        val cur = in.take(iA + 1)
        val rem = in.drop(iA + 1)
        val next = typeSomething(controlA, cur, control)
        memoizef(rem, r) + memoizef(next.minBy(ss => memoizef(ss, r + 1)), r + 1)

    def solve(code: String): Long =
      val codeDirs = typeSomething(keypadA, code, keypad)
      codeDirs.map(cc => memoizef(cc, 0)).min * code.filter(_.isDigit).toLong

  private val answer1 = codes.map(s => Solver(2).solve(s)).sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  private val answer2 = codes.map(c => Solver(25).solve(c)).sum
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
