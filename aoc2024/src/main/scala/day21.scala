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

object day00 extends App:

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
  val sample = codes.head

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

  val cache: mutable.Map[((Int, Int), (Int, Int), BoundedGrid), Seq[String]] = new mutable.HashMap()
//  def memoizeNavigate: (((Int, Int), (Int, Int), BoundedGrid)) => Seq[String] = Memoize[((Int, Int), (Int, Int), BoundedGrid), Seq[String]](navigateKeypad).f

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

    cache.getOrElseUpdate((start, end, grid), go(Seq.empty).map(createMovement))

  def typeNumber(start: (Int, Int), number: String): Seq[String] =

    @tailrec
    def go(s: (Int, Int), sequence: String, acc: Seq[String]): Seq[String] =
      if sequence.isEmpty then acc
      else
        val end = keypad.getPointsOf(sequence.head).head
        val r: Seq[String] = navigateKeypad(s, end, keypad)

        val nextAcc =
          for
            cur <- acc
            rr <- r
          yield cur ++ rr

        go(end, sequence.drop(1), nextAcc)

    go(start, number, Seq(""))

  def typeNumberControl(start: (Int, Int), directionSequence: String): Seq[String] =

    @tailrec
    def go(s: (Int, Int), sequence: String, acc: Seq[String]): Seq[String] =
      if sequence.isEmpty then acc
      else
        val end = control.getPointsOf(sequence.head).head
        val r = navigateKeypad(s, end, control)
        val nextAcc =
          for
            cur <- acc
            rr <- r
          yield cur ++ rr
        go(end, sequence.drop(1), nextAcc)

    go(start, directionSequence, Seq(""))

  def scoreCode(code: String): Long =
    val sequence = resolve(code)
    code.filter(_.isDigit).toLong * sequence.length

  def heuristic(sequence: String): Int =
    sequence.sliding(2).map(s =>
      val c1 = s.head
      val c2 = s.last
      if c1 == c2 then 1 else 0
    ).sum

  def resolve(code: String): String =
    val s1 = typeNumber(keypadA, code)
    val ts2 = s1.flatMap(seq => typeNumberControl(controlA, seq))
    val ls2 = ts2.maxBy(heuristic).length
    val s2 = ts2.filter(_.length == ls2)

    val s3 = s2.flatMap(seq => typeNumberControl(controlA, seq))
    s3.minBy(_.length)

//  private val answer1 = codes.map(scoreCode).sum
//  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis


//  def typeNumberControl(start: (Int, Int), directionSequence: String): Seq[String] =
//
//    @tailrec
//    def go(s: (Int, Int), sequence: String, acc: Seq[String]): Seq[String] =
//      if sequence.isEmpty then acc
//      else
//        val end = control.getPointsOf(sequence.head).head
//        val r = navigateKeypad(s, end, control)
//        val nextAcc =
//          for
//            cur <- acc
//            rr <- r
//          yield cur ++ rr
//        go(end, sequence.drop(1), nextAcc)
//
//    go(start, directionSequence, Seq(""))

  val memoizef = Memoize[(String, Int), Long](controlNumber).f

  val maxRobot = 2
  def controlNumber(in: String, r: Int): Long =
    if r >= maxRobot then in.length.toLong
    else if in.isEmpty then 0L
    else
      val iA = in.indexOf('A')
      val cur = in.take(iA + 1)
      val rem = in.drop(iA + 1)
      val next = typeNumberControl(controlA, cur).maxBy(heuristic)
//      println(s"$cur --> $next")
      memoizef(rem, r) + memoizef(next, r + 1)
  //      next.map(ns => memoizef(ns, r + 1)).sum + memoizef(rem, r)

//      next.flatMap(ns => controlNumber(ns, r + 1)) + controlNumber(rem, r)






  val res2 = controlNumber("<A^A^>^AvvvA", 0)
  println(res2)

  private val answer2 = ""
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
