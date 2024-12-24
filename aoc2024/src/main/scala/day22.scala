import jdk.jshell.execution.Util

import scala.io.*
import math.*
import scala.annotation.tailrec
import scala.util.matching.Regex

object day22 extends App:

  private val day: String = "22"

  private val start1: Long =
    System.currentTimeMillis

  private val input: Seq[Long] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toSeq
      .map(_.toLong)

  def mix(l: Long, secret: Long): Long = l ^ secret

  def prune(l: Long): Long = l % 16777216L

  def next(secret: Long): Long =
    val s1 = prune(mix(secret * 64, secret))
    val s2 = prune(mix(s1 / 32, s1))
    prune(mix(s2 * 2048, s2))

  def it(secret: Long, n: Int): Long = Iterator.iterate(secret)(next).drop(n).next()
  def lazyIt(secret: Long): Iterator[Long] = Iterator.iterate(secret)(next)

  private val answer1 = input.map(secret => it(secret, 2000)).sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  def lastDigit(l: Long): Int = s"$l".takeRight(1).toInt

  def prizes(secret: Long, n: Int): Seq[(Int, Int)] =
    Iterator.iterate(secret)(next).map(lastDigit).take(n).sliding(2).map(sl => sl.last -> (sl.last - sl.head)).toSeq

  def groupedPrizes(secret: Long, n: Int): Map[Seq[Int], Int] =
    prizes(secret, n).sliding(4).toSeq.map(sl => sl.map(_._2) -> sl.last._1).reverse.toMap

  def compute(sequence: Seq[Int], buyers: Seq[Map[Seq[Int], Int]]): Int = buyers.flatMap(mm => mm.get(sequence)).sum

  println("started program")

  val buyers: Seq[Map[Seq[Int], Int]] = input.map(i => groupedPrizes(i, 2000))
  val uniqueSequences: Seq[Seq[Int]] = buyers.flatMap(_.keySet).distinct
  val uniqueSize: Int = uniqueSequences.size

  println(s"working with ${buyers.length} buyers")
  println(s"investigating ${uniqueSize} sequences")

  val highest = uniqueSequences.zipWithIndex.foldLeft(0) {(res: Int, seq: (Seq[Int], Int)) =>
    val (sequence, i) = seq

    if i % 1000 == 0 then println(s"currently at $i / ${uniqueSize} with highest=$res") else ()
    val computed = compute(sequence, buyers)
    if computed > res then computed else res
  }

  private val answer2 = highest
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")


