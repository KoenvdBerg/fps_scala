import jdk.jshell.execution.Util

import scala.io.*
import math.*
import scala.annotation.tailrec
import scala.util.matching.Regex

object day05 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val rules: Seq[(Int, Int)] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toSeq
      .flatMap(parseRule)

  private val input: Seq[Seq[Int]] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toSeq
      .map(parseSeq)
      .filter(_.nonEmpty)

  def parseRule(s: String): Option[(Int, Int)] =s match
    case s"$i|$j" => Some(i.toInt -> j.toInt)
    case _ => None

  def parseSeq(s: String): Seq[Int] =
    if parseRule(s).isEmpty && s.nonEmpty then s.split(",").toSeq.map(_.toInt)
    else Seq.empty

  def getBefore(i: Int): Seq[Int] = rules.filter(_._2 == i).map(_._1).filterNot(_ == i).sorted

  def setInOrder(si: Seq[Int]): Seq[Int] =
    val order = si.foldLeft(Map.empty[Int, Int]) { (res: Map[Int, Int], i: Int) =>
      val before = getBefore(i)
      val bl = si.filter(before.contains)
      res.updated(i, bl.length)
    }
    si.sortBy(order)

  private val answer1 = input.filter(si => setInOrder(si) == si)
    .map(seq => seq(seq.length/2)).sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  private val remainder = input.filter(si => setInOrder(si) != si)

  private val answer2 = remainder
    .map(setInOrder)
    .map(si => si(si.length/2)).sum
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")

