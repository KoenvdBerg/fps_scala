import jdk.jshell.execution.Util

import scala.io.*
import math.*
import scala.annotation.tailrec
import scala.util.matching.Regex
import aoc2024.NumberTheory.{binaryToLong, toBinary}

object day00 extends App:

  private val day: String = "24"

  private val start1: Long =
    System.currentTimeMillis

  case class Op(n1: String, n2: String, z: String, op: String)

  def parseVal(s: String): (String, Int) = s match
    case s"$x: $i" => x -> i.toInt
    case _ => sys.error(s"cannot parse $s")

  def parseOp(s: String): Op = s match
    case s"$x $op $y -> $z" => Op(x, y, z, op)
    case _ => sys.error(s"cannot parse op from $s")

  private val input: (Map[String, Int], Seq[Op]) =
    val in = Source
      .fromResource(s"day$day.txt")
      .getLines
      .toSeq

    in.takeWhile(_.nonEmpty).map(parseVal).toMap -> in.dropWhile(_.nonEmpty).drop(1).map(parseOp)

  def and(i1: Int, i2: Int) = if i1 == i2 && i1 == 1 then 1 else 0
  def or(i1: Int, i2: Int) = if i1 == i2 && i1 == 0 then 0 else 1
  def xor(i1: Int, i2: Int) = if i1 != i2 then 1 else 0

  def algorithm(start: Map[String, Int], ops: Seq[Op]): Map[String, Int] =

    val localOps = scala.collection.mutable.ListBuffer.from(ops)

    @tailrec
    def go(known: Map[String, Int]): Map[String, Int] =
      if localOps.isEmpty then known
      else
        val i = localOps.indexWhere(op => known.contains(op.n1) && known.contains(op.n2))
        val op = localOps(i)
        val i1 = known(op.n1)
        val i2 = known(op.n2)
        val z =
          if op.op == "AND" then and(i1, i2)
          else if op.op == "OR" then or(i1, i2)
          else if op.op == "XOR" then xor(i1, i2)
          else sys.error(s"don't know ${op.op}")
        val nextKnown = known + (op.z -> z)
        localOps.remove(i)
        go(nextKnown)

    go(start)

  def binary(known: Map[String, Int], wire: String): String =
    known.toSeq
      .filter(_._1.startsWith(wire))
      .sortBy(-_._1.drop(1).toInt)
      .map(_._2).mkString

  val res1 = algorithm(input._1, input._2)
  val binary1 = binary(res1, "z")

  private val answer1 = binaryToLong(binary1)
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  def binary2(known: Map[String, Int], wire: String): String =
    known.toSeq
      .filter(_._1.startsWith(wire))
      .sortBy(_._1.drop(1).toInt)
      .map(_._2).mkString

  val x = binaryToLong(binary2(input._1, "x"))
  val y = binaryToLong(binary2(input._1, "y"))
  val z = toBinary(x + y)  // NOTE: this is the expected value
  val wiresToBacktrack = z.reverse.zipWithIndex.map((c, i) => s"z%02d".format(i) -> s"$c".toInt)


  println(wiresToBacktrack)
  println(z)
  println(binary1)

  // todo: backtrack from the known z (what it should be) using reverted logic:
  //  AND if z == 1 then both should be 1
  //  OR if z == 0 then both should be 0
  //  XOR if z == 1 then different else same values


  private val answer2 = ""
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")


