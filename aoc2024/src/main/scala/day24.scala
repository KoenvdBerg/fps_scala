import jdk.jshell.execution.Util

import scala.io.*
import math.*
import scala.annotation.tailrec
import scala.util.matching.Regex
import aoc2024.NumberTheory.{binaryToLong, toBinary}
import aoc2024.SequenceUtils.identityMap
import aoc2024.Optim.Memoize

object day00 extends App:

  private val day: String = "24"

  private val start1: Long =
    System.currentTimeMillis

  case class Op(n1: String, n2: String, z: String, op: String):

    def swap(newz: String): Op = copy(z = newz)

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
  val z = toBinary(x + y) // NOTE: this is the expected value
  val wiresToBacktrack = z.reverse.zipWithIndex.map((c, i) => s"z%02d".format(i) -> s"$c".toInt)

  private val ops = input._2
  def createGraphInput(wire: String): Seq[(String, String)] =
    val nextOps = ops
      .filter(_.z == wire)
      .flatMap(op => Seq(op.n1 -> wire, op.n2 -> wire))
    val complete = nextOps.filter((w, _) => w.startsWith("x") || w.startsWith("y"))
    val next = nextOps.filterNot((w, _) => w.startsWith("x") || w.startsWith("y"))
    complete ++ next ++ next.flatMap((nextWire, _) => createGraphInput(nextWire))

  val res2 = wiresToBacktrack.flatMap((w, _) => createGraphInput(w)).distinct
  res2.foreach((n1, n2) =>
    println(s"${n1} -> ${n2};")
  )
  def solutionSwapped(known: Map[String, Int], ops: Seq[Op], z1: String, z2: String) =
    val i1 = ops.indexWhere(_.z == z1)
    val op1 = ops(i1)
    val i2 = ops.indexWhere(_.z == z2)
    val op2 = ops(i2)
    val res = algorithm(known, ops.patch(i1, Seq(op1.swap(z2)), 1).patch(i2, Seq(op2.swap(z1)), 1))
    binary(res, "z")

  // https://dreampuf.github.io/GraphvizOnline/?engine=dot
  println(binary1)
  println(z)
  println(solutionSwapped(input._1, ops, "jnt", "vkd"))

  println(ops
    .filter(_.z.startsWith("z"))
    .filter(_.op != "XOR")
  )
  // z11, z06, z35, fhc, ggt, hqk, mwh, qhj

  private val answer2 = "z11, z06, z35, fhc, ggt, hqk, mwh, qhj".split(",").toSeq.map(_.trim).sorted.mkString(",")
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")

//val memf: ((String, Int, Int)) => Boolean = Memoize[(String, Int, Int), Boolean](backtrackWire).getMemoizedf
//val known = input._1
//val found = scala.collection.mutable.ListBuffer.empty[(String, Int, Int)]
//def backtrackWire(wire: String, wireValue: Int, depth: Int): Boolean =
//  if wire.startsWith("x") || wire.startsWith("y") then known(wire) == wireValue
//  else
//    val relevantOp: Seq[Seq[(String, Int)]] = ops.filter(_.z == wire)
//      .flatMap { op =>
//        (op.op, wireValue) match
//          case ("AND", 1) => Seq(
//            Seq(op.n1 -> 1, op.n2 -> 1)
//          )
//          case ("AND", 0) => Seq(
//            Seq(op.n1 -> 1, op.n2 -> 0),
//            Seq(op.n1 -> 0, op.n2 -> 1),
//            Seq(op.n1 -> 0, op.n2 -> 0)
//          )
//          case ("OR", 1) => Seq(
//            Seq(op.n1 -> 1, op.n2 -> 0),
//            Seq(op.n1 -> 0, op.n2 -> 1),
//            Seq(op.n1 -> 1, op.n2 -> 1)
//          )
//          case ("OR", 0) => Seq(
//            Seq(op.n1 -> 0, op.n2 -> 0)
//          )
//          case ("XOR", 1) => Seq(
//            Seq(op.n1 -> 0, op.n2 -> 1),
//            Seq(op.n1 -> 1, op.n2 -> 0)
//          )
//          case ("XOR", 0) => Seq(
//            Seq(op.n1 -> 0, op.n2 -> 0),
//            Seq(op.n1 -> 1, op.n2 -> 1)
//          )
//      }
//    val okay: Boolean = relevantOp.exists(r => r.forall((nw, wv) => memf(nw, wv, depth + 1)))
//    if !okay then
//      val tryThis = memf(wire, if wireValue == 1 then 0 else 1, depth + 1)
//      if tryThis then
//        found.append((wire, wireValue, depth))
//      true
//    //          tryThis
//    //        else tryThis
//    else okay
