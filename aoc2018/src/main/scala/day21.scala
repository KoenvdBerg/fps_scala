import scala.io.*
import math.*
import scala.collection.mutable
import scala.collection.mutable.Stack
import scala.util.{Failure, Success, Try}


object day21 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  case class Operation(op: String, a: Int, b: Int, c: Int):
    def addr(in: Array[Int]): Array[Int] = {in(c) = in(a) + in(b); in}
    def addi(in: Array[Int]): Array[Int] = {in(c) = in(a) + b; in}
    def mulr(in: Array[Int]): Array[Int] = {in(c) = in(a) * in(b); in}
    def muli(in: Array[Int]): Array[Int] = {in(c) = in(a) * b; in}
    def banr(in: Array[Int]): Array[Int] = {in(c) = in(a) & in(b); in}
    def bani(in: Array[Int]): Array[Int] = {in(c) = in(a) & b; in}
    def borr(in: Array[Int]): Array[Int] = {in(c) = in(a) | in(b); in}
    def bori(in: Array[Int]): Array[Int] = {in(c) = in(a) | b; in}
    def setr(in: Array[Int]): Array[Int] = {in(c) = in(a); in}
    def seti(in: Array[Int]): Array[Int] = {in(c) = a; in}
    def gtir(in: Array[Int]): Array[Int] = {in(c) = if a > in(b) then 1 else 0; in}
    def gtri(in: Array[Int]): Array[Int] = {in(c) = if in(a) > b then 1 else 0; in}
    def gtrr(in: Array[Int]): Array[Int] = {in(c) = if in(a) > in(b) then 1 else 0; in}
    def eqir(in: Array[Int]): Array[Int] = {in(c) = if a == in(b) then 1 else 0; in}
    def eqri(in: Array[Int]): Array[Int] = {in(c) = if in(a) == b then 1 else 0; in}
    def eqrr(in: Array[Int]): Array[Int] = {in(c) = if in(a) == in(b) then 1 else 0; in}

    val ops: Map[String, Array[Int] => Array[Int]] = Map(
      "addr" -> addr, "addi" -> addi, "mulr" -> mulr, "muli" -> muli, "banr" -> banr, "bani" -> bani,
      "borr" -> borr, "bori" -> bori, "setr" -> setr, "seti" -> seti, "gtir" -> gtir, "gtri" -> gtri,
      "gtrr" -> gtrr, "eqir" -> eqir, "eqri" -> eqri, "eqrr" -> eqrr)

    def run(input: Vector[Int]): Vector[Int] =
      val executable: Array[Int] => Array[Int] = ops(op)
      executable(input.toArray).toVector



  private val input: (Vector[Operation], Int) =

    def parseOps(s: String): Option[Operation] =
      if s.contains("#ip") then None
      else
        val in: Vector[String] = s.split(" ").toVector
        Some(Operation(in(0), in(1).toInt, in(2).toInt, in(3).toInt))

    def parseIp(s: String): Option[Int] =
      if s.contains("#ip") then
        Some(s.split(" ")(1).toInt)
      else None

    val lines: Vector[String] = Source
      .fromResource(s"day${day}.txt")
      .getLines
      .toVector

    (lines.flatMap(parseOps), lines.flatMap(parseIp).head)

  def incrementIp(reg: Vector[Int], ip: Int): Vector[Int] =
    val tmp: Array[Int] = reg.toArray
    tmp(ip) = tmp(ip) + 1
    tmp.toVector


  // TODO: reverse engineer the code making notes for the entire input program
  def runProgram(prg: Vector[Operation], ip: Int, n: Int = 0, lelz: Set[Int] = Set.empty)(maxIts: Int, reg: Vector[Int]): Vector[Int] =
    if n >= maxIts && maxIts != -1 then reg
    else if reg(ip) == 28 && lelz.contains(reg(3)) then { println("JAJAJAJA") ; reg }
    else
      println(reg)
//      if reg(ip) == 28 then println(reg(3)) else ()
      val nextlelz: Set[Int] = if reg(ip) == 28 then lelz + reg(3) else lelz
      val instruction: Try[Operation] = Try(prg(reg(ip)))
      instruction match
        case Failure(_) => {println("HIT") ; reg}
        case Success(i) =>
          val next: Vector[Int] = i.run(reg)
          //          if next.head != reg.head then { println(reg) ; println(next) }
          runProgram(prg, ip, n + 1, nextlelz)(maxIts, incrementIp(next, ip))


  def program(in: Int, n: Int): Int =
    if n >= 3 then in
    else
      println(in)
      val inn = if n % 2 == 1 then in + 1 else in
      val x = inn | 65536
      val y = x & 16777215
      val z = y * 65899
      val res = z & 16777215
      program(res, n+1)


  val prg: (Int, Vector[Int]) => Vector[Int] = runProgram(input._1, input._2)
  private val answer1: Vector[Int] = prg(2000, Vector(1,0,0,0,0,0))
//  program(1177631, 0)
  println(s"Answer day $day part 1: None [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  private val answer2 = None
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")

