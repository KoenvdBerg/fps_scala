import scala.io.*
import math.*
import scala.collection.mutable
import scala.collection.mutable.Stack
import scala.util.{Failure, Success, Try}


object day19 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  case class Operation(op: String, a: Int, b: Int, c: Int):
    def addr(in: Vector[Int]): Vector[Int] = in.updated(c, in(a) + in(b))
    def addi(in: Vector[Int]): Vector[Int] = in.updated(c, in(a) + b)
    def mulr(in: Vector[Int]): Vector[Int] = in.updated(c, in(a) * in(b))
    def muli(in: Vector[Int]): Vector[Int] = in.updated(c, in(a) * b)
    def banr(in: Vector[Int]): Vector[Int] = in.updated(c, in(a) & in(b))
    def bani(in: Vector[Int]): Vector[Int] = in.updated(c, in(a) & b)
    def borr(in: Vector[Int]): Vector[Int] = in.updated(c, in(a) | in(b))
    def bori(in: Vector[Int]): Vector[Int] = in.updated(c, in(a) | b)
    def setr(in: Vector[Int]): Vector[Int] = in.updated(c, in(a))
    def seti(in: Vector[Int]): Vector[Int] = in.updated(c, a)
    def gtir(in: Vector[Int]): Vector[Int] = in.updated(c, if a > in(b) then 1  else 0)
    def gtri(in: Vector[Int]): Vector[Int] = in.updated(c, if in(a) > b then 1 else 0)
    def gtrr(in: Vector[Int]): Vector[Int] = in.updated(c, if in(a) > in(b) then 1 else 0)
    def eqir(in: Vector[Int]): Vector[Int] = in.updated(c, if a == in(b) then 1 else 0)
    def eqri(in: Vector[Int]): Vector[Int] = in.updated(c, if in(a) == b then 1 else 0)
    def eqrr(in: Vector[Int]): Vector[Int] = in.updated(c, if in(a) == in(b) then 1 else 0)

    val ops: Map[String, Vector[Int] => Vector[Int]] = Map(
      "addr" -> addr, "addi" -> addi, "mulr" -> mulr, "muli" -> muli, "banr" -> banr, "bani" -> bani,
      "borr" -> borr, "bori" -> bori, "setr" -> setr, "seti" -> seti, "gtir" -> gtir, "gtri" -> gtri,
      "gtrr" -> gtrr, "eqir" -> eqir, "eqri" -> eqri, "eqrr" -> eqrr)

    def run(input: Vector[Int]): Vector[Int] =
      val executable: Vector[Int] => Vector[Int] = ops.getOrElse(op, sys.error("BOOM!!!"))
      executable(input)



  private val input: (Vector[Operation], Int) =

    def parseOps(s: String): Option[Operation] =
      s match
        case s"$op $i0 $i1 $i2" => Some(Operation(op, i0.toInt, i1.toInt, i2.toInt))
        case _                  => None

    def parseIp(s: String): Option[Int] =
      s match
        case s"#ip $ip" => Some(ip.toInt)
        case _ => None

    val lines: Vector[String] = Source
      .fromResource(s"day${day}.txt")
      .getLines
      .toVector

    (lines.flatMap(parseOps), lines.flatMap(parseIp).head)

  def incrementIp(reg: Vector[Int], ip: Int): Vector[Int] = reg.updated(ip, reg(ip) + 1)

  def runProgram(prg: Vector[Operation], ip: Int, n: Int = 0)(maxIts: Int, reg: Vector[Int]): Vector[Int] =
    if n >= maxIts && maxIts != -1 then reg
    else
      val instruction: Try[Operation] = Try(prg(reg(ip)))  // yields Failure if the ip index is not present in the prg
      instruction match
        case Failure(_) => {println("HIT") ; reg}
        case Success(i) =>
          val next: Vector[Int] = i.run(reg)
          runProgram(prg, ip, n + 1)(maxIts, incrementIp(next, ip))

  val prg: (Int, Vector[Int]) => Vector[Int] = runProgram(input._1, input._2)
  private val answer1: Vector[Int] = prg(-1, Vector(0,0,0,0,0,0))
  println(s"Answer day $day part 1: ${answer1} should be: 1694 [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis


  /**
   * output of this:
   * prg(1000000000, Vector(1,0,0,0,0,0))
   *
   *   Vector(1, 0, 10550400, 0, 34, 10551364)
   *   Vector(0, 0, 10550400, 0, 34, 10551364)
   *   Vector(0, 10551364, 1, 1, 7, 10551364)
   *   Vector(1, 10551364, 1, 1, 7, 10551364)
   *   Vector(1, 5275682, 1, 2, 7, 10551364)
   *   Vector(3, 5275682, 1, 2, 7, 10551364)
   *   Vector(3, 2637841, 1, 4, 7, 10551364)
   *   Vector(7, 2637841, 1, 4, 7, 10551364)
   *
   *   It's summing whatever 10551364 % i == 0
   */

  val limit: Int = prg(1000, Vector(1,0,0,0,0,0)).max

  val res2 = Range(1, limit + 1).toVector.filter((i: Int) => limit % i == 0)
  println(res2)

  private val answer2 = res2.sum
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")

