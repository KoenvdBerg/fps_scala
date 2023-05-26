import scala.io.*
import math.*
import scala.collection.mutable
import scala.collection.mutable.Stack
import scala.util.{Failure, Success, Try}

/**
 *
 * PART 1
 *
 * The idea of this puzzle is to reuse the code from day 16 and update a few properties. In this new puzzle the code
 * can jump to specific instructions using an extra register that tracks the instruction index. What happens is that
 * the code at some point will jump to an instruction that doesn't exist and when that happens the code crashes.
 *
 * To find this location I've reused the code from day 16. I updated everything to work with immutable data structures
 * instead of Arrays, to keep things functional and it turned out to be more performant as well. The logic for going
 * to next and previous instructions (basically a GOTO jump) is added to the code and whenever a instruction is called
 * that is out of index, then the program crashes. The value left in register 0 when the program crashes is the result
 * for part 1.
 *
 *
 * PART 2:
 *
 * The difficulty of part 2 is that it's an extremely long running program, so you'll have to figure out what's
 * happening and code a solution that runs quicker. To do this, I ran several tests with the program and eventually
 * figured out that the program first computes a large number, and then sums all the divisors for that number to the
 * result. If that's finished, it crashes (I presumed). So the answer is specific to my input, but I think that
 * the general direction to get to this answer is identical across inputs.
 *
 */
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

  /**
   * The function below will run the input program until the program crashes. A crash is defined as a call to an
   * instruction pointer that doesn't exist (i.e. IndexError). The `ip` is the index of the register that holds
   * the instruction number to execute. The `program` holds all the instructions as obtained from the input file in order.
   */
  def runProgram(program: Vector[Operation], ip: Int, n: Int = 0)(maxIts: Int, reg: Vector[Int]): Vector[Int] =
    if n >= maxIts && maxIts != -1 then reg
    else
      // get instruction based on the instruction pointer (ip)
      val instruction: Try[Operation] = Try(program(reg(ip)))  // yields Failure if the ip index is not present in the program
      instruction match
        case Failure(_) => {println("HIT") ; reg}  // exit condition: instruction was not available thus program has crashed
        case Success(i) =>
          val next: Vector[Int] = i.run(reg)       // run the instruction and update the registers
          runProgram(program, ip, n + 1)(maxIts, incrementIp(next, ip))  // increment the instruction pointer

  val program: (Int, Vector[Int]) => Vector[Int] = runProgram(input._1, input._2)
  private val answer1: Vector[Int] = program(-1, Vector(0,0,0,0,0,0))
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

  val limit: Int = program(1000, Vector(1,0,0,0,0,0)).max

  val res2 = Range(1, limit + 1).toVector.filter((i: Int) => limit % i == 0)
  println(res2)

  private val answer2 = res2.sum
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")

