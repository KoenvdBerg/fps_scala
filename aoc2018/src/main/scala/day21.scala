import scala.io.*
import math.*
import scala.util.{Failure, Success, Try}
import scala.collection.immutable.Set

/**
 *
 * PART 1:
 *
 * The difficulty in this puzzle is the fact that you've to recognize that you've got to dig into the input file
 * code and decipher what's exactly going on. Doing that, I reached an instruction at index 28 that works with
 * register 0 and can cause the program to halt/crash with an out of bounds error (see also day21_notes.txt):
 *
 * Instruction: eqrr 3 0 1
 * Translaction to Scala: registers.updated(1, if in(3) == in(0) then 1 else 0)
 *
 * This instruction works by comparing the value at register 0 with the value in register 3. If they're equal,
 * then in the next instruction (=29) the program crashes. So Part 1 then consists of investigating when instruction
 * 28 is run for the first time, and then seeing which number is in register 3. Of course, other inputs may have
 * different instruction numbers and indices, but I expect that the general idea for the solution is similar.
 *
 * Part 2:
 *
 * To solve this, you've to track the values in register 3 that cause the program to crash, and find the first repeating
 * value (using a Set). The one value just before the crash, so not the last one but the penultimate one, is the answer
 * to part 2.
 *
 * The difficulty is that the program takes a really long time to reach this point: in my case 12508 possible crashes
 * before the program starts repeating itself. So I coded a general solution, which takes a lot of time to reach that
 * point(about 2 mins),  and I also refactored the input code manually to reach a much more reasonable running
 * time (about 8s).
 *
 * In the code below I made the crash-able instruction and the register that's involved in the crash
 * (in my case that was register 3) variable so that it might also work for other input files.
 *
 */


object day21 extends App:

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
    def gtir(in: Vector[Int]): Vector[Int] = in.updated(c, if a > in(b) then 1 else 0)
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
        case _ => None

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
  def runProgram(program: Vector[Operation], ip: Int, n: Int = 0)(maxIts: Int, reg: Vector[Int], crashInstruction: Int): Vector[Int] =
    if n >= maxIts && maxIts != -1 then reg
    else if reg(ip) == crashInstruction then reg
    else
      // get instruction based on the instruction pointer (ip)
      val instruction: Try[Operation] = Try(program(reg(ip)))  // yields Failure if the ip index is not present in the program
      instruction match
        case Failure(_) => {println("HIT") ; reg}  // exit condition: instruction was not available thus program has crashed
        case Success(i) =>
          val next: Vector[Int] = i.run(reg)  // run the instruction and update the registers
          runProgram(program, ip, n + 1)(maxIts, incrementIp(next, ip), crashInstruction)  // increment the instruction pointer


  /**
   * Below the following can be edited to tailor the solution to your own input:
   *
   * crashInstructionIndex: The index of the instruction that causes the crash.
   * crashRegister: The index of the register that's involved in the crash
   */
  val crashInstructionIndex: Int = 28
  val crashRegister: Int = 3

  // create the program:
  val prg: (Int, Vector[Int], Int) => Vector[Int] = runProgram(input._1, input._2)

  // run the program 2000 times and stop at the first time the crash instruction is encountered.
  private val answer1: Vector[Int] = prg(2000, Vector(1,0,0,0,0,0), crashInstructionIndex)

  // test if the value of the crash register really crashes the program by putting it in register 0 and running again.
  // If successful, this should print HIT to the output.
  private val _: Vector[Int] = prg(2000, Vector(answer1(crashRegister),0,0,0,0,0), -1)
  println(s"Answer day $day part 1: ${answer1(crashRegister)} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  /**
   * This function is the general solution to the problem of part2. Note: it takes a long time to compute the answer
   * (in my case) so take caution.
   */
  def loop(prg: (Int, Vector[Int], Int) => Vector[Int], in: Vector[Int], seen: Set[Int],
           crashInstruction: Int, crashRegister: Int): Int =
    val res: Vector[Int] = prg(-1, in, crashInstruction)                   // reach the next crash value
    if seen.size % 100 == 0 then println(s"loading: ${seen.size}") else () // print loading
    if seen.contains(res(crashRegister)) then in(crashRegister)            // exit condition
    else
      // add crash value to seen and continue
      loop(prg, res.updated(4, 29).updated(1, 0), seen + res(3), crashInstruction, crashRegister)


  /**
   * The function below is the refactoring of the main loop of the input code to Scala. The main reason for this
   * refactoring was performance and an exercise of refactoring skills. Please do not pay specific attention to the
   * implementation, as it's highly dependent on my own input code. So this is not reusable for other inputs.
   *
   * It essentially works by splitting the program into two loops, and recursively running those two loops
   * until a crashable value in register 3 is encountered twice. The value just before that one is the answer to
   * part 2.
   */
  def refactorMainLoop(in: Vector[Int], seen: Set[Int] = Set.empty[Int], n: Int = 0): Vector[Int] =
    if n >= 12510 then { println("NOT IT") ; in }
    else
      val x1: Int = in(2) & 255
      val x3: Int = (((in(3) + x1) & 16777215) * 65899) & 16777215
      if 256 > in(2) then
        println(s"$n : $x3")
        if seen.contains(x3) then { println(s"THE PRINT BEFORE THIS IS ANSWER: $x3") ; in }
        else
          val cont: Vector[Int] = Vector(in(0), 1, x3 | 65536, 1099159, in(4), in(5))
          refactorMainLoop(cont, seen + x3, n + 1)
      else
        val cont: Vector[Int] = refactorLoop2(Vector(in(0), 0, in(2), x3, in(4), in(5)), seen, n)
        refactorMainLoop(cont, seen, n)

  /**
   * This is the subloop to the main loop of the refactored version of the input code.
   */
  def refactorLoop2(in: Vector[Int], seen: Set[Int], n: Int): Vector[Int] =
    val x5: Int = (in(1) + 1) * 256
    if x5 > in(2) then
      val res: Vector[Int] = Vector(in(0), in(1), in(1), in(3), in(4), 1)
      res
    else
      val res: Vector[Int] = Vector(in(0), in(1) + 1, in(2), in(3), in(4), 0)
      refactorLoop2(res, seen, n)

  // below is the solution using the refactored code. This solution does not work on a different input.
  private val answer2 = refactorMainLoop(Vector(1,0,0 | 65536,1099159,0,0), Set.empty[Int])
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")

  // uncomment below to run the general slow solution:
  // private val answer2 = loop(prg, Vector(1,0,0,0,0,0), Set.empty[Int], crashInstructionIndex, crashRegister)
  // println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")

