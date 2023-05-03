import scala.io.*
import math.*
import scala.collection.mutable
import scala.collection.mutable.Stack


/**
 * PART 01:
 *
 * The challenging bit in part 1 is understanding how the registers work, if you haven't worked with registers before.
 * After seeing how they work, you can implement all the mentioned mini-functions (as implemented below) that modify
 * a register (here an Array) in their specific way.
 *
 * Then, make a list of all the operations and parse in the input. Check for every sample which operation leads to the
 * desired output (in the After bit). Keep track of the operators while doing so.
 *
 * In my code this boils down to the validate() function, that goes over every sample and validates for which operations
 * that sample holds. This gives a Vector of operation codes with a vector of length 16 containing true and false,
 * depending for which function the sample was correct.
 *
 * Then to get the answer, count all instances where the amount of true (in the vector of length 16) >= 3.
 *
 * PART 02:
 *
 * The challenging bit here is that you have to first make sure to find which operation code is linked to what function.
 * This can be done manually, however I opted to do it automatically. The way this is done is to first remove all the
 * duplicates from the result of part 01 by making it a set. This set looks something like this table:
 *
 * +----------------+-------+------+-------+-------+-----+
 * | code/operation | addr  | addi | mulr  | muli  | ... |
 * +----------------+-------+------+-------+-------+-----+
 * | 1              | true  | true | false | false |     |
 * +----------------+-------+------+-------+-------+-----+
 * | 2              | false | true | false | false |     |
 * +----------------+-------+------+-------+-------+-----+
 * | 3              | true  | true | true  | false |     |
 * +----------------+-------+------+-------+-------+-----+
 * | N              |       |      |       |       | ... |
 * +----------------+-------+------+-------+-------+-----+
 *
 * (Note that instead of the operation name, the code below uses the operation index of the vector that holds all operations)
 *
 * The first known can be derived from finding a code for which only 1 operation is true, in this case addi. Then,
 * set all the instances of addi to false (essentially removing it from the table, but keeping the it present to obtain
 * the correct index). Then continue the process and find that now addr can be identified. Lastly, mulr is identified.
 * This yields: Map(2 -> 1, 1 -> 0, 3 -> 2) (1st integer is the code, 2nd is the index of the operation).
 *
 * Using the identified operation codes Map, the program can be run (after parsing). This is done in runProgram().
 * Use a register Array(0,0,0,0) as first input to get the result.
 *
 *
 */


object day16 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  case class Operation(op: Int, a: Int, b: Int, c: Int):
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
    val allOps: Vector[Array[Int] => Array[Int]] =
      Vector(addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr)

  case class Sample(before: Vector[Int], op: Operation, after: Vector[Int]):

    def validateOps: (Int, Vector[Boolean]) = (op.op, op.allOps.map(f => f(before.toArray).toVector == after))

  private val input: List[Sample] =

    def parser(s: String): Sample =

      def parseInt(s: String, spl: String = ","): Vector[Int] = s.split(s"$spl").map(_.strip().toInt).toVector

      def parseOp(s: String): Operation =
        val in: Vector[Int] = parseInt(s, " ")
        Operation(in(0), in(1), in(2), in(3))

      s match
        case s"Before: [$q]${w}After:  [$e]" => Sample(parseInt(q), parseOp(w), parseInt(e))
        case _ => sys.error("parsing incorrect!")

    Source
      .fromResource(s"day${day}_01.txt")
      .getLines
      .toList
      .filter(_ != "")         // filtering out the whitespace
      .grouped(3)              // making groups of 3 (put into Arrays)
      .map(_.mkString(""))     // making a single parsable string
      .map(s => parser(s))     // parsing the grouped string to Operation
      .toList


  def validate(ins: List[Sample], acc: Vector[(Int, Vector[Boolean])] = Vector.empty): Vector[(Int, Vector[Boolean])] = ins match
    case Nil => acc
    case h :: t =>
      val hit: (Int, Vector[Boolean]) = h.validateOps
      validate(t, hit +: acc)

  private val res1: Vector[(Int, Vector[Boolean])] = validate(input)
  private val answer1: Int = res1.map(t => t._2.count(_ == true)).count(_ >= 3)
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  private val input2: List[Operation] =

    def parser(s: String): Operation =
      val in: Vector[Int] = s.split(" ").map(_.toInt).toVector
      Operation(in(0), in(1), in(2), in(3))

    Source
      .fromResource(s"day${day}_02.txt")
      .getLines
      .toList
      .map(s => parser(s)) // parsing the grouped string to Operation


  /**
   * idea here is to find the first hit for which only 1 operation is true. That holds the first known
   * operator that we can assign to a function, eg 12 -> 2 means op code 12 to function at index 2. Then
   * remove op code from search space, and set all Booleans for the other operators to false at found index.
   * Keep on looking until the search space is empty.
   */
  def retrieveOptCodes(searchSpace: Set[(Int, Vector[Boolean])], acc: Map[Int, Int] = Map.empty): Map[Int, Int] =
    if searchSpace.isEmpty then acc  // exit condition, search space is empty
    else
      val hit: (Int, Vector[Boolean]) = searchSpace  // a hit is defined as a op that has only 1 true
        .filter(vals => vals._2.count(_ == true) == 1)  // find op that has 1 true value, so it is known
        .head
      val nextKnown: (Int, Int) = hit._1 -> hit._2.indexWhere(_ == true)
      val nextSearch: Set[(Int, Vector[Boolean])] = searchSpace
        .filter(f => f._1 != nextKnown._1)  // filter the found op from the search space
        .map(f => (f._1, f._2.patch(nextKnown._2, Vector(false), 1)))  // set boolean elem to false at found op index
      retrieveOptCodes(nextSearch, acc + nextKnown)

  private val opCodes: Map[Int, Int] = retrieveOptCodes(res1.toSet)

  def runProgram(prg: List[Operation], reg: Array[Int]): Array[Int] = prg match
    case h :: t =>
      val op: Array[Int] => Array[Int] = h.allOps(opCodes(h.op))  // select function based on retrieved opCodes
      runProgram(t, op(reg))
    case Nil    => reg

  private val answer2: Array[Int] = runProgram(input2, Array(0, 0, 0, 0))
  println(s"Answer day $day part 2: ${answer2.toVector}, of which register 0 = ${answer2(0)} [${System.currentTimeMillis - start2}ms]")

