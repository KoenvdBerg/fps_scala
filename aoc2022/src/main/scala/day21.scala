import scala.io.*
import math.*
import scala.annotation.tailrec
import day21.Op.*

import scala.util.{Failure, Try, Success}


/**
 * PART 01:
 * 
 * The main idea for this puzzle is that it basically forms an equation in the form of a tree. I solved this by creating 
 * the ADT called Equation. Then I loaded the input into this ADT and wrote a solve function that solves the equation. 
 *
 * PART 02:
 * 
 * Fill the tree with both sides of the equation. One side should be solvable, then the other side can be reverse
 * engineered using basic math substitution. This has been implemented in solveForX. 
 * 
 * In the example case the equation to solve is. This one is not representative for the final input, because it only
 * has the left-side of the operator (i.e. +-*div) unsolved:
 * 
 * (((x-3) * 2) + 4) / 4 = 150
 * (x-3) * 2) + 4 = 600 
 * (x-3) * 2 = 596
 * x-3 = 298
 * x = 301
 * 
 * The tricky part is for when the x is on the other side of the equation, then division and minus change like this: 
 * 
 * 10 / x = 2
 * x = 10 / 2 = 5
 * 
 * 10 - x = 5
 * x = -(5 - 10) = 5
 * 
 * This has been incorporated in the code below. 
 * 
 */


object day21 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis


  enum Op:
    case Plus, Minus, Mul, Div

    def toFunc: (Long, Long) => Long = this match
      case Plus => (a: Long, b: Long) => a + b
      case Minus => (a: Long, b: Long) => a - b
      case Mul => (a: Long, b: Long) => a * b
      case Div => (a: Long, b: Long) => a / b

    def invertFunc: (Long, Long) => Long = this match
      case Plus => (a: Long, b: Long) => a - b
      case Minus => (a: Long, b: Long) => a + b
      case Mul => (a: Long, b: Long) => a / b
      case Div => (a: Long, b: Long) => a * b


  object Op: 
    def toOp(s: String): Op = s match
      case "+" => Plus
      case "-" => Minus
      case "/" => Div
      case "*" => Mul

  private val input: Map[String, String]=

    def parseMap(s: String): (String, String) = s match
      case s"humn: $rest"  => "humn" -> ("FLAG" + " " + rest)  // add flag for part02
      case s"$name: $rest" => name -> rest
      case _ => sys.error(s"Cannot parse monkey: $s")

    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector
      .map(parseMap)
      .toMap

  enum Equation[+A]:
    case Value(a: A)
    case Operation(l: Equation[A], op: Op, r: Equation[A])
    case X

  object Equation:

    import Op.*

    def fromInput(in: Map[String, String], key: String)(part: Int): Equation[Long] = in(key) match
      case s"$x $op $y"     => Operation(fromInput(in, x)(part), toOp(op), fromInput(in, y)(part))
      case s"FLAG $number" if part == 1 => Value(number.toLong)  // set humn to its value
      case s"FLAG $_"      if part == 2 => X                     // set humn to X
      case s"$number"                   => Value(number.toLong)
      case _                            => sys.error(s"cannot parse to Expr: $key, $part")

    def solve(in: Equation[Long]): Long = in match
      case Operation(l, op, r) => op.toFunc(solve(l), solve(r))
      case Value(v) => v
      case X => sys.error("cannot interpret this tree")

    @tailrec
    def solveForX(in: Equation[Long], inVal: Long): Long = in match
      case Operation(l, op, r) =>
        Try(solve(r)) match
          // first case when right is known e.g. X - 100 = 130  --> 130 + 100 = 230.
          // use a simple function inversion here to solve for X
          case Success(value) => solveForX(l, op.invertFunc(inVal, value))
          case Failure(_)     =>
            // this case left is unknown, this changes the order for minus and division. 
            val nextVal: Long = op match
              case Minus => -(inVal - solve(l))   // e.g. 10 - X = 8 --> X = -(8 - 10) = 2
              case Div   => inVal / solve(l)      // e.g. 6 / X = 2  --> X = 6 / 2 = 3
              case _     => op.invertFunc(inVal, solve(l))  // for multiplication and plus this order doesn't matter
            solveForX(r, nextVal)
      case Value(_) => inVal
      case X  => inVal

  private val in1: Equation[Long] = Equation.fromInput(input, "root")(1)
  private val answer1 = Equation.solve(in1)
  println(s"Answer day $day part 2: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  private val left: Equation[Long] = Equation.fromInput(input, input("root").split(" ")(0))(2)
  private val right: Equation[Long] = Equation.fromInput(input, input("root").split(" ")(2))(2)
  private val answer2: Long = Try(Equation.solve(right)) match
    case Success(toEqual) => Equation.solveForX(left, toEqual)  // solve for left
    case Failure(_)       => Equation.solveForX(right, Equation.solve(left))  // solve for right
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")

