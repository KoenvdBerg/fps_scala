import scala.io.*
import math.*
import scala.annotation.tailrec
import day21.Op.*

import scala.util.{Failure, Try, Success}


/**
 * PART 01:
 *
 * PART 02:
 * IDEA:
 *
 * instead of using strings, use an ADT that can be interpreted for getting to the right and left
 * hand side of the = sign. Then determine on which one the human has influence. Find the 'fixed' result on which no
 * influence was possible, and then inverse the interpretation of the program that leads to the human through the
 * interpreter.
 *
 * The ADT should look like a tree eg see below
 *
 * Fill the tree with both sides of the equation. One side should be solvable, then the other side can be reverse
 * engineered using basic math substitution.
 *
 * TODO: use map for monkeys Map[name, Operation]
 *
 *
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
    case Unknown

  object Equation:

    import Op.*

    def fromInput(in: Map[String, String], key: String)(part: Int): Equation[Long] = in(key) match
      case s"$unkown1 $op $unkown2"     => Operation(fromInput(in, unkown1)(part), toOp(op), fromInput(in, unkown2)(part))
      case s"FLAG $number" if part == 1 => Value(number.toLong)
      case s"FLAG $_"      if part == 2 => Unknown
      case s"$number"                   => Value(number.toLong)
      case _                            => sys.error(s"cannot parse to Expr: $key, $part")

    def interpreter(in: Equation[Long]): Long = in match
      case Operation(l, op, r) => op.toFunc(interpreter(l), interpreter(r))
      case Value(v) => v
      case Unknown => sys.error("cannot interpret this tree")

    def invertEquation(in: Equation[Long], inVal: Long): Long = in match
      case Operation(l, op, r) =>
        Try(interpreter(r)) match
          case Success(value) =>
            println(s"$inVal $op $value --> ${op.invertFunc(inVal, value)}")
            invertEquation(l, op.invertFunc(inVal, value))
          case Failure(_)     =>
            println(s"$inVal $op ${interpreter(l)} --> ${op.invertFunc(inVal, interpreter(l))}")
            invertEquation(r, op.invertFunc(inVal, interpreter(l)))
      case Value(_) => inVal
      case Unknown  => inVal

    def solver(left: Equation[Long], right: Equation[Long]): Long =
      Try(interpreter(left)) match
        case Success(value) =>
          println("solving rigth")
          invertEquation(right, value)
        case Failure(_)     =>
          println("solving left")
          invertEquation(left, interpreter(right))

  private val in1: Equation[Long] = Equation.fromInput(input, "root")(1)
  private val answer1 = Equation.interpreter(in1)
  println(s"Answer day $day part 2: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  // not it: 8764479278265

  private val left: Equation[Long] = Equation.fromInput(input, input("root").split(" ")(0))(2)
  private val right: Equation[Long] = Equation.fromInput(input, input("root").split(" ")(2))(2)
  private val answer2 = Equation.solver(left, right)
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")







//case class Monkey(name: String, operation: Either[String, String]):
//
//  def interpret: Monkey =
//    val newOp: Try[Either[String, String]] = operation match
//      case Right(x) => Success(Right(x))
//      case Left(unknown) => unknown match
//        case s"$n1+$n2" => Try(Right(s"${n1.toLong + n2.toLong}"))
//        case s"$n1*$n2" => Try(Right(s"${n1.toLong * n2.toLong}"))
//        case s"$n1/$n2" => Try(Right(s"${n1.toLong / n2.toLong}"))
//        case s"$n1-$n2" => Try(Right(s"${n1.toLong - n2.toLong}"))
//        case _ => sys.error(s"cannot interpret monkey: $unknown")
//    newOp match
//      case Failure(_) => this
//      case Success(v) => this.copy(operation = v)
//
//
//private val input: Vector[Monkey] =
//
//  def parse(s: String): Monkey = s match
//    case s"$name: $unknown1 $op $unknown2" => Monkey(name, Left(unknown1 + op + unknown2))
//    case s"$name: $number" => Monkey(name, Right(number))
//    case _ => sys.error(s"Cannot parse monkey: $s")
//
//  Source
//    .fromResource(s"day$day.txt")
//    .getLines
//    .toVector
//    .map(parse)
//
//def updateMonkeys(monkeys: Vector[Monkey], m: Monkey): Vector[Monkey] = m.operation match
//  case Left(_) => monkeys
//  case Right(known) => monkeys.filterNot(_ == m).foldLeft(Vector.empty)((newms: Vector[Monkey], mm: Monkey) => mm.operation match
//    case Right(_) => mm +: newms
//    case Left(unkown) => mm.copy(operation = Left(unkown.replace(m.name, known))) +: newms
//  )
//
//@tailrec
//def solver(monkeys: Vector[Monkey]): Monkey =
//  if monkeys.length <= 1 then monkeys.head
//  else
//    val updated: Vector[Monkey] = monkeys.foldLeft(monkeys)(updateMonkeys)
//    val next: Vector[Monkey] = updated.map(_.interpret)
//    solver(next)
