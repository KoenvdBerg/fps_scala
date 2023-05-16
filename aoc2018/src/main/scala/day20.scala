import scala.io.*
import aoc2018.Combinator.*
import aoc2018.Combinator.P.*

object day20 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: String =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList
      .head


  enum Direction:
    case O(p: String)
    case C(lead: String, c: List[Direction])


  object Direction:
    val option: Parser[O] = regex("""[A-Z]""".r).many1.slice.map(O)
    val empty: Parser[O] = string("").map(a => O(""))
    val choice: Parser[C] = for {
      l <- option
      _ <- char('(')
      v <- sequence(char('|'), choice | option | empty)
      _ <- char(')')
    } yield C(l.p, v)
    val direction: Parser[List[Direction]] = for {
      _ <- char('^')
      v <- (choice | option).many1
      _ <- char('$')
    } yield v

//    def walk(rgx: List[Direction], acc: Int = 0): Int = rgx match
//      case h :: t => h match
//        case Path(p) => walk(t, acc + p.length)
//        case Choice(c) =>
//          val l: List[Int] = c.map {
//            case Path(s) => s.length
//            case Choice(c) =>
//          }
//          if l.contains(0) then walk(t, acc + l.max) else walk(t, acc + l.min)
//      case Nil => acc

    val f = Direction.direction.run(input)

  println(Direction.f)
  private val answer1 = None // Direction.walk(f)
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  private val answer2 = "NONE"
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")


//
//def takePars(in: String): String =
//  val pars = in.zipWithIndex.filter(x => x._1 == ')' | x._1 == '(')
//  val lpar = pars.zipWithIndex.filter(_._1._1 == ')').head._2
//  val (s, e) = (pars(lpar - 1)._2, pars(lpar)._2)
//  in.slice(s, e + 1)
//
//def longestRoute(r: String): String =
//  def go(i: String): String =
//    if i.count(_ == '(') <= 0 then i
//    else
//      val cs: String = takePars(i)
//      val choiceSpace: String = cs.slice(1, cs.length - 1)
//      val choices: Vector[String] = if choiceSpace.endsWith("|") then "" +: choiceSpace.split('|').toVector
//      else choiceSpace.split('|').toVector
//      val sel: String =
//        if choices.contains("") then "" // skippable
//        else choices.maxBy(_.length)
//      go(i.replace(cs, sel))
//
//  go(r)

// NOTE THAT 1 example doens't work 30 and not 18
