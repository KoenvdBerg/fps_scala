import scala.annotation.tailrec
import scala.io.*

object day08 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: Vector[Puzzle] =
    
    def parse(s: String): Puzzle = s match
      case s"$x | $y" => Puzzle(x.split(" ").toVector, y.split(" ").toVector)
      case _          => sys.error(s"Cannot parse: $s")
    
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector
      .map(parse)
    
  case class Puzzle(unknown: Vector[String], answer: Vector[String])
  
  val digits: Map[String, Vector[String] => String] = Map(
    "4" -> ((v: Vector[String]) => v.find(_.length == 4).get),
    "1" -> ((v: Vector[String]) => v.find(_.length == 2).get),
    "7" -> ((v: Vector[String]) => v.find(_.length == 3).get),
    "8" -> ((v: Vector[String]) => v.find(_.length == 7).get),
    "9" -> ((v: Vector[String]) => v.find(s => s.length == 6 && s.toSet.intersect(digits("4")(v).toSet).size == 4).get),
    "6" -> ((v: Vector[String]) => v.find(s => s.length == 6 && s.toSet.intersect(digits("1")(v).toSet).size == 1 && s != digits("9")(v)).get),
    "0" -> ((v: Vector[String]) => v.find(s => s.length == 6 && s != digits("9")(v) && s != digits("6")(v)).get),
    "5" -> ((v: Vector[String]) => v.find(s => s.length == 5 && s.toSet.intersect(digits("6")(v).toSet).size == 5).get),
    "3" -> ((v: Vector[String]) => v.find(s => s.length == 5 && s.toSet.intersect(digits("5")(v).toSet).size == 4).get),
    "2" -> ((v: Vector[String]) => v.find(s => s.length == 5 && s != digits("3")(v) && s != digits("5")(v)).get))
  
  def deduce(p: Puzzle): Int =
    val all: Vector[String] = p.unknown ++ p.answer
    val key: Map[String, String] = digits
      .map(f => (f._2(all).sorted, f._1))
    val finalNumber: String = p.answer.map((f: String) => key(f.sorted)).mkString("")
    finalNumber.toInt
  
  private val answer1: Int = input.map(p => p.answer.count((s: String) => s.length == 2 || s.length == 4 || s.length == 7 || s.length == 3)).sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  private val res2: Vector[Int] = input.map(p => deduce(p))
  private val answer2 = res2.sum
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")