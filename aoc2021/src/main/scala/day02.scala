import scala.annotation.tailrec
import scala.io.*

object day02 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: List[String] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList

  
  def move(pos: (Int, Int), in: String): (Int, Int) = in match
    case s"forward $i" => (pos._1 + i.toInt, pos._2)
    case s"up $i"      => (pos._1, pos._2 - i.toInt)
    case s"down $i"    => (pos._1, pos._2 + i.toInt)
    case _             => sys.error(s"cannot parse $in")

  def moveWithAim(pos: (Int, Int, Int), in: String): (Int, Int, Int) = in match
    case s"forward $i" => (pos._1 + i.toInt, pos._2 + pos._3 * i.toInt, pos._3)
    case s"up $i"      => (pos._1, pos._2, pos._3 - i.toInt)
    case s"down $i"    => (pos._1, pos._2, pos._3 + i.toInt)
    case _             => sys.error(s"cannot parse $in")
  
  private val res1: (Int, Int) = input.foldLeft((0,0))(move)
  private val answer1: Int = res1._1 * res1._2
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  private val res2: (Int, Int, Int) = input.foldLeft((0, 0, 0))(moveWithAim)
  private val answer2: Int = res2._1 * res2._2
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")