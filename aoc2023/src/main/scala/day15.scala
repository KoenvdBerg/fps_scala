import scala.io.*
import math.*
import scala.annotation.tailrec

object day15 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: Vector[String] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector
      .flatMap(_.split(","))

  def hash(s: String): Int =

    val l: Int = s.length

    @tailrec
    def go(i: Int, score: Int): Int =
      if i >= l then score
      else
        val cur: Int = s(i).toInt
        val next: Int = (score + cur) * 17 % 256
        go(i+1, next)

    go(0, 0)

  private val res1: Vector[Int] = input.map(hash)
  private val answer1: Int = res1.sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  case class Lens(original: String, label: String, op: Char, focalLength: Int)

  object Lens:
    def parse(s: String): Lens = s match
      case s"$x=$y" => Lens(s, x, '=', y.toInt)
      case s"$x-" => Lens(s, x, '-', -1)
      case _ => sys.error(s"cannot parse $s")

    def hashMap(stack: Map[Int, Vector[Lens]], b: Lens): Map[Int, Vector[Lens]] =
      val boxNo: Int = hash(b.label)

      b.op match
        case '-' =>
          val toUpdate: Option[Map[Int, Vector[Lens]]] = stack.get(boxNo)
            .map(vb => stack.updated(boxNo, vb.filter(_.label != b.label)))
          toUpdate.getOrElse(stack)

        case '=' =>
          val toUpdate: Option[Vector[Lens]] = stack.get(boxNo).map(vb =>
            val i: Int = vb.indexWhere(_.label == b.label)
            if i != -1 then vb.updated(i, b)
            else vb.appended(b)
          )
          stack.updated(boxNo, toUpdate.getOrElse(Vector(b)))
    def getFocusPower(lenses: Map[Int, Vector[Lens]]): Long =
      lenses.map((i, vb) => vb.zipWithIndex.map((b, i2) => (i + 1L) * (i2 + 1) * b.focalLength).sum).sum

  private val res2: Map[Int, Vector[Lens]] = input.map(Lens.parse).foldLeft(Map.empty[Int, Vector[Lens]])(Lens.hashMap)
  private val answer2: Long = Lens.getFocusPower(res2)
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
