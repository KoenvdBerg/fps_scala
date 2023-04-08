import scala.io.*

object day03 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: List[String] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList

  case class Claim(x: Int, y: Int, id: Int)

  def computeClaim(cs: String): List[Claim] =
    val t = cs.split(" ")
    val id = t(0).substring(1).toInt
    val xstart = t(2).split(",")(0).toInt + 1
    val ystart = t(2).split(",")(1).replace(":", "").toInt + 1
    val xend = t(3).split("x")(0).toInt + xstart
    val yend = t(3).split("x")(1).toInt + ystart

    (for {
      x <- Range(xstart, xend)
      y <- Range(ystart, yend)
    } yield Claim(x, y, id)).toList

  val claims = input.flatMap(computeClaim(_))
  private val answer1: Int = claims.groupBy(x => (x.x, x.y)).map((_, y) => y.size).count(_ > 1)
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  def getAllIds(in: List[String]): List[Int] =
    in.map(_.split(" ")(0).substring(1).toInt)

  val idswithmultipleclaims = claims.groupBy(x => (x.x, x.y)).filter((_, y) => y.size > 1).map((_, y) => y).flatten.map(_.id).toSet
  val allids = getAllIds(input).toSet
  private val answer2 = allids.diff(idswithmultipleclaims)
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
