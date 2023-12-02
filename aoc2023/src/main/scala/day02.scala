import scala.io.*
import math.*
import scala.annotation.tailrec

object day02 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  case class Cube(color: String, n: Long)
  case class Game(id: Int, cubes: Vector[Cube])

  private val inputGames: Vector[Game] =

    def parse(s: String): Game = s match
      case s"Game $id: $tosplit" =>
        val cubeSets: Seq[String] = tosplit.split(";")
          .flatMap(_.strip().split(",").map(_.strip()))
        val cubes: Vector[Cube] = cubeSets
          .map { (c: String) =>
            val cube1 = c.split(" ")
            Cube(cube1.last, cube1.head.toLong)
          }.toVector
        Game(id.toInt, cubes)
      case _ => sys.error("SLKDJFKLSDJf")

    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector
      .map(parse)

  private def validateGame(game: Game): Int =
    val validated: Boolean = game.cubes.forall {
      case Cube("green", n) => n <= 13
      case Cube("red", n) => n <= 12
      case Cube("blue", n) => n <= 14
    }
    if validated then game.id else 0

  private def minCube(game: Game): Long =
    game.cubes
      .groupBy(_.color).values
      .map(_.maxBy(_.n).n)
      .product

  private val answer1 = inputGames.map(validateGame).sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  private val answer2 = inputGames.map(minCube).sum
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
