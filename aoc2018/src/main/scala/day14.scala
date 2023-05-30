import scala.io.*
import math.*
import scala.collection.mutable
import scala.collection.mutable.Stack

/**
 *
 * PART 1:
 *
 * The solution to this puzzle was to model the recipe making game and then run the simulation.
 *
 * Big lesson here is to use Vectors and not lists to track the state of the recipes for a big bump in performance.
 *
 * PART 2:
 *
 * Reused the code from part 1 with slight modification so that it can look for a certain number in the recipes.
 *
 *
 */
object day14 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: Int =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector
      .map(_.toInt)
      .head

  def splitInteger(i: Int): Vector[Int] =
    i.toString.map(p => s"$p".toInt).toVector

  def simulateRecepiCreation(board: Vector[Int], maxN: Int, leapSize: Int, players: Map[Int, Int]): Vector[Int] =
    if board.length - leapSize == maxN then board.takeRight(leapSize)
    else
      val playerVals: Vector[Int] = players.values.map(board(_)).toVector
      val recipe: Int = playerVals.sum
      val toAdd: Vector[Int] = if recipe >= 10 then splitInteger(recipe) else Vector(recipe)
      val newBoard: Vector[Int] = board ++: toAdd
      val updatedPlayers: Map[Int, Int] = players.view
        .map(p => (p._1, (p._2 + playerVals(p._1) + 1) % newBoard.length)).toMap
      simulateRecepiCreation(newBoard, maxN, leapSize, updatedPlayers)


  private val res: Vector[Int] = simulateRecepiCreation(Vector(3, 7), input, 10, Map(0 -> 0, 1 -> 1))
  private val answer1: String = res.mkString("")
  println(s"Answer day $day part 1: ${answer1} should be: 7121102535 [${System.currentTimeMillis - start1}ms]")


  def simulateRecepiCreation2(board: Vector[Int], toFind: Vector[Int], leapSize: Int, players: Map[Int, Int], l: Int = 2): Int =
    if board.takeRight(leapSize) == toFind then l - toFind.length
    else
      val playerVals: Vector[Int] = players.map(p => board(p._2)).toVector
      val recipe: Int = playerVals.sum
      val toAdd: Vector[Int] = if recipe >= 10 then splitInteger(recipe) else Vector(recipe)
      val newBoard: Vector[Int] = board ++: toAdd
      val newl: Int = l + toAdd.length
      val updatedPlayers: Map[Int, Int] = players
        .map(p => (p._1, (p._2 + playerVals(p._1) + 1) % newl))
      simulateRecepiCreation2(newBoard, toFind, leapSize, updatedPlayers, newl)

  private val start2: Long =
    System.currentTimeMillis

  private val toFind = splitInteger(input).take(5)  // just looking for 5 digits as this is already a proper signature
  private val answer2 = simulateRecepiCreation2(Vector(3, 7), toFind, toFind.length, Map(0 -> 0, 1 -> 1))
  println(s"Answer day $day part 2: ${answer2} should be: 20236441 [${System.currentTimeMillis - start2}ms]")
