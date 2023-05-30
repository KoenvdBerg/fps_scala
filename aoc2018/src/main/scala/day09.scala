import scala.io.*
import math.*
import scala.collection.mutable
import scala.collection.mutable.Stack

/**
 *
 * PART 1:
 *
 * This one was pretty easy. I implemented a rotate function for Lists, as well as takeHead and appendHead for taking
 * the head of a list or appending a value to the head of a list.
 *
 * The simulation function just followed the rules as presented in the problem description:
 *
 *  0. exit condition --> marble is lastMarble
 *  1. add new marble
 *  2. if new marble is divisible by 23 (mod 23) then
 *    a. rotate marble circle by -7, take marble and compute score as takenMarble + current marble.
 *  3. else
 *    a. rotate marble circly by +2 and append new marble to the head of the circle. computed score is 0
 *  4. update scoreboard with the player being nPlayers % marble with the computed score
 *  5. recurse
 *
 *
 *  Total computing time was around 12 seconds
 *
 *  PART 2:
 *
 *  Okay happy days were suddenly over because my implementation to part 1 was not performant. Hence the large amount
 *  of code in the graveyard at the bottom of this file. I tried working with streams (LazyList in Scala 3) but
 *  couldn't make it work. I could make a stream of marble locations, but not the actual marble circe because it
 *  required a state that I couldn't figure out (using LazyList.unfold).
 *
 *  So then I just replaced all the Ints with Longs, and the List with Vector. For the latter I updated the implemented
 *  functions rotate, takeHead and appendHead to work with Vectors. This made the code performant.
 *
 *  Lesson learned, use performant data structures when they're available.
 *
 */


object day09 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: (Int, Int) =

    val parser: String => (Int, Int) = {
      case s"${nPlayers} players; last marble is worth ${lastMarblePoints} points" => (nPlayers.toInt, lastMarblePoints.toInt)
      case _ => sys.error("boom")
    }

    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList
      .map(parser)
      .head

  object Vectorr {
    def rotateVector[A](n: Int, s: Vector[A]): Vector[A] =
      if s.isEmpty then s
      else
        val nbound = n % s.length // skipping the full rotation rounds
        if nbound < 0 then rotateVector(nbound + s.length, s)
        else s.drop(nbound) ++ s.take(nbound)
  }

  def simulateMarbleGame(lastMarble: Int, nPlayers: Int, scoreBoard: Map[Int, Long] = Map.empty,
                         circle: Vector[Long] = Vector(0), marble: Long = 1): Map[Int, Long] =
    import Vectorr.*
    if marble >= lastMarble then scoreBoard   // Exit condition
    else

      // in case n == 23, then the score gets computed. In all other cases, score is 0
      val (score, nextCircle): (Long, Vector[Long]) =
        if marble % 23 == 0 then
          rotateVector(-7, circle) match
            case h +: t => (h + marble, t)  // updating score, don't add marble to circle
            case Vector() => sys.error("cannot take head of empty vector")
        else
          rotateVector(2, circle) match
            case h +: t => (0L, marble +: h +: t)  // score is 0, adding marble to circle
            case Vector() => sys.error("marble circle cannot be empty")

      // the current player is based off the total N players mod n. Update the scoreboard for the current player.
      val thisPlayer: Int = (marble % nPlayers).toInt
      val updatedScoreBoard: Map[Int, Long] =
        if !scoreBoard.contains(thisPlayer) then
          scoreBoard.updated(thisPlayer, score)
        else
          scoreBoard.updated(thisPlayer, scoreBoard(thisPlayer) + score)

      // continue simulation with updated scoreboard and the next marble circle
      simulateMarbleGame(lastMarble, nPlayers, updatedScoreBoard, nextCircle, marble + 1)

  private val res1: Map[Int, Long] = simulateMarbleGame(input._2, input._1)
  private val answer1 = res1.maxBy(_._2)._2
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  private val res2: Map[Int, Long] = simulateMarbleGame(input._2*100, input._1)
  private val answer2 = res2.maxBy(_._2)._2
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
