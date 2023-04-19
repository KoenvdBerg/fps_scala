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
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList
      .map{
        case s"${nPlayers} players; last marble is worth ${lastMarblePoints} points" => (nPlayers.toInt, lastMarblePoints.toInt)
        case _ => sys.error("boom")
      }
      .head

  object Vectorr {
    def takeHeadVector[A](ls: Vector[A]): (A, Vector[A]) =
      if ls.isEmpty then sys.error("Empty Vector doesn't have head")
      else
        val tmp = ls.splitAt(1)
        (tmp._1.head, tmp._2)

    def appendHeadVector[A](as: Vector[A], r: A): Vector[A] =
      as.+:(r)

    def rotateVector[A](N: Int, s: Vector[A]): Vector[A] =
      if s.isEmpty then s
      else
        val nbound = N % s.length // skipping the full rotation rounds
        if nbound < 0 then rotateVector(nbound + s.length, s)
        else s.drop(nbound) ++ s.take(nbound)
  }

  def simulateMarbleGame(lastMarble: Int, nPlayers: Int, scoreBoard: Map[Int, Long] = Map(),
                         circle: Vector[Long] = Vector(0), n: Long = 1): Map[Int, Long] =
    import Vectorr.*
    if n >= lastMarble then scoreBoard   // Exit condition
    else

      // in case n == 23, then the score gets computed. In all other cases, score is 0
      val (score, nextCircle): (Long, Vector[Long]) = if n % 23 == 0 then
        val tmp: (Long, Vector[Long]) = takeHeadVector(rotateVector(-7, circle))
        (tmp._1 + n, tmp._2)
      else
        (0.toLong, appendHeadVector(rotateVector(2, circle), n))

      // the current player is based off the total N players mod n. Update the scoreboard for the current player.
      val thisPlayer: Int = (n % nPlayers).toInt
      val updatedScoreBoard = if !scoreBoard.contains(thisPlayer) then
        scoreBoard.updated(thisPlayer, score)
      else
        scoreBoard.updated(thisPlayer, scoreBoard(thisPlayer) + score)

      // continue simulation with updated scoreboard and the next marble circle
      simulateMarbleGame(lastMarble, nPlayers, updatedScoreBoard, nextCircle, n + 1)

  private val res1: Map[Int, Long] = simulateMarbleGame(input._2, input._1)
  private val answer1 = res1.maxBy(_._2)._2
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  private val res2: Map[Int, Long] = simulateMarbleGame(input._2*100, input._1)
  private val answer2 = res2.maxBy(_._2)._2
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")




// GRAVEYARD:
//def simulateMarbleGame(lastMarble: Int, nPlayers: Int, scoreBoard: Map[Int, Int] = Map(),
//                       circle: List[Int] = List(0), n: Int = 1): Map[Int, Int] =
//  if n >= lastMarble then scoreBoard
//  else
//    val (score, next): (Int, List[Int]) = if n % 23 == 0 then
//      val tmp: (Int, List[Int]) = ListHelper.takeHead(ListHelper.rotate(-7, circle))
//      (tmp._1 + n, tmp._2)
//    else
//      (0, ListHelper.appendHead(ListHelper.rotate(2, circle), n))
//
//    val thisPlayer: Int = n % nPlayers
//    val updatedScoreBoard = if !scoreBoard.contains(thisPlayer) then
//      scoreBoard.updated(thisPlayer, score)
//    else
//      scoreBoard.updated(thisPlayer, scoreBoard(thisPlayer) + score)
//
//    simulateMarbleGame(lastMarble, nPlayers, updatedScoreBoard, next, n + 1)

//object ListHelper {
//      def takeHead[A](ls: List[A]): (A, List[A]) = ls match
//        case h :: t => (h, t)
//        case _ => sys.error("empty list cannot takeHead")
//
//      def setHead[A](as: List[A], r: A): List[A] = as match
//        case _ :: t => r :: t
//        case Nil => r :: Nil
//
//      def appendHead[A](as: List[A], r: A): List[A] = as match
//        case h :: t => r :: h :: t
//        case Nil => r :: Nil
//
//      def rotate[A](N: Int, ls: List[A]): List[A] =
//        if ls.isEmpty then ls
//        else
//          val nbound = N % ls.length // skipping the full rotation rounds
//          if nbound < 0 then rotate(nbound + ls.length, ls)
//          else ls.drop(nbound) ::: ls.take(nbound)
//
//  def lazyTakeHead[A](ls: LazyList[A]): (A, LazyList[A]) = ls match
//    case h #:: t => (h, t)
//    case _ => sys.error("empty list cannot takeHead")
//
//  def lazyAppendHead[A](as: LazyList[A], r: A): LazyList[A] = as match
//    case h #:: t => r #:: h #:: t
//    case LazyList() => r #:: LazyList()
//
//  def lazyRotate[A](n: Int, ls: LazyList[A]): LazyList[A] =
//    if ls.isEmpty then ls
//    else
//      val nbound = n % ls.length // skipping the full rotation rounds
//      if nbound < 0 then lazyRotate(nbound + ls.length, ls)
//      else ls.drop(nbound) ++: ls.take(nbound)
//}
//
//def marbleState(f: Int, s: Int): Option[(Int, (Int, Int))] =
//  val next = (f + 2) % s
//  Some(next, (next, s + 1))
//
//// at % 23 --> s + 0 and f - 7
//def marbleLocStream2: LazyList[Int] =
//  val marbleLocations: LazyList[Int] = LazyList.unfold((1, 1))(marbleState)
//  marbleLocations.map(_ + 1)

//
//object MarbleGame {
//
//  def marbleStream: LazyList[Int] =
//    val marbles = LazyList.unfold(1)(n => if n % 23 == 0 then Some(-99, n + 1) else Some(n, n + 1))
//    marbles.filter(_ != -99)
//
//  def marbleState(f: Int, s: Int): Option[(Int, (Int, Int))] =
//    if s % 23 == 0 then
//      val next = math.abs(f - 7) % s
//      Some(next, (next, s + 1))
//    else
//      val next = (f + 2) % s
//      Some(next, (next, s + 1))
//
//  def marbleLocStream: LazyList[Int] =
//    val marbleLocations: LazyList[Int] = LazyList.unfold((1, 1))(marbleState)
//    marbleLocations.map(_ + 1)
//}
//
//def simulateMarbleGame(marbles: LazyList[Int], indices: LazyList[Int], lastMarble: Int, nPlayers: Int): Map[Int, Int] =
//
//  def scoreOfRound(n: Int, i: Int): Int =
//    val sevenBack = marbles.slice(i - 1, i).head
//    sevenBack + n
//
//  def go(n: Int, thisIndices: LazyList[Int], lm: Int, acc: Map[Int, Int] = Map()): Map[Int, Int] =
//    val (ih, ri) = thisIndices.splitAt(1)
//    val i = ih.head
//
//    println(s"$n --> $i = ${marbles.slice(i - 1, i).head}")
//
//    if n >= lm then acc
//    else if n % 23 == 0 then
//      val score = scoreOfRound(n, i)
//      val thisPlayer = n % nPlayers
//      val nextAcc = if !acc.contains(thisPlayer) then
//        acc.updated(thisPlayer, score)
//      else
//        acc.updated(thisPlayer, acc(thisPlayer) + score)
//      go(n + 1, ri, lm, nextAcc)
//    else
//      go(n + 1, ri, lm, acc)
//
//  go(1, indices, lastMarble)
