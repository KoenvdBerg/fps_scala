import scala.io.*
import math.*
import scala.collection.mutable
import scala.collection.mutable.Stack


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

  object ListHelper {
    def takeHead[A](ls: List[A]): (A, List[A]) = ls match
      case h :: t => (h, t)
      case _ => sys.error("empty list cannot takeHead")

    def setHead[A](as: List[A], r: A): List[A] = as match
      case _ :: t => r :: t
      case Nil => r :: Nil

    def appendHead[A](as: List[A], r: A): List[A] = as match
      case h :: t => r :: h :: t
      case Nil => r :: Nil

    def rotate[A](N: Int, ls: List[A]): List[A] =
      if ls.isEmpty then ls
      else
        val nbound = N % ls.length // skipping the full rotation rounds
        if nbound < 0 then rotate(nbound + ls.length, ls)
        else ls.drop(nbound) ::: ls.take(nbound)
  }

  def simulateMarbleGame(lastMarble: Int, circle: List[Int] = List.empty[Int], n: Int = 0): List[Int] =
    if circle.length == lastMarble then circle
    else
//      if n % 23 == 0 then
      val next: List[Int] = ListHelper.appendHead(ListHelper.rotate(2, circle), n)
      simulateMarbleGame(lastMarble, next, n+1)

  private val answer1 = simulateMarbleGame(72000)
//  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")
  println(s"Answer day $day part 1:  [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  private val answer2 = "NONE"
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
