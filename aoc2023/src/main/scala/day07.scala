import scala.io.*
import math.*
import scala.annotation.tailrec

object day07 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  
  case class Hand(cards: Vector[Int], bet: Long):
    
    private val counts = cards.groupBy(identity).map(_._2.length).toVector
    
    def score: Int =
      if counts.max == 5 then 10
      else if counts.max == 4 then 9
      else if counts.min == 2 && counts.max == 3 then 8
      else if counts.max == 3 then 7
      else if counts.count(_ == 2) == 2 then 6
      else if counts.count(_ == 2) == 1 then 5
      else 4
    
    def toTuple: (Int, Int, Int, Int, Int) = (cards(0), cards(1), cards(2), cards(3), cards(4))
    
    def jscore: Int =
      Vector(2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 13, 14).map(r =>
        val newc = cards.map(f => if f == 0 then r else f)
        Hand(newc, bet).score
      ).max
  
  object Hand: 
    
    def parse(s: String, p: Int): Hand = s match
      case s"$c $bet" =>
        val cards: Seq[Int] = c.map {
          case cc@('2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') => s"$cc".toInt
          case 'T' => 10
          case 'J' => if p == 1 then 11 else 0
          case 'Q' => 12
          case 'K' => 13
          case 'A' => 14
        }
        Hand(cards.toVector, bet.toLong)
      case _ => sys.error("PARSING")

  private val input: Vector[String] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector

  private val res1: Seq[Long] = input.map(s => Hand.parse(s, 1)).sortBy(h => (h.score, h.toTuple)).zipWithIndex.map((s, i) => s.bet * (i+1))
  private val answer1: Long = res1.sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis
  
  private val res2: Seq[Long] = input.map(s => Hand.parse(s, 2)).sortBy(h => (h.jscore, h.toTuple)).zipWithIndex.map((s, i) => s.bet * (i + 1))
  private val answer2: Long = res2.sum
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
