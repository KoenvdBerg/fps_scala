import scala.io.*
import math.*
import scala.annotation.tailrec

object day04 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  case class Card(id: Int, winning: Vector[Int], hand: Vector[Int])

  private val input: Vector[Card] =

    def parse(s: String): Card = s match
      case s"$id: $winning | $hand" => Card(id.split("\\s+").toVector.last.toInt,
        winning.strip.split("\\s+").toVector.map(_.toInt),
        hand.strip.split("\\s+").toVector.map(_.toInt))
      case _ => sys.error("sdf")

    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector
      .map(parse)

  def computeScore(card: Card): Long =
    val overlap = card.winning.intersect(card.hand)
    if overlap.isEmpty then 0L
    else overlap.drop(1).foldLeft(1L)((res, _) => res * 2L)

  private val answer1 = input.map(computeScore).sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  def addWinningCard(cards: Map[Card, Long], toAdd: Map[Int, Long]): Map[Card, Long] =
    toAdd.foldLeft(cards) { (cs: Map[Card, Long], in: (Int, Long)) =>
      val index: Int = in._1
      val score: Long = in._2
      val todo: Option[Card] = cs.keySet.find(_.id == index)
      todo.map(css => cs.updated(css, cs.get(css).map(_ + score).getOrElse(0L))).getOrElse(cs)
    }

  def processPile(cards: Map[Card, Long]): Map[Card, Long] =
    cards.toVector.sortBy(_._1.id).foldLeft(cards) { (res: Map[Card, Long], in: (Card, Long)) =>
      val card: Card = in._1
      val overlap: Int = card.winning.intersect(card.hand).length
      val toUpdate: Map[Int, Long] = (card.id + 1 to (card.id + overlap)).map(i => (i, res.getOrElse(card, 0L))).toMap
      addWinningCard(res, toUpdate)
    }

  val cardMap: Map[Card, Long] = input.map(f => (f, 1L)).toMap
  private val answer2 = processPile(cardMap).values.sum
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
