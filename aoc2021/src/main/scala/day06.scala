import scala.annotation.tailrec
import scala.io.*

object day06 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: Map[Int, Long] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector
      .flatMap(_.split(',').toVector.map(_.toInt))
      .groupBy(identity).map((i, l) => (i, l.length.toLong)).toMap

  
  extension (fish: Map[Int, Long]) 
    
    def next: Map[Int, Long] =
      val move = fish.foldLeft(Map.empty) { (res: Map[Int, Long], in: (Int, Long)) => 
        res.updated(in._1 - 1, in._2)
      }
      move.get(-1) match
        case Some(v) => move.updated(6, move.getOrElse(6, 0L) + v).updated(8, move.getOrElse(8, 0L) + v).removed(-1)
        case None    => move 
  
  
  private val res1: Map[Int, Long] = Iterator.iterate(input)(_.next).drop(80).next
  private val answer1: Long = res1.values.sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  private val res2: Map[Int, Long] = Iterator.iterate(input)(_.next).drop(256).next
  private val answer2: Long = res2.values.sum
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")