import scala.io.*
import math.*
import scala.annotation.tailrec

object day06 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis
    
  case class Race(t: Long, d: Long):
    
    def distances: Vector[Long] = (0L to t).toVector.map(tt => (t - tt) * tt)
    
    def wins: Long = distances.count(t => t > d)
    
    def winsFast: Long =

      def go(tt: Long, count: Long): Long =
        if tt > t then count
        else 
          val score: Long = (t - tt) * tt
          if score > d then go(tt+1, count+1)
          else go(tt+1, count)
      
      go(0, 0)
        

  private val input: Seq[Race] =
    val in: Seq[String] = Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList
    
    val races: Seq[Vector[String]] = in.map(_.split("\\s+").toVector)
    races.transpose.drop(1).map(v => Race(v.head.toInt, v.last.toInt))

  private val res1: Seq[Long] = input.map(_.wins)
  private val answer1: Long = res1.product
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  private val input2: Race =
    val in: Seq[String] = Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList

    val races: Seq[Vector[String]] = in.map(_.split("\\s+").toVector)
    val oneR: Seq[String] = races.map(_.drop(1).mkString)
    Race(oneR.head.toLong,oneR.last.toLong)
    
  val res2: Long = input2.winsFast
  private val answer2: Long = res2
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
