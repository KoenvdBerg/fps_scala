import aoc2021.FlatGrid

import scala.annotation.tailrec
import scala.io.*
import aoc2021.FlatGrid.neighbours8

import scala.collection.immutable.Queue

object day11 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val (width, input): (Int, Vector[Int]) =
    val infile: Vector[String] = Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector
    
    val width: Int      = infile.head.length
    val in: Vector[Int] = infile.flatMap(_.toVector.map(c => s"$c".toInt))
    (width, in)
    
    
  case class Cavern(octopus: Vector[Int], flashes: Int, flashAll: Boolean): 
  
    def step: Cavern =
      val increased: Vector[Int] = octopus.map(_ + 1)
      
      @tailrec
      def go(q: Queue[Int], oct: Vector[Int], flash: Int): (Vector[Int], Int) =
        if q.isEmpty then (oct, flash)
        else
          val (i, rem) = q.dequeue
          if oct(i) == 0 then go(rem, oct, flash)
          else if oct(i) + 1 >= 10 then
            val newq: Queue[Int] = rem.enqueueAll(neighbours8(i, width, oct.length))
            go(newq, oct.updated(i, 0), flash + 1)
          else go(rem, oct.updated(i, oct(i) + 1), flash)
          
      val start: Queue[Int] = Queue.from(increased.zipWithIndex.filter(_._1 >= 10).map(_._2))
      val next = go(start, increased, 0)
      Cavern(next._1, flashes + next._2, next._2 == octopus.length)


  private val answer1: Int = Iterator.iterate(Cavern(input, 0, false))(_.step).drop(100).next.flashes
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  private val answer2: Int = Iterator.iterate(Cavern(input, 0, false))(_.step).takeWhile(_.flashAll == false).length
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")