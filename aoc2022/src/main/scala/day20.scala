
import scala.io.*
import math.*
import scala.annotation.tailrec
import cats.data.State

import scala.collection.immutable.Queue
import scala.collection.mutable.ListBuffer

/**
 * PART 01:
 *
 * Easy foldRight
 *
 * PART 02:
 *
 * Sort and then take 3 and sum
 *
 */


object day20 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: Vector[Int] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .map((v: String) => v.toInt)
      .toVector

  def getPos(idx: Int, n: Int, s: Int): Int =
    val dir: Int = (idx + n) % s
    if dir <= 0 then s + dir else dir
    
  def mix(in: Vector[Int]): Vector[Int] = 
    
    val size: Int = in.length - 1
    val buffer = ListBuffer.from(in.zipWithIndex)
    
    @tailrec
    def go(q: Queue[(Int, Int)]): Unit = 
      if q.isEmpty then ()
      else 
        val (n, rem) = q.dequeue
        val idx: Int = buffer.indexOf(n)
        val elem: (Int, Int) = buffer(idx)
        
        buffer.remove(idx)
        val np: Int = getPos(idx, elem._1, size)
        buffer.insert(np, elem)
        go(rem)
        
    go(Queue.from(in.zipWithIndex))
    buffer.toVector.map(_._1)
      
  
  private val res1: Vector[Int] = mix(input)
  private val zeroIndex: Int = res1.indexWhere(_ == 0)
  private val indices: List[Int] = List(1000, 2000, 3000).map((i: Int) => (i + zeroIndex) % res1.length)
  private val answer1 = indices.map((i: Int) => res1(i)).sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  val answer2 = None
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
