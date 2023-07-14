
import scala.io.*
import math.*
import scala.annotation.tailrec
import cats.data.State

import scala.collection.immutable.Queue
import scala.collection.mutable.ArrayBuffer

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

  private val input: Vector[Long] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .map((v: String) => v.toLong)
      .toVector

  def getPos(idx: Int, n: Long, s: Int): Int =
    val dir: Int = ((idx + n) % s).toInt
    if dir <= 0 then s + dir else dir
    
  def mix(in: Vector[Long], times: Int): Vector[Long] = 
    
    val size: Int = in.length - 1
    val buffer = ArrayBuffer.from(in.zipWithIndex)
    
    @tailrec
    def go(q: Queue[(Long, Int)]): Unit = 
      if q.isEmpty then ()
      else 
        val (n, rem) = q.dequeue
        val idx: Int = buffer.indexOf(n)
        val elem: (Long, Int) = buffer(idx)
        buffer.remove(idx)  // happy side effect here
        val np: Int = getPos(idx, elem._1, size)
        buffer.insert(np, elem)  // another happy side effect here
        go(rem)
        
    def gotimes(t: Int): Unit =
      if t <= 0 then ()
      else
        println(buffer.map(_._1))  
        go(Queue.from(in.zipWithIndex))
        gotimes(t - 1)
      
    gotimes(times)
    //go(Queue.from(in.zipWithIndex))
    buffer.toVector.map(_._1)
      
  
  private val res1: Vector[Long] = mix(input, 1)
  private val zeroIndex: Int = res1.indexWhere(_ == 0)
  private val indices: List[Int] = List(1000, 2000, 3000).map((i: Int) => (i + zeroIndex) % res1.length)
  private val answer1 = indices.map((i: Int) => res1(i)).sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis
    
  private val res2: Vector[Long] = mix(input.map(_ * 811589153L), 10)
  private val zeroIndex2: Int = res1.indexWhere(_ == 0)
  private val indices2: List[Int] = List(1000, 2000, 3000).map((i: Int) => (i + zeroIndex2) % res2.length)
  private val answer2 = indices2.map((i: Int) => res2(i)).sum
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
