
import scala.io.*
import math.*
import scala.annotation.tailrec

import scala.collection.immutable.Queue
import scala.collection.mutable.ArrayBuffer

/**
 * PART 01:
 *
 * First tried solving this without any mutable data structures, but then found that I had to use them to get the
 * mixing right and also speedy.
 *
 * PART 02:
 *
 * Updated code for part 1 to be able to run multiple times. See below.
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
    if dir < 0 then s + dir else dir
    
  def mix(in: Vector[Long], times: Int): Vector[Long] = 
    
    val size: Int = in.length - 1  // length here -1 because the element to be mixed is removed first, leading to a size smaller by 1. See below
    val buffer = ArrayBuffer.from(in.zipWithIndex)

    def go(elem: (Long, Int)): Unit =
      val idx: Int = buffer.indexOf(elem)
      val toMix: (Long, Int) = buffer(idx)
      buffer.remove(idx)        // happy side effect here --> first remove element to mix
      val np: Int = getPos(idx, toMix._1, size)
      buffer.insert(np, toMix)  // another happy side effect here --> then insert element to mix based on logic of getPos()

    def gotimes(t: Int): Unit =
      if t <= 0 then ()
      else
        in.zipWithIndex.foldLeft(())((_, elem) => go(elem))
        gotimes(t - 1)
      
    gotimes(times)
    buffer.toVector.map(_._1)
      
  
  private val res1: Vector[Long] = mix(input, 1)
  private val zeroIndex: Int = res1.indexWhere(_ == 0)
  private val indices: List[Int] = List(1000, 2000, 3000).map((i: Int) => (i + zeroIndex) % res1.length)
  private val answer1 = indices.map((i: Int) => res1(i)).sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  private val res2: Vector[Long] = mix(input.map(_ * 811589153), 10)
  private val zeroIndex2: Int = res2.indexWhere(_ == 0)
  private val indices2: List[Int] = List(1000, 2000, 3000).map((i: Int) => (i + zeroIndex2) % res2.length)
  private val answer2 = indices2.map((i: Int) => res2(i)).sum
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
