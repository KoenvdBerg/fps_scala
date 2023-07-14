
import scala.io.*
import math.*
import scala.annotation.tailrec
import cats.data.State

import scala.collection.immutable.Queue

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

  private val input: Vector[Item] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .zipWithIndex
      .map((v: String, i: Int) => Item(v.toInt, i, i))
      .toVector

  case class Item(v: Int, startPos: Int, curPos: Int)
  
  
  def mix(q: Queue[Int], len: Int, acc: Vector[Item]): Vector[Item] =
    if q.isEmpty then acc
    else 
      val (n, rem): (Int, Queue[Int]) = q.dequeue
      // 1 - move based on value i.e. add value to index
      // 2 - other items with i > curPos and i < newPos have index - 1
      val (t, other): (Vector[Item], Vector[Item]) = acc.partition((t: Item) => t.startPos == n)
      val todo: Item = t.head
      val np: Int = (todo.curPos + todo.v + len-1) % (len-1)
      val newPos: Int = if np == 0 then len - 1 else np 
      val newItem: Item = todo.copy(curPos = newPos)
      val nextAcc: Vector[Item] = if newPos == todo.curPos then other
      else if newPos > todo.curPos then other.map{
        case Item(v, s, c) if c > todo.curPos && c <= newPos => Item(v, s, c - 1)
        case x => x
      }
      else other.map{
        case Item(v, s, c) if c >= newPos && c < todo.curPos => Item(v, s, c + 1)
        case x => x
      }
      //println(s"val: ${todo.v} from ${todo.curPos} to ${newPos}")
      //println(s"${(newItem +: nextAcc).sortBy(_.curPos).map(t => (t.v, t.curPos)).mkString(", ")}")
        
      mix(rem, len, newItem +: nextAcc)

  // TODO: obtain the grove coordinates 
  
  // not it: -649
  
  private val queue: Queue[Int] = Queue.from(input.indices)
  private val res1: Vector[Item] = mix(queue, input.length, input)
  private val zeroIndex: Int = res1.find(_.v == 0).get.curPos
  private val indices: List[Int] = List(1000, 2000, 3000).map((i: Int) => (i + zeroIndex) % res1.length)
  private val answer1 = indices.flatMap((i: Int) => res1.find(_.curPos == i)).map(_.v).sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  val answer2 = None
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
