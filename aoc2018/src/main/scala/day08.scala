import scala.io.*
import math.*
import scala.collection.mutable
import scala.collection.mutable.Stack

/**
 * PART 1:
 *
 * the idea is as follows:
 *
 *  1. exit condition --> input has become empty return accumulator
 *  2. parse header 1x
 *    a. parse child node quantity
 *    b. parse metadata entries amount
 *    c. go to end of header line to get metadata entries
 *  3. append metadata entries to accumulator
 */


object day08 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: List[Int] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList
      .head.split(" ")
      .map(_.toInt)
      .toList

  sealed trait Tree[Int]
  case object Leaf extends Tree[Int]
  case class Node(n: Seq[Tree[Int]], meta: Seq[Int], childs: Int) extends Tree[Int]

  def parseHeaderTree(h: mutable.Stack[Int]): Tree[Int] =
    val c = h.pop
    val e = h.pop
    val children = for {
      x <- Range(0,c)
    } yield parseHeaderTree(h)
    val meta = for {
      y <- Range(0,e)
    } yield h.pop
    Node(children, meta, c)


  def treeMetaToList(t: Tree[Int]): List[Int] = t match
    case Leaf => Nil
    case Node(st, meta, _) =>
      meta.toList ::: st.flatMap(tt => treeMetaToList(tt)).toList


  var inc = new mutable.Stack[Int]
  input.map(inc += _)
  private val res1 = parseHeaderTree(inc)
  private val answer1 = treeMetaToList(res1).sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  def treeRootValues(t: Tree[Int]): Seq[Int] = t match
    case Leaf => Nil
    case Node(st, meta, c) =>
      val x = meta.filter(e => e <= st.length & e != 0 )
      println(s"$meta and $x with $c --> $st")
      if c == 0 then meta
      else if x.isEmpty then Seq(0)
      else x.flatMap(i => treeRootValues(st(i-1)))

  private val answer2 = treeRootValues(res1).sum
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
