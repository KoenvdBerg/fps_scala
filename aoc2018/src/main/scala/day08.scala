import scala.io.*
import math.*
import scala.collection.mutable
import scala.collection.mutable.Stack


object day08 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: Seq[Int] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList
      .head.split(" ")
      .map(_.toInt)

  sealed trait Tree[+A]:
    def metaToList: List[Int] = this match
      case Node(st, meta, _) =>
        meta.toList ::: st.flatMap(tt => tt.metaToList).toList

    def treeRootValues: Seq[Int] = this match
      case Node(st, meta, nChilds) =>
        val viableSubNodes: Seq[Int] = meta.filter(e => e <= st.length & e != 0)
        if nChilds == 0 then meta
        else if viableSubNodes.isEmpty then Seq(0)
        else viableSubNodes.flatMap(i => st(i-1).treeRootValues)

  case class Node[A](n: Seq[Tree[A]], meta: Seq[Int], childs: Int) extends Tree[A]

  object Tree {
    def treeFromHeader(h: Seq[Int]): Tree[Int] =

      val mutableSeq = new mutable.Stack[Int]

      def fillStack(s: mutable.Stack[Int], is: Seq[Int]): mutable.Stack[Int] =
        is.map(s += _)
        s

      def parseHeader(s: mutable.Stack[Int]): Tree[Int] =
        val c: Int = s.pop
        val e: Int = s.pop
        val children: Seq[Tree[Int]] = for {
          _ <- Range(0, c)
        } yield parseHeader(s)
        val meta: Seq[Int] = for {
          _ <- Range(0, e)
        } yield s.pop
        Node(children, meta, c)

      fillStack(mutableSeq, h)
      parseHeader(mutableSeq)
  }


  private val res1 = Tree.treeFromHeader(input)
  private val answer1 = res1.metaToList.sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis


  private val answer2 = res1.treeRootValues.sum
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")

//
//def treeRootValues(t: Tree[Int]): Seq[Int] = t match
//  case Leaf => Nil
//  case Node(st, meta, c) =>
//    val x = meta.filter(e => e <= st.length & e != 0)
//    println(s"$meta and $x with $c --> $st")
//    if c == 0 then meta
//    else if x.isEmpty then Seq(0)
//    else x.flatMap(i => treeRootValues(st(i - 1)))