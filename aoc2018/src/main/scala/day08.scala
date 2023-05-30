import scala.io.*
import math.*
import scala.collection.mutable
import scala.collection.mutable.Stack

/**
 *
 * PART 1:
 *
 * The difficult part was parsing in the tree structure from the string. This can be done with a mutable stack,
 * by going over the stack and popping value for value from the head. Then continuing recursively for any children
 * that were encountered.
 *
 * Then part 1 is solved by taking out all the meta-values and computing the sum.
 *
 * PART 2:
 *
 * Essentially adding an extra method to the Tree case class that follows the logic stated in the problem. Please
 * see the code below to see how it works.
 *
 */


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

  case class Tree(stub: Seq[Tree], meta: Seq[Int]):
    def metaToSeq: Seq[Int] =
      this.meta ++ this.stub.flatMap(tt => tt.metaToSeq)
    def treeRootValues: Seq[Int] =
      val viableSubNodes: Seq[Int] = this.meta.filter(e => e <= this.stub.length & e != 0)
      if this.stub.isEmpty then this.meta
      else if viableSubNodes.isEmpty then Seq(0)
      else viableSubNodes.flatMap(i => this.stub(i - 1).treeRootValues)


  object Tree {
    def treeFromHeader(h: Seq[Int]): Tree =

      val mutableSeq: mutable.Stack[Int] = h.to(mutable.Stack)

      def parseHeader(s: mutable.Stack[Int]): Tree =
        val c: Int = s.pop
        val e: Int = s.pop
        val children: Seq[Tree] = for {
          _ <- Range(0, c)
        } yield parseHeader(s)
        val meta: Seq[Int] = for {
          _ <- Range(0, e)
        } yield s.pop
        Tree(children, meta)

      parseHeader(mutableSeq)
  }


  private val res1 = Tree.treeFromHeader(input)
  private val answer1 = res1.metaToSeq.sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis


  private val answer2 = res1.treeRootValues.sum
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
