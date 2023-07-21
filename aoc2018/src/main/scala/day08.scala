import scala.io.*
import math.*
import scala.collection.mutable
import aoc2018.Combinator.Parser
import aoc2018.Combinator.Parser.*

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

  private val input: String =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toIndexedSeq.head + " "

  case class Tree(nodes: IndexedSeq[Tree], metaData: IndexedSeq[Int]):
    def allMeta: IndexedSeq[Int] =
      this.metaData ++ this.nodes.flatMap(tt => tt.allMeta)
    def treeRootValues: Seq[Int] =
      val viableSubNodes: Seq[Int] = this.metaData.filter(e => e <= this.nodes.length & e != 0)
      if this.nodes.isEmpty then this.metaData
      else if viableSubNodes.isEmpty then Seq(0)
      else viableSubNodes.flatMap(i => this.nodes(i - 1).treeRootValues)

  object Tree:
    val entry: Parser[Int] = for {e <- Parser.int; _ <- Parser.string(" ")} yield e
    val header: Parser[(Int, Int)] = entry ** entry
    def meta(amount: Int): Parser[List[Int]] = Parser.listOfN(amount, entry)

    // TODO: Perhaps this can be made more performant using a State[Int, IndexedSeq[Int]] Monad
    val parseTree: Parser[Tree] = for {
        head <- header
        nodes <- Parser.listOfN(head._1, parseTree).map(_.toIndexedSeq)
        entries <- meta(head._2).map(_.toIndexedSeq)
      } yield Tree(nodes, entries)

    def treeFromHeader(h: IndexedSeq[Int]): Tree =

      val mutableSeq: mutable.Stack[Int] = h.to(mutable.Stack)

      def parseHeader(s: mutable.Stack[Int]): Tree =
        val c: Int = s.pop
        val e: Int = s.pop
        val children: IndexedSeq[Tree] = for {
          _ <- Range(0, c)
        } yield parseHeader(s)
        val meta: IndexedSeq[Int] = for {
          _ <- Range(0, e)
        } yield s.pop
        Tree(children, meta)

      parseHeader(mutableSeq)

  //private val res1: Tree = Tree.parseTree.run(input) match
  //  case Right(t) => t
  //  case Left(e)  => sys.error(e)

  private val res1: Tree = Tree.treeFromHeader(input.split(" ").map(_.toInt))
  private val answer1 = res1.allMeta.sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  private val answer2 = res1.treeRootValues.sum
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")


