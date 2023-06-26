import scala.io.*
import math.*
import scala.annotation.tailrec

/**
 * PART 01:
 *
 * The solution for me was to have two Vector[Vector[Int]] available: one with the input as is, and one with the input 
 * transposed. This allowed me to just have to check the left and right side of a tree, in the original input and in 
 * the transposed input. The transposed input essentially are the trees above and below the current tree. 
 *
 * PART 02:
 *
 * Refactored the isVisible() function from part 1 to now compute the score. It still works using the original input and 
 * the transposed input. 
 *
 */


object day08 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: Vector[TreeRow] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector
      .map(_.map(_.toString.toInt).toVector)


  type TreeRow = Vector[Int]

  def isVisible(i: Int, j: Int, trees: TreeRow, treesTransposed: TreeRow): Boolean =
    def checkRow(index: Int, row: TreeRow): Boolean =
      val (left, right): (TreeRow, TreeRow) = row.splitAt(index)
      left.forall(_ < row(index)) || right.drop(1).forall(_ < row(index))

    // tree is on border and thus visible OR check if tree is taller than surrounding trees
    i == 0 || i == (trees.length-1) || j == 0 || j == (treesTransposed.length-1) || checkRow(i, trees) || checkRow(j, treesTransposed)

  private val transposedInput = input.transpose
  private val indices: IndexedSeq[(Int, Int)] = for {
    i <- input.indices
    j <- input.indices
  } yield (i, j)

  private val res1: IndexedSeq[Boolean] = indices.map((f: (Int, Int)) => isVisible(f._1, f._2, input(f._2), transposedInput(f._1)))
  private val answer1 = res1.count(identity)
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  def scenicScore(i: Int, j: Int, trees: TreeRow, treesTransposed: TreeRow): Int =

    def scoreSide(side: TreeRow, height: Int): Int =
      if side.isEmpty then 0 else  // at border of map
        val sideView: TreeRow = side.takeWhile(_ < height)
        if sideView.length == side.length then sideView.length else sideView.length + 1  // the +1 accounts for the blocking tree 

    def score(index: Int, row: TreeRow): Int =
      val (left, right): (TreeRow, TreeRow) = row.splitAt(index)
      scoreSide(left.reverse, row(index)) * scoreSide(right.drop(1), row(index))

    score(i, trees) * score(j, treesTransposed)

  private val res2: IndexedSeq[Int] = indices.map((f: (Int, Int)) => scenicScore(f._1, f._2, input(f._2), transposedInput(f._1)))
  private val answer2 = res2.max
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
