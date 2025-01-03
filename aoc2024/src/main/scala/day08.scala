import scala.io.*
import scala.annotation.tailrec
import aoc2024.BoundedGrid

object day08 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  val grid: BoundedGrid = BoundedGrid.fromResource("day08.txt")

  def getAntiNode(p1: (Int, Int), p2: (Int, Int)): Seq[(Int, Int)] =
    val c = grid.coefficient(p1, p2)
    Seq(grid.stepNeighbour(p1, c), grid.stepNeighbour(p2, c))
      .flatten
      .filterNot(p => p == p1 || p == p2)

  def algorithm(t: Char): Seq[(Int, Int)] =
    val coords: Seq[(Int, Int)] = grid.getPointsOf(t)

    @tailrec
    def go(todo: Seq[(Int, Int)], acc: Seq[(Int, Int)]): Seq[(Int, Int)] =
      if todo.isEmpty then acc
      else
        val cur = todo.head
        val next = todo.drop(1)
        val antiNodes = next.flatMap(j => getAntiNode(cur, j))
        go(next, acc ++ antiNodes)

    go(coords, Seq.empty[(Int, Int)])

  val types = grid.grid.flatten.filterNot(_ == '.').distinct

  private val answer1 = types.flatMap(algorithm).distinct.length
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  def getAntiNode2(p1: (Int, Int), p2: (Int, Int)): Seq[(Int, Int)] =
    val c = grid.coefficient(p1, p2)
    grid.line(p1, c)

  def algorithm2(t: Char): Seq[(Int, Int)] =
    val coords: Seq[(Int, Int)] = grid.getPointsOf(t)

    @tailrec
    def go(todo: Seq[(Int, Int)], acc: Seq[(Int, Int)]): Seq[(Int, Int)] =
      if todo.isEmpty then acc
      else
        val cur = todo.head
        val next = todo.drop(1)
        val antiNodes = next.flatMap(j => getAntiNode2(cur, j))
        go(next, acc ++ antiNodes)

    go(coords, Seq.empty[(Int, Int)])

  private val answer2 = types.flatMap(algorithm2).distinct.length
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
