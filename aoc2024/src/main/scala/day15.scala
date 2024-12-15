import aoc2024.{BoundedGrid, FlatGrid}
import aoc2024.BoundedGrid.*
import jdk.jshell.execution.Util

import scala.io.*
import math.*
import scala.annotation.tailrec
import scala.util.matching.Regex

object day15 extends App:

  private val day: String = "15"

  private val start1: Long =
    System.currentTimeMillis

  private val (grid, directions): (BoundedGrid, String) =
    val in = Source
      .fromResource(s"day$day.txt")
      .getLines
      .toSeq

    val grid = in.takeWhile(_.nonEmpty)
    BoundedGrid.fromString(grid.mkString, grid.head.length) -> in.dropWhile(_.nonEmpty).mkString

  val dirMap: Map[Char, ((Int, Int)) => (Int, Int)] = Map(
    '^' -> grid.neighbourAbove,
    '>' -> grid.neighbourRight,
    'v' -> grid.neighbourBelow,
    '<' -> grid.neighbourLeft
  )

  private val field: Array[Array[Char]] = grid.grid.map(_.toArray).toArray

  def swap(old: (Int, Int), up: (Int, Int)): Unit =
    val cur: Char = field(old._2)(old._1)
    field(old._2).update(old._1, '.')
    field(up._2).update(up._1, cur)

  def moveChain(in: (Int, Int), dir: Char): Boolean =
    val next: (Int, Int) = dirMap(dir)(in)
    val nextChar: Char = field(next._2)(next._1)
    if nextChar == '#' then false
    else if nextChar == '.' then
      swap(in, next)
      true
    else
      if moveChain(next, dir) then
        swap(in, next)
        true
      else false

  def moveAt(in: (Int, Int), dir: Char): (Int, Int) = if moveChain(in, dir) then dirMap(dir)(in) else in
  val robotStart = grid.getPointsOf('@').head
  val end = directions.foldLeft(robotStart){ moveAt}
  val boxes =
    for
      y <- Range(0, grid.yLim)
      x <- Range(0, grid.xLim)
      if field(y)(x) == 'O'
    yield y * 100 + x

  private val answer1 = boxes.sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  val updated: BoundedGrid = BoundedGrid(grid.xLim * 2, grid.yLim,
    grid.grid.map(_.flatMap(c =>
      if c == 'O' then "[]"
      else if c == '@' then "@."
      else s"$c$c")))

  private var field2: Array[Array[Char]] = updated.grid.map(_.toArray).toArray
  def swap2(old: (Int, Int), up: (Int, Int)): Unit =
    val cur: Char = field2(old._2)(old._1)
    field2(old._2).update(old._1, '.')
    field2(up._2).update(up._1, cur)

  def moveSide(in: (Int, Int), dir: Char): Boolean =
    val next: (Int, Int) = dirMap(dir)(in)
    val nextChar: Char = field2(next._2)(next._1)
    if nextChar == '#' then false
    else if nextChar == '.' then
      swap2(in, next)
      true
    else
      if moveSide(next, dir) then
        swap2(in, next)
        true
      else false

  def moveVert(a: (Int, Int), dir: Char): Boolean =

    val fieldCopy: Array[Array[Char]] = field2.map(_.map(identity))

    def swapLocal(old: (Int, Int), up: (Int, Int)): Unit =
      val cur: Char = fieldCopy(old._2)(old._1)
      fieldCopy(old._2).update(old._1, '.')
      fieldCopy(up._2).update(up._1, cur)

    def move(in: (Int, Int)): Boolean =
      val cur = fieldCopy(in._2)(in._1)
      val next: (Int, Int) = dirMap(dir)(in)
      val nextChar: Char = fieldCopy(next._2)(next._1)
      if nextChar == '#' then false
      else if nextChar == '.' then
        swapLocal(in, next)
        true
      else
        val box =
          if nextChar == '[' then Seq(next, next + (1, 0))
          else Seq(next + (-1, 0), next)
        if box.forall(move) then
          swapLocal(in, next)
          true
        else false
    val res = move(a)
    if res then
      field2 = fieldCopy
      res
    else res

  def moveAt2(in: (Int, Int), dir: Char): (Int, Int) =
    if dir == '>' || dir == '<' then
      if moveSide(in, dir) then dirMap(dir)(in) else in
    else
      if moveVert(in, dir) then dirMap(dir)(in) else in

  val robotStart2 = updated.getPointsOf('@').head
  val res2 = directions.foldLeft(robotStart2){ moveAt2 }

  val boxes2 =
    for
      y <- Range(0, updated.yLim)
      x <- Range(0, updated.xLim)
      if field2(y)(x) == '['
    yield y * 100 + x

  private val answer2 = boxes2.sum
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
