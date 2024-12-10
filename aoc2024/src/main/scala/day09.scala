import jdk.jshell.execution.Util

import scala.io.*
import math.*
import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.matching.Regex

object day09 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: Seq[Long] =
    val in = Source
      .fromResource(s"day09.txt")
      .getLines
      .mkString
    parse(in)

  def parse(s: String): Seq[Long] =
    s.zipWithIndex.foldLeft(Seq.empty){ (res: Seq[Long], p: (Char, Int)) =>
      val (c, i) = p
      val digit = s"$c".toInt
      val n = if i % 2 == 0 then Seq.fill(digit)(i/2L) else Seq.fill(digit)(-1L)
      res.appendedAll(n)
    }

  def isComplete(disk: Seq[Long]): Boolean =
    val iSpace = disk.indexWhere(_ == -1L)
    disk.drop(iSpace).forall(_ == -1L)

  @tailrec
  def algorithm(disk: Seq[Long]): Seq[Long] =
    if isComplete(disk) then disk
    else
      val lastFile = disk.lastIndexWhere(_ != -1L)
      val firstSpace = disk.indexWhere(_ == -1L)
      val f = disk(lastFile)
      val u = disk.patch(lastFile, Seq.empty, 1)
      val next = u.patch(firstSpace, Seq(f), 1)
      algorithm(next)

  private val res = algorithm(input)
  private val answer1 = res.zipWithIndex.map((l, i) => if l != -1L then l * i else 0).sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  private val input2: Seq[Disk] =
    val in = Source
      .fromResource(s"day09.txt")
      .getLines
      .mkString
    parse2(in)

  def parse2(s: String): Seq[Disk] =
    s.zipWithIndex.foldLeft(Seq.empty) { (res: Seq[Disk], p: (Char, Int)) =>
      val (c, i) = p
      val digit = s"$c".toInt
      val n = if i % 2 == 0 then Disk(digit, i / 2) else Disk(digit, -1)
      res.appended(n)
    }

  case class Disk(size: Int, id: Int):
    def split: Seq[Disk] = Seq.fill(size)(Disk(1, id))

  def score(disk: Seq[Disk]): Long =
    val all = disk.flatMap(_.split)
    all.zipWithIndex.map((d, i) => if d.id != -1 then d.id.toLong * i else 0L).sum

  def algorithm2(disk: Seq[Disk]) =

    val todo: Seq[Disk] = disk.filter(_.id > -1).sortBy(-_.id)

    @tailrec
    def go(todo: Seq[Disk], acc: Seq[Disk]): Seq[Disk] =
      if todo.isEmpty then acc
      else
        val cur = todo.head
        val spaceI = acc.indexWhere(d => d.id == -1 && d.size >= cur.size)
        val curI = acc.indexWhere(_ == cur)
        if spaceI == -1 then go(todo.drop(1), acc)
        else if spaceI > curI then go(todo.drop(1), acc)
        else
          val space = acc(spaceI)
          val u = acc.patch(curI, Seq(Disk(cur.size, -1)), 1)
          val toReplace =
            if space.size == cur.size then Seq(cur)
            else Seq(cur, Disk(space.size - cur.size, -1))
          val next = u.patch(spaceI, toReplace, 1)
          go(todo.drop(1), next)

    go(todo, disk)

  private val res2 = algorithm2(input2)
  private val answer2 = score(res2)
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")


