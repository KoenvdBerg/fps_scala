import jdk.jshell.execution.Util

import scala.io.*
import math.*
import scala.annotation.tailrec
import scala.util.matching.Regex

object day07 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  case class I(a: Long, t: Seq[Long])

  private val input: Seq[I] =
    Source
      .fromResource(s"day07.txt")
      .getLines
      .map {
        case s"$i: $y" => I(i.toLong, y.split(" ").map(_.trim.toLong).toSeq)
      }.toSeq

  def algorithm(todo: I): Boolean =

    def go(r: Seq[Long], acc: Seq[Long]): Seq[Long] =
      if r.isEmpty then acc
      else
        val cur = r.head
        val next = acc.flatMap(l => Seq(l + cur, l  * cur))
        go(r.drop(1), next)
    go(todo.t.drop(1), Seq(todo.t.head))

    val expanded = go(todo.t.drop(1), todo.t.take(1))
    expanded.contains(todo.a)

  private val answer1 = input.filter(algorithm).map(_.a).sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis


  def append(l1: Long, l2: Long): Long = s"$l1$l2".toLong

  def algorithm2(todo: I): Boolean =

    def go(r: Seq[Long], acc: Seq[Long]): Seq[Long] =
      if r.isEmpty then acc
      else
        val cur = r.head
        val next = acc.flatMap(l => Seq(l + cur, l * cur, append(l, cur)))
        go(r.drop(1), next)

    go(todo.t.drop(1), Seq(todo.t.head))

    val expanded = go(todo.t.drop(1), todo.t.take(1))
    expanded.contains(todo.a)

  private val answer2 = input.filter(algorithm2).map(_.a).sum
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")


