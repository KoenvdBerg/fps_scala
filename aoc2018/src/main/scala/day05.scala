// IDEA: convert Char to Int and then scanRight for diff is abs 26 (or smth)
// if HIT --> recur

import scala.io.*
import math.*

object day05 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: String =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList
      .head

  def findFirstPair(s: String)(f: (Char, Char) => Boolean): String =
    def loop(n: Int): String = {
      if n + 1 >= s.length then ""
      else if f(s(n), s(n + 1)) then s"${s(n)}${s(n+1)}"
      else
        loop(n + 1)
    }
    loop(0)

  def destructPolymer(polymer: String): String =
    val hit = findFirstPair(polymer)((x: Char, y: Char) => math.abs(x.toInt - y.toInt) == 32)
    if hit == "" then polymer
    else destructPolymer(polymer.replace(hit, ""))

  private val answer1 = destructPolymer(input).length
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  val alphabet = Range(97,123).toList.map(_.toChar)
  val res = alphabet.map(
    c =>
      val newin = input.replace(c.toString, "").replace(c.toUpper.toString, "")
      destructPolymer(newin).length
  )
  private val answer2 = res.min
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")

