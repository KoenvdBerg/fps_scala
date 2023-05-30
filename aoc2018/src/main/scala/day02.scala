import scala.annotation.tailrec
import scala.io.*

/**
 *
 * PART 1:
 *
 * The approach is to go over every string in the input and check for each string if there are N amount of identical
 * characters present. In this case, N is 2 and 3. The function below called countLetters() implements that.
 *
 * The answer is the count with N=2 * count with N=3
 *
 *
 * PART 2:
 *
 * The solution of part 2 consists of 2 functions:
 *
 * 1) function that compares 2 strings and returns the overlapping characters
 * 2) function that compares every string to every other string in a list
 *
 * Using these two functions, the solution could be found. Please see the code below to see how.
 *
 */

object day02 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: List[String] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList

  /**
   * Checks if a string has n occurrences of any given character of that string.
   */
  def countLetters(ls: String, n: Int): Int =
    @tailrec
    def go(m: Int): Int =
      if m >= ls.length then 0                  // not found, return 0
      else if ls.count(_ == ls(m)) == n then 1  // found, return 1
      else go(m+1)  // continue looking
    go(0)

  private val answer1: Int = input.map(x => countLetters(x, 2)).sum * input.map(x => countLetters(x, 3)).sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  /**
   * Finds the number of overlapping chars at the same index between two strings of equal length
   */
  def overlappingChars(s1: String, s2: String): String =
    @tailrec
    def go(n: Int, acc: String): String =
      if n >= s1.length then acc
      else if s1(n) == s2(n) then go(n+1, s"${acc}${s1(n)}")
      else go(n+1, acc)
    go(0, "")


  /**
   * Takes head of list of strings, then compares that head to every other string in the list. Exit condition
   * is when the difference between two strings is exactly 1 character at the same index.
   */
  @tailrec
  def multiCompare(ls: List[String]): (String, Int) = ls match
    case h :: t =>
      val c: List[(String, Int)] = t
        .map((x: String) => overlappingChars(h, x))   // compare head to every other remaining string in tail
        .map((o: String) => (o, h.length - o.length)) // find difference in length, keep comparison result (o)
        .filter((p: (String, Int)) => p._2 == 1)      // filter for length difference equals 1
      if c.nonEmpty then c.head  // exit condition
      else multiCompare(t)       // continue looking on the tail
    case Nil => ("", 0)

  private val (answer2, _): (String, Int) = multiCompare(input)
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
