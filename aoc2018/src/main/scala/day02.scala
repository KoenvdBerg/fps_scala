import scala.io.*

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

  def countLetters(ls: String, n: Int): Boolean =
    def go(m: Int): Boolean =
      if m >= ls.length then false
      else if ls.count(_ == ls(m)) == n then true else go(m+1)
    go(0)

  private val answer1: Int = input.map(x => countLetters(x, 2)).filter(y => y == true).length * input.map(x => countLetters(x, 3)).filter(y => y == true).length
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  def overlappingChars(s1: String, s2: String): String =
    def go(n: Int, acc: String): String =
      if n >= s1.length then acc
      else if s1(n) == s2(n) then go(n+1, s"${acc}${s1(n)}")
      else go(n+1, acc)
    go(0, "")


  def multiCompare(ls: List[String]): (String, Int) = ls match
    case h :: t =>
      val c = t.map(x =>
        val tmp = overlappingChars(h, x)
        (tmp, h.length - tmp.length)).filter(x => x._2 == 1)
      if c.length >= 1 then c.head else multiCompare(t)
    case Nil => ("", 0)

  private val (answer2, _): (String, Int) = multiCompare(input)
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
