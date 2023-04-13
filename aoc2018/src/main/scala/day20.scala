import scala.io.*
object day20 extends App:

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

  def takePars(in: String): String =
    val pars = in.zipWithIndex.filter(x => x._1 == ')' | x._1 == '(')
    val lpar = pars.zipWithIndex.filter(_._1._1 == ')').head._2
    val (s, e) = (pars(lpar - 1)._2, pars(lpar)._2)
    in.slice(s, e + 1)

  def longestRoute(r: String): String =
    def go(i: String): String =
      if i.count(_ == '(') <= 0 then i
      else
        val cs = takePars(i)
        val choices = cs.slice(1, cs.length - 1).split("\\|")
        val sel = choices(choices.zipWithIndex.maxBy(_._1.length)._2)
        go(i.replace(cs, sel))

    go(r)

  private val res = longestRoute(input).length - 2
  private val answer1 = res
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  // NOTE!!! Experiment here with this function, rewriting it to find the first part between ( and )
//  def findFirstInstance[A](as: Array[A], f: (A, A) => Boolean): (Int, Int) = {
//    def loop(n: Int): (Int, Int) = {
//      if n + 1 >= as.length then (-1, -1)
//      else if f(as(n), as(n + 1)) then (n, n + 1)
//      else
//        loop(n + 1)
//    }
//
//    loop(0)
//  }

  def stringFrom(s: String)(f: Char => Boolean): Option[String] =
    def loop(n: Int): Option[String] = {
      if n >= s.length then None
      else if f(s(n)) then Some(s.slice(n, s.length))
      else
        loop(n + 1)
    }
    loop(0)


  def stringFromTo(s: String)(from: Char => Boolean)(to: Char => Boolean): String =
    def go(ins: String, n: Int, start: Int)(f: Char => Boolean): String = {
      if n >= ins.length then ""
      else if f(ins(n)) then
        if start == 0 then ins.slice(0, n+1) else ins.slice(n, ins.length)
      else
        go(ins, n + 1, start)(f)
    }
    go(go(s, 0, 0)(to), 0, 1)(from)

  private val start2: Long =
    System.currentTimeMillis

  private val answer2 = "NONE"
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
