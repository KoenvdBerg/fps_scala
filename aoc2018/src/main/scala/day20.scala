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

  private val start2: Long =
    System.currentTimeMillis

  private val answer2 = "NONE"
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
