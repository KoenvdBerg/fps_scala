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


  private val start2: Long =
    System.currentTimeMillis

  private val answer2 = "NONE"
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
