import scala.io.*
import math.*
import math.Integral.Implicits.*
import scala.annotation.tailrec

/**
 * PART 01:
 *
 * The conversion from SNAFU to Long was not too difficult. However, the conversion from Long to SNAFU was. The 
 * final solution is relatively straightforward. 
 * 
 * It uses a divmod of 5 on the input long. It has an offset of 2, due to the rule about - and =. Example: 
 * 
 * 18 --> 
 * 
 * (18 + 2) /% 5 = (4, 0)  // since we just added 2, the 0 remainder indicates that -2 is needed: i.e. '='
 * (4 + 2) /% 5 = (1, 1)   // since we just added 2, the 1 remainder indicates that a -5 is needed: i.e. '-'
 * (1 + 2) /% 5 = (0, 3)   // the 3 here indicates that a 25 is needed, i.e. '1'
 * 
 * Thus the number is: 1-=
 * 
 * This can be compared to converting any number to binary, as implemented in the utils file. 
 *
 */


object day25 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: List[String] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList
    
  val SNAFU: Map[Char, Long] = Map('2' -> 2L, '1' -> 1L, '0' -> 0L, '-' -> -1L, '=' -> -2L)
    
  def snafuToLong(snafu: String): Long = 
    snafu.foldLeft((snafu.length - 1, 0L)) { (res: (Int, Long), in: Char) => 
      val toAdd: Long = math.pow(5, res._1).toLong * SNAFU(in)
      (res._1 - 1, res._2 + toAdd)
    }._2
    
  @tailrec
  def longToSnafu(in: Long, acc: String = ""): String =
    val (div, rem): (Long, Long) = (in+2) /% 5L
    val snafu: Char = "=-012"(rem.toInt)
    if div == 0 then (acc + snafu).reverse else longToSnafu(div, acc + snafu)
  
  private val res1: Long = input.map(snafuToLong).sum
  private val answer1: String = longToSnafu(res1)
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")
