import scala.io.*
import aoc2021.MapUtils

object day14 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val (polymer, steps): (String, Map[String, String]) =
    val infile: Vector[String] = Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector
    
    val step = infile.drop(2).map(s => 
      val both = s.split(" -> ")
      (both.head, both.last)
    ).toMap
    (infile.head, step)

  case class Polymer(doplets: Map[String, Long], counts: Map[String, Long], steps: Map[String, String]):
  
    def next: Polymer =
      val n = doplets.foldLeft((Map.empty[String, Long], counts)) { (res, in) =>
        steps.get(in._1) match
          case Some(v) =>
            val c: Long = doplets(in._1)
            val (split1, split2) = (in._1.head + v, v + in._1.last)
            (MapUtils.op(res._1, Map(split1 -> c, split2 -> c)), MapUtils.op(res._2, Map(v -> c)))
          case None    => res
      }
      Polymer(n._1, n._2, steps)
      
  def getCounts(in: String, slide: Int): Map[String, Long] =
    in.sliding(slide).toList.groupBy(identity).map((a, b) => a -> b.length.toLong).toMap

  private val start: Map[String, Long] = getCounts(polymer, 2)
  private val startCounts: Map[String, Long] = getCounts(polymer, 1)
  private val res1 = Iterator.iterate(Polymer(start, startCounts, steps))(_.next).drop(10).next.counts
  private val answer1 = res1.values.max - res1.values.min
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  private val res2 = Iterator.iterate(Polymer(start, startCounts, steps))(_.next).drop(40).next.counts
  private val answer2 = res2.values.max - res2.values.min
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")

