import scala.annotation.tailrec
import scala.io.*
import aoc2021.FlatGrid.neighbours4
import aoc2021.Algorithms.floodAlgorithm

object day09 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val (width, input): (Int, Vector[Int]) =
    val infile = Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector
    (infile.head.length, infile.flatten.map(c => s"${c}".toInt))
    
  def findLowPoints(field: Vector[Int], w: Int): Vector[Int] =
    field.indices.foldLeft(Vector.empty) { (res: Vector[Int], index: Int) =>
      val ns: Vector[Int] = neighbours4(index, w, input.length)
      if ns.map(input).forall(p => p > input(index)) then index +: res else res
    }
    
  def bassinSearch(field: Vector[Int], w: Int)(index: Int): Set[Int] =
    neighbours4(index, w, field.length).filter(pred => field(pred) < 9).toSet

  private val res1: Vector[Int] = findLowPoints(input, width) 
  private val answer1: Int = res1.map(f => input(f) + 1).sum 
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  private val res2: Vector[Int] = res1.map { (lowPoint: Int) =>
    floodAlgorithm(bassinSearch(input, width))(lowPoint).size
  }
  private val answer2: Int = res2.sorted.reverse.take(3).product
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")