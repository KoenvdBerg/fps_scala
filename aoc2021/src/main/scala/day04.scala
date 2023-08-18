import scala.annotation.tailrec
import scala.io.*

object day04 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val (numbers, boards): (Vector[Int], Vector[Vector[Int]])  =
    val infile = Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector.filter(!_.isBlank)

    (
      infile.head.split(',').map(_.toInt).toVector,
      infile.drop(1).flatMap(_.trim.split("\\s+").map(_.trim.toInt)).grouped(25).toVector
    )
  
  def mark(n: Int)(board: Vector[Int]): Vector[Int] =
    val found: Int = board.indexOf(n)
    if found == -1 then board else board.updated(found, -1)

  def isWinner(board: Vector[Int]): Boolean =
    board.grouped(5).exists(_.forall(_ == -1)) ||
      board.grouped(5).toVector.transpose.exists(_.forall(_ == -1))
    
  @tailrec
  def firstWinner(boards: Vector[Vector[Int]], i: Int): (Vector[Int], Int) =
    val winner = boards.indexWhere(isWinner)
    if winner > -1 then (boards(winner), numbers(i - 1))
    else firstWinner(boards.map(mark(numbers(i))), i + 1)

  @tailrec
  def lastWinner(boards: Vector[Vector[Int]], i: Int): (Vector[Int], Int) =
    if boards.length == 1 && isWinner(boards.head) then (boards.head, numbers(i-1))
    else 
      val winner: Int = boards.indexWhere(isWinner)
      if winner > -1 then lastWinner(boards.patch(winner, Vector.empty, 1), i)
      else lastWinner(boards.map(mark(numbers(i))), i + 1)

  private val res1 = firstWinner(boards, 0)
  private val answer1 = res1._1.filter(_ != -1).sum * res1._2
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis
  
  private val res2 = lastWinner(boards, 0)
  private val answer2 = res2._1.filter(_ != -1).sum * res2._2
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")