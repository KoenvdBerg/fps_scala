import scala.io.*
import math.*
import scala.annotation.tailrec

/**
 * PART 01:
 *
 * PART 02:
 *
 */


object day19 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

//  private val input: List[(Char, Char)] =
//    Source
//      .fromResource(s"day$day.txt")
//      .getLines
//      .toList
//      .map((s: String) => (s(0), s(2)))

  extension (c: Vector[Int])
    def -(that: Vector[Int]): Vector[Int] = c.zipWithIndex.map((v: Int, i: Int) => v - that(i))
    def +(that: Vector[Int]): Vector[Int] = c.zipWithIndex.map((v: Int, i: Int) => v + that(i))
    def *(i: Int): Vector[Int] = c.map((v: Int) => v * i)
    def >=(that: Vector[Int]): Boolean = c.zipWithIndex.forall((v: Int, i: Int) => v >= that(i))
    def <=(that: Vector[Int]): Boolean = c.zipWithIndex.forall((v: Int, i: Int) => v <= that(i))

  case class MineState(time: Int, wallet: Vector[Int], bots: Vector[Int]):
    def mine(n: Int): MineState = copy(wallet = wallet + bots * n)
    def progress: MineState = copy(time = time + 1, wallet = wallet + bots)

  object MineState:

    // TODO: fix the off by 1 error 
    def simulate(current: MineState, costs: Vector[Vector[Int]], await: Int, skip: Int): MineState =
      println(current)
      if current.time >= 24 then current
      else
        val t = buyBot(current, costs, -1)
        val n = buyBot(current.mine(await), costs, -1)
        (t, n) match
          case (Some(tt), Some(nn)) if nn > tt =>
            println(s"!!!!!!!gained: ${current.progress.bots}")
            simulate(current.progress, costs, await, skip)
          case _ =>
            val next: MineState = spendAll(current, costs, skip)
            val nextBot: MineState = current.copy(time = current.time + 1, wallet = next.wallet + current.bots, bots = next.bots)
            println(s"spend: ${current.wallet - next.wallet}")
            println(s"gained: ${current.bots}")
            simulate(nextBot, costs, await, skip)
        
    
    @tailrec
    def spendAll(current: MineState, costs: Vector[Vector[Int]], skip: Int): MineState =
      buyBot(current, costs, skip) match
        case Some(i) => spendAll(
          current.copy(wallet = current.wallet - costs(i), bots = current.bots + Vector.fill(4)(0).updated(i, 1)),
          costs,
          skip
        )
        case None => current

    def buyBot(state: MineState, cost: Vector[Vector[Int]], skipWhen: Int): Option[Int] =
      val toSkip: Vector[Int] = if skipWhen < 0 then Vector.empty[Int] else state.bots.zipWithIndex
        .filter((b: Int, i: Int) => cost.map((c: Vector[Int]) => c(i)).forall((v: Int) => b >= v + skipWhen))
        .map(_._2)
      val possibilities = cost
        .zipWithIndex
        .filterNot((_: Vector[Int], i: Int) => toSkip.contains(i))
        .filter((c: Vector[Int], _: Int) => c <= state.wallet)
        .map(_._2)
      possibilities.lastOption



  val cost: Vector[Vector[Int]] = Vector(Vector(4, 0, 0, 0), Vector(2, 0, 0, 0), Vector(3, 14, 0, 0), Vector(2, 0, 7, 0))
  val cost2: Vector[Vector[Int]] = Vector(Vector(2, 0, 0, 0), Vector(3, 0, 0, 0), Vector(3, 8, 0, 0), Vector(3, 0, 12, 0))


  val grid = for {
    i <- 0 to 5
    j <- -1 to 5
  } yield (i, j)
//  private val res1 = grid.filter((i: Int, j: Int) => (i, j) != (0, -1)).map((i: Int, j: Int) => 
//    val res =  MineState.simulate(MineState(0, Vector(0,0,0,0), Vector(1,0,0,0)), cost2, i, j).wallet(3)
//    s"result is: res=$res, i=$i, j=$j"
//  )
  private val res1 = MineState.simulate(MineState(0, Vector(0,0,0,0), Vector(1,0,0,0)), cost2, 0, 4)
  println(res1)
  private val answer1 = None
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis
  
  private val answer2 = None
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
