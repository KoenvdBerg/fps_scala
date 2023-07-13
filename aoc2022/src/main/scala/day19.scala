import scala.io.*
import aoc2022.VectorUtils.*
import aoc2022.GameTree.DecisionTree.*
import aoc2022.GameTree.DecisionTree
import cats.data.State

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

  private val input: List[Vector[Vector[Int]]] =
    
    def parse(s: String): Vector[Vector[Int]] = s match
      case s"Blueprint $n: Each ore robot costs $ore ore. Each clay robot costs $clay ore. Each obsidian robot costs $ob1 ore and $ob2 clay. Each geode robot costs $geo1 ore and $geo2 obsidian." => 
        Vector(
          Vector(ore.toInt,0,0,0),
          Vector(clay.toInt,0,0,0),
          Vector(ob1.toInt,ob2.toInt,0,0),
          Vector(geo1.toInt,0,geo2.toInt,0)
        )
      case _ => sys.error(s"cannot parse $s")
    
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList
      .map(parse)

 
    
  case class MineState(time: Int, wallet: Vector[Int], bots: Vector[Int]):
    def lookForward(n: Int): MineState = copy(wallet = wallet + bots * n)
    def progress: MineState = copy(time = time + 1, wallet = wallet + bots)
    
  object MineState:
    
    
//    def buyQ(s: MineState, cost: Vector[Vector[Int]]): List[MineState] =
//      val options: List[Int] = cost
//        .zipWithIndex
//        .filter((c: Vector[Int], _: Int) => c <= s.wallet)
//        .map(_._2)
//        .toList
//      val purchases: List[MineState] = options.map((i: Int) => s.copy(wallet = s.wallet - cost(i), bots = s.bots + Vector.fill(4)(0).updated(i, 1)))
//      s :: purchases
//    def mineQ(s: MineState): MineState = s.progress
    
    
    def mineT(s: MineState): DecisionTree[MineState] = Decision(List(Result(s.progress)))
    def progress(s: MineState, gainedBots: Vector[Int]): DecisionTree[MineState] = Decision(List(Result(s.copy(time = s.time + 1, wallet = s.wallet + s.bots - gainedBots))))
    
    def buyT(s: MineState, cost: Vector[Vector[Int]]): DecisionTree[MineState] =
      if s.bots(0) > 4 || s.bots(1) > 5 || s.bots(2) > 4 then Decision(List(Result(s)))
//      else if s.time > 10 && (s.bots(1) < 4 || s.bots(0) < 2) then Decision(Nil)
      else 
        val options: List[Int] = cost 
          .zipWithIndex
          .filter((c: Vector[Int], _: Int) => c <= s.wallet)
          .map(_._2)
          .toList
        // TODO: make sure to halt here by returning empty Tree if it's not plausable to have the best tree here
        val purchases: List[MineState] = options.map((i: Int) => s.copy(wallet = s.wallet - cost(i), bots = s.bots + Vector.fill(4)(0).updated(i, 1)))
        if s.time > 14 then Decision(purchases.map(Result.apply))
        else Decision((s :: purchases).map(Result.apply))
      
    def roundT(s: MineState, cost: Vector[Vector[Int]]): DecisionTree[MineState] = for {
      m <- buyT(s, cost)
      x <- progress(m, m.bots - s.bots)
    } yield x

    def programT(start: MineState, time: Int, costs: Vector[Vector[Int]]): DecisionTree[MineState] = for {
      x   <- roundT(start, costs)
      fin <- if time <= 1 then Result(x) else programT(x, time-1, costs)
    } yield fin
  

  
  val cost1: Vector[Vector[Int]] = Vector(Vector(4, 0, 0, 0), Vector(2, 0, 0, 0), Vector(3, 14, 0, 0), Vector(2, 0, 7, 0))
  val cost2: Vector[Vector[Int]] = Vector(Vector(2, 0, 0, 0), Vector(3, 0, 0, 0), Vector(3, 8, 0, 0), Vector(3, 0, 12, 0))
  
  private val start: MineState = MineState(0, Vector(0,0,0,0), Vector(1,0,0,0))
  
  //println(MineState.roundT(MineState(0, Vector(10,10,10,10), Vector(2,3,3,3)), cost1))
  
  val test = MineState.programT(start, 18,  cost1)
  private val answer1 = DecisionTree.treeToList(test).maxBy(_.wallet(3))
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis
  
  private val answer2 = None
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")


  /**
   * object day19 extends App:
   *
   * private val day: String =
   * this.getClass.getName.drop(3).init
   *
   * private val start1: Long =
   * System.currentTimeMillis
   *
   * //  private val input: List[(Char, Char)] =
   * //    Source
   * //      .fromResource(s"day$day.txt")
   * //      .getLines
   * //      .toList
   * //      .map((s: String) => (s(0), s(2)))
   *
   * extension (c: Vector[Int])
   * def -(that: Vector[Int]): Vector[Int] = c.zipWithIndex.map((v: Int, i: Int) => v - that(i))
   * def +(that: Vector[Int]): Vector[Int] = c.zipWithIndex.map((v: Int, i: Int) => v + that(i))
   * def *(i: Int): Vector[Int] = c.map((v: Int) => v * i)
   * def >=(that: Vector[Int]): Boolean = c.zipWithIndex.forall((v: Int, i: Int) => v >= that(i))
   * def <=(that: Vector[Int]): Boolean = c.zipWithIndex.forall((v: Int, i: Int) => v <= that(i))
   *
   * case class MineState(time: Int, wallet: Vector[Int], bots: Vector[Int]):
   * def mine(n: Int): MineState = copy(wallet = wallet + bots * n)
   * def progress: MineState = copy(time = time + 1, wallet = wallet + bots)
   *
   * object MineState:
   *
   * // TODO: fix the off by 1 error 
   * def simulate(current: MineState, costs: Vector[Vector[Int]], await: Int, skip: Int): MineState =
   * println(current)
   * if current.time >= 24 then current
   * else
   * val t = buyBot(current, costs, -1)
   * val n = buyBot(current.mine(await), costs, -1)
   * (t, n) match
   * case (Some(tt), Some(nn)) if nn > tt =>
   * println(s"!!!!!!!gained: ${current.progress.bots}")
   * simulate(current.progress, costs, await, skip)
   * case _ =>
   * val next: MineState = spendAll(current, costs, skip)
   * val nextBot: MineState = current.copy(time = current.time + 1, wallet = next.wallet + current.bots, bots = next.bots)
   * println(s"spend: ${current.wallet - next.wallet}")
   * println(s"gained: ${current.bots}")
   * simulate(nextBot, costs, await, skip)
   *
   *
   * @tailrec
   *  def spendAll(current: MineState, costs: Vector[Vector[Int]], skip: Int): MineState =
   *  buyBot(current, costs, skip) match
   *  case Some(i) => spendAll(
   *  current.copy(wallet = current.wallet - costs(i), bots = current.bots + Vector.fill(4)(0).updated(i, 1)),
   *  costs,
   *  skip
   *  )
   *  case None => current
   *
   *  def buyBot(state: MineState, cost: Vector[Vector[Int]], skipWhen: Int): Option[Int] =
   *  val toSkip: Vector[Int] = if skipWhen < 0 then Vector.empty[Int] else state.bots.zipWithIndex
   *  .filter((b: Int, i: Int) => cost.map((c: Vector[Int]) => c(i)).forall((v: Int) => b >= v + skipWhen))
   *  .map(_._2)
   *  val possibilities = cost
   *  .zipWithIndex
   *  .filterNot((_: Vector[Int], i: Int) => toSkip.contains(i))
   *  .filter((c: Vector[Int], _: Int) => c <= state.wallet)
   *  .map(_._2)
   *  possibilities.lastOption
   *
   *
   *
   *  val cost: Vector[Vector[Int]] = Vector(Vector(4, 0, 0, 0), Vector(2, 0, 0, 0), Vector(3, 14, 0, 0), Vector(2, 0, 7, 0))
   *  val cost2: Vector[Vector[Int]] = Vector(Vector(2, 0, 0, 0), Vector(3, 0, 0, 0), Vector(3, 8, 0, 0), Vector(3, 0, 12, 0))
   *
   *
   *  val grid = for {
   *  i <- 0 to 5
   *  j <- -1 to 5
   *  } yield (i, j)
   *  //  private val res1 = grid.filter((i: Int, j: Int) => (i, j) != (0, -1)).map((i: Int, j: Int) => 
   *  //    val res =  MineState.simulate(MineState(0, Vector(0,0,0,0), Vector(1,0,0,0)), cost2, i, j).wallet(3)
   *  //    s"result is: res=$res, i=$i, j=$j"
   *  //  )
   *  private val res1 = MineState.simulate(MineState(0, Vector(0,0,0,0), Vector(1,0,0,0)), cost2, 0, 4)
   *  println(res1)
   *  private val answer1 = None
   *  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")
   *
   *  private val start2: Long =
   *  System.currentTimeMillis
   *
   *  private val answer2 = None
   *  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
   *
  */

//  def buy(cost: Vector[Vector[Int]]): State[MineState, Vector[Int]] =
//
//    def getOption(cost: Vector[Vector[Int]], s: MineState): Option[Int] =
//      cost
//        .zipWithIndex
//        .filter((c: Vector[Int], _: Int) => c <= s.wallet)
//        .map(_._2)
//        .lastOption
//
//    // TODO: make sure that look forward computes for 0 and 1 always. i.e. make it a parameter to the function buy
//    State {
//      (s: MineState) =>
//        println(s)
//        // i here means the index of the bot to buy
//        val i1: Int = getOption(cost, s).getOrElse(-1)
//        val i2: Int = getOption(cost, s.mine(0)).getOrElse(-1)
//        if i2 > i1 then (s, Vector(0, 0, 0, 0)) // waiting gives the opportunity to buy better bot
//        else if i1 == -1 then (s, Vector(0, 0, 0, 0)) // cannot buy anything so wait
//        else // buy the bot TODO: make sure to buy all possible bots (perhaps...)
//          println(s"Purchased 1 of ${
//            i1 match {
//              case 0 => "ore";
//              case 1 => "clay";
//              case 2 => "obsidian";
//              case 3 => "geode";
//              case _ => "nothing"
//            }
//          } ")
//          val nexts: MineState = s.copy(wallet = s.wallet - cost(i1), bots = s.bots + Vector.fill(4)(0).updated(i1, 1))
//          (nexts, nexts.bots - s.bots)
//    }
