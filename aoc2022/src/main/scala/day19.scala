import scala.io.*
import aoc2022.VectorUtils.*
import aoc2022.GameTree.DecisionTree.*
import aoc2022.GameTree.DecisionTree
import cats.data.State
import scala.collection.immutable.Queue

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

  
  private val input: List[List[(Cost, Prod)]] =
    
    def parse(s: String): List[(Cost, Prod)] = s match
      case s"Blueprint $n: Each ore robot costs $ore ore. Each clay robot costs $clay ore. Each obsidian robot costs $ob1 ore and $ob2 clay. Each geode robot costs $geo1 ore and $geo2 obsidian." => 
        List(
          // ↓ What it costs ↓  |  ↓ What bot it gives ↓
          ((ore.toInt,0,0,0),           (1,0,0,0)),  // ore bot
          ((clay.toInt,0,0,0),          (0,1,0,0)),  // clay bot
          ((ob1.toInt,ob2.toInt,0,0),   (0,0,1,0)),  // obsidian bot
          ((geo1.toInt,0,geo2.toInt,0), (0,0,0,1)),  // geode bot
          ((0,0,0,0),                   (0,0,0,0))   // no bot
        )
      case _ => sys.error(s"cannot parse $s")
    
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList
      .map(parse)

  
  type Wallet = (Int, Int, Int, Int)
  type Prod = Wallet
  type Cost = Wallet
  
  extension (v: Wallet) 
    def <=(that: Wallet): Boolean = v._1 <= that._1 && v._2 <= that._2 && v._3 <= that._3 && v._4 <= that._4
    def -(that: Wallet): Wallet = (v._1 - that._1, v._2 - that._2, v._3 - that._3, v._4 - that._4)
    def +(that: Wallet): Wallet = (v._1 + that._1, v._2 + that._2, v._3 + that._3, v._4 + that._4)
  case class MineState(time: Int, wallet: Wallet, bots: Prod)
    
  object MineState:
    
    def orderLogic(s: MineState): (Int, Int, Int, Int, Int, Int, Int, Int) = 
      // Goal here is to sort by the next wallet state, keeping the bots into mind. Therefore, I opted 
      // for a Tuple8 with the wallet first, and then the bots. Geode in wallet and Geode bots are the most important,
      // Ore is the least important. 
      val b: Prod = s.bots
      val w: Wallet = s.wallet + b
      (w(3), w(2), w(1), w(0), b(3), b(2), b(1), b(0))
    
    def doMinute(s: MineState, cost: List[(Cost, Prod)]): List[MineState] =
      val purchases: List[MineState] = cost
        .filter((c: Cost, _: Prod) => c <= s.wallet)  // only get bots that can be bought
        .map((c: Cost, p: Prod) => s.copy(wallet = s.wallet - c + s.bots, bots = s.bots + p, time = s.time + 1))  // add bot and mine with bots in current state s
      purchases
      
    def program(in: List[MineState], time: Int, costs: List[(Cost,Prod)]): MineState =

      // function below performs 1 minute for all MineStates that are in the search space. 
      // Returns the next minestates for the next minute, handling purchases and mining
      def go(q: Queue[MineState], acc: List[MineState]): List[MineState] =
        if q.isEmpty then acc  // exit
        else
          val (n, rem) = q.dequeue
          val next: List[MineState] = doMinute(n, costs)  // determine next MineStates for this minute. 
          // append all next MineState to the accumulator. This will be the Queue in the next minute. 
          go(rem, next.foldLeft(acc)((a: List[MineState], m: MineState) => m :: a))
      
      if time < 1 then in.maxBy(x => x.wallet._4)  // no minutes left, return max 
      else
        // add all MineStates from the previous minute to a Queue to compute the next minute. Sort 
        // and take only 100 values to keep the search space small.
        val next: List[MineState] = go(Queue(in: _*), Nil).sortBy(x => orderLogic(x)).takeRight(1500)
        program(next, time - 1, costs)
      
  
  private val start: MineState = MineState(0, (0,0,0,0), (1,0,0,0))
  private val allres: List[MineState] = input.map(bluePrint => MineState.program(List(start), 24, bluePrint))
  private val answer1 = allres.map(_.wallet._4).zipWithIndex.map((a, b) => a * (b+1)).sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis
    
  // TODO: fix that part02 also works
  private val res2: List[MineState] = input.take(3).map(bluePrint => MineState.program(List(start), 32, bluePrint))
  println(res2)
  private val answer2: Int = res2.map(_.wallet._4).product
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
