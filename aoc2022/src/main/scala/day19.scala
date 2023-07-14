import scala.io.*
import aoc2022.VectorUtils.*
import aoc2022.GameTree.DecisionTree.*
import aoc2022.GameTree.DecisionTree
import cats.data.State
import scala.collection.immutable.Queue

/**
 * PART 01:
 * 
 * Note that for this solution I took a lot of inspiration from the Reddit answer board.
 * 
 * For this puzzle I tried many different things. First if it was possible to use the State Monad, then a decision
 * tree and finally a queue. It's nice to see that a queue gives a lot of power for making a flat decision tree, 
 * precisely because it can be sorted, filtered, etc... 
 * 
 * Also nice to see that it's easy in Scala to sort Tuples of any size. It saved this solution.
 *
 * PART 02:
 * 
 * Just run part01 but then on the first 3 blueprints and with 32 minutes as input. 
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
          ((0,0,0,ore.toInt),           (0,0,0,1)),  // ore bot
          ((0,0,0,clay.toInt),          (0,0,1,0)),  // clay bot
          ((0,0,ob2.toInt,ob1.toInt),   (0,1,0,0)),  // obsidian bot
          ((0,geo2.toInt,0,geo1.toInt), (1,0,0,0)),  // geode bot
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
      // Ore is the least important. Tuples are easy to sort in Scala hence the tuple. 
      val b: Prod = s.bots
      val w: Wallet = s.wallet + b  // wallet for next minute
      (w._1, w._2, w._3, w._4, b._1, b._2, b._3, b._4)
    
    def doMinute(s: MineState, cost: List[(Cost, Prod)]): List[MineState] =
      val purchases: List[MineState] = cost
        .filter((c: Cost, _: Prod) => c <= s.wallet)  // only get bots that can be bought
        .map((c: Cost, p: Prod) => s.copy(wallet = s.wallet - c + s.bots, bots = s.bots + p, time = s.time + 1))  // add bot and mine with bots in current state s
      purchases
      
    def program(in: List[MineState], time: Int, costs: List[(Cost,Prod)]): MineState =

      // function below performs 1 minute for all MineStates that are in the search space. 
      // Returns the next minestates for the next minute, handling purchases and mining
      def go(q: Queue[MineState], acc: List[MineState]): List[MineState] =
        if q.isEmpty then acc.distinct  // exit, deduplicate all the MineStates for speed. 
        else
          val (n, rem) = q.dequeue
          val next: List[MineState] = doMinute(n, costs)  // determine next MineStates for this minute. 
          // append all next MineState to the accumulator. This will be the Queue in the next minute. 
          go(rem, next.foldLeft(acc)((a: List[MineState], m: MineState) => m :: a))
      
      if time < 1 then in.maxBy(x => x.wallet)  // no minutes left, return max 
      else
        // add all MineStates from the previous minute to a Queue to compute the next minute. Sort 
        // and take only 1000 values to keep the search space small.
        val next: List[MineState] = go(Queue.from(in), Nil).sortBy(x => orderLogic(x)).takeRight(1000)
        program(next, time - 1, costs)
      
  
  private val start: MineState = MineState(0, (0,0,0,0), (0,0,0,1))
  private val allres: List[MineState] = input.map(bluePrint => MineState.program(List(start), 24, bluePrint))
  private val answer1 = allres.map(_.wallet._1).zipWithIndex.map((a, b) => a * (b+1)).sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis
    
  private val res2: List[MineState] = input.take(3).map(bluePrint => MineState.program(List(start), 32, bluePrint))
  private val answer2: Int = res2.map(_.wallet._1).product
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
