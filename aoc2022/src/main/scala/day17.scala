import scala.io.*
import math.*
import scala.annotation.tailrec
import aoc2022.Grid2D.Point
import aoc2022.VectorUtils.rotateVector

/**
 * PART 01:
 *
 * Difficult part for me was to get the rocks right. Eventually I settled for an enum with the x and y coordinates 
 * as input to generate a rock. Then I only had to compute how the start x and y coordinates changed when the rock
 * was moved by either a jet or gravity. 
 * 
 * Each new rock starts at x = 2 and yMax. The yMax is tracked by seeing if the newly settled rock is higher than the 
 * previous yMax. 
 * 
 * The solution is running the simulation for 2022 blocks and then returning the yMax. 
 *
 * PART 02:
 *
 * Solution is explained in more detail below. Basically found a repetition and exploited that. 
 *
 */


object day17 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: Vector[Char] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector
      .head
      .toVector
    
  enum Rock(coords: (Int, Int) => Vector[Point], w: Int, h: Int):
    
    def blow(dir: Char, x: Int, y: Int, obstacles: List[Point]): Int = dir match
      case '>' => if (x + w - 1) < 6 && !coords(x+1,y).exists((p: Point) => obstacles.contains(p)) then x + 1 else x 
      case '<' => if x > 0 && !coords(x-1,y).exists((p: Point) => obstacles.contains(p)) then x - 1 else x 
      
    def down(x: Int, y: Int, obstacles: List[Point]): Int =
      if y <= 1 || coords(x, y-1).exists((p: Point) => obstacles.contains(p)) then y
      else y-1
      
    def loc(x: Int, y: Int): Vector[Point] = coords(x, y)
    
    def height: Int = h
    
    case Min extends Rock((x: Int, y: Int) => Vector(Point(x, y), Point(x+1, y), Point(x+2, y), Point(x+3, y)), 4, 1)
    case Plus extends Rock((x: Int, y: Int) => Vector(Point(x, y+1), Point(x+1, y), Point(x+1, y+1), Point(x+1, y+2), Point(x+2, y+1)), 3, 3)
    case L extends Rock((x: Int, y: Int) => Vector(Point(x, y), Point(x+1, y), Point(x+2, y), Point(x+2, y+1), Point(x+2, y+2)), 3, 3)
    case Pipe extends Rock((x: Int, y: Int) => Vector(Point(x, y), Point(x, y+1), Point(x, y+2), Point(x, y+3)), 1, 4)
    case Block extends Rock((x: Int, y: Int) => Vector(Point(x, y), Point(x, y+1), Point(x+1, y), Point(x+1, y+1)), 2, 2)

  object Rock:
    val rocks: Vector[Rock] = Vector(Min, Plus, L, Pipe, Block)
    val nRocks: Int = rocks.length
    val jets: Vector[Char] = input
    val nJets: Int = jets.length
    
    def simulate(yMax: Int, ij: (Int, Int), obstacles: List[Point], n: Int): Int =
      
      def go(r: Rock, x: Int, y: Int, j: Int): (Int, Int, Int) =
        val xs: Int = r.blow(jets(j), x, y, obstacles)
        val ys: Int = r.down(xs, y, obstacles)
        if y == ys then (xs, y, j) else go(r, xs, ys, (j + 1) % nJets)
      
      // enable println below to see repetition:
      //if ij._2 == 3 then println(s"$yMax and $n")
      
      if n <= 0 then yMax
      else
        val thisRock: Rock = rocks(ij._1)
        val (xf, yf, jf): (Int, Int, Int) = go(thisRock, 2, yMax+4, ij._2)
        val settledRock: Vector[Point] = thisRock.loc(xf, yf)
        val nextMaxY: Int = if (yf + thisRock.height - 1) > yMax then yf + thisRock.height - 1 else yMax
        
        // Limit the obstacles List to max 100 rocks, because the rocks below that don't influence next rocks. This 
        // increases program speed and memory efficiency. 
        val nextObs: List[Point] = settledRock.foldLeft(obstacles)((b, a) => a :: b).take(100)  
        simulate(nextMaxY, ((ij._1 + 1) % nRocks, (jf + 1) % nJets), nextObs, n - 1)
  
  private val answer1: Int = Rock.simulate(0, (0, 0), List.empty[Point], 2022)
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")
  
  private val start2: Long =
    System.currentTimeMillis

  /**
   * 
   * After 1 iteration through the jets, the next iterations start a loop with an increase in height of 2768 each 
   * 1755 blocks. This can be found by seeing that the block index i and the jet index j repeat at i=0 and j=3 
   * (in my case). Thus, the start n (before this repeated loop starts) is 1752 with an height of 2729. 
   * 
   * The remainder after running the repeated loop is (1000000000000L - 1752) % 1755 = 1408. 
   * The height of 1408 blocks in the repeated loop is 2233 (can be computed with function above: 
   * score = sim(n=1752 + 1408) - 2729). 
   * 
   * Thus the final score can be computed with the formula below:
   */

  private val answer2: Long = (1000000000000L - 1752) / 1755 * 2768 + 2729 + 2233
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
