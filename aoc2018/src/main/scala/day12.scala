import scala.io.*
import math.*
import scala.collection.mutable
import scala.collection.mutable.Stack

/**
 *
 * PART 1:
 *
 * The difficult part of this puzzle was in my opinion that you had to take the numbering of the pots into account. This
 * was difficult, because the plants could obtain negative numbers towards the left side of the plant row. To determine
 * the correct numbering, I computed the offset towards the end of the simulation by making sure that each generation
 * 1 pots was added, and then finding the first plant and substracting from the number of generations. Example:
 *
 *                  1         2         3
 *        0         0         0         0
 * 20: .#....##....#####...#######....#.#..##
 *
 * In the example above (from the official example in the puzzle description) you can see that there's 1 plant at location
 * -2, which is the starting point for computing the score. To compute this, see example below for generation 20:
 *
 * ..................#....##....#####...#######....#.#..##..........
 * ^^^^^^^^^^^^^^^^^^^^0
 *
 * Since each generation a dot '.' gets added to the plant row state (indicated by `^`), and currently we find
 * 18 dots before the first plant, the left offset here is:
 *
 * offset = 20 - 18 = 2
 *
 * So the first plant is at location: -2
 *
 * PART 2:
 *
 * The idea here is that since the amount of generations is way too large to compute by brute force, we'll have to be
 * smart about our solution. I found that after a certain amount of generations the increase in score becomes a
 * linear function in the form of: y = ax + b. See below in the code how I solved that. Your input will probably lead
 * to different values, but the idea on how to solve should be similar.
 *
 *
 */

object day12 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: Generation =

    val stateParser: String => Option[String] = {
      case s"initial state: ${state}" => Some(state)
      case _ => None
    }

    val growthParser: String => Option[Growth] = {
      case s"${current} => ${next}" => Some(Growth(current, next.head))
      case _ => None
    }

    val infile = Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList

    Generation(
      n = 0,
      state = infile.flatMap(stateParser).head,
      growth = infile.flatMap(growthParser)
    )

  case class Growth(current: String, next: Char)
  case class Generation(n: Int, state: String, growth: List[Growth]):

    def updatePlant(plant: String): Char =
      val selection = this.growth.filter(_.current == plant)
      if selection.isEmpty then
        '.'
      else
        selection.head.next  // taking the next state according to growth schedule

    def computeOffset: (Int, Int) =
      // since each generation 1 pot gets added, offset can be computed by finding the amount of preceding empty
      // pots subtracted from the current generation.
      val offset: String => Int = (s: String) => s.takeWhile(_ == '.').length
      val leftOffset = offset(this.state)
      val rightOffset = offset(this.state.reverse)
      (this.n - leftOffset, this.n - rightOffset)

    def computeScore: Int =
      val (leftOff, _): (Int, Int) = this.computeOffset
      this
        .state.dropWhile(_ == '.')  // stripping preceding empty pots
        .zipWithIndex               // including index for calculation
        .filter(f => f._1 == '#')   // only taking plants
        .map(_._2 - leftOff)        // subtracting the left offset
        .sum


    def simulatePlants(lookSize: Int, maxGen: Int = 20): Generation =
      if this.n >= maxGen then this  // exit condition
      else

        // add 1 extra pot to each side each new generation
        val plantRow: String = "." * (lookSize+1) + this.state + "." * (lookSize+1)

        // create slices of 5 plants each, appending empty pots to the first and last plants in the row
        val plantCouplets: List[String] =
          Range(lookSize, plantRow.length-lookSize)
            .map(i => plantRow.slice(i-lookSize, i+lookSize+1))
            .toList

        val nextState: String =
          plantCouplets
            .map(plant => updatePlant(plant))
            .mkString("")

        val nextGeneration = Generation(this.n + 1, nextState, this.growth)

        nextGeneration.simulatePlants(lookSize,  maxGen)


  private val finalGeneration: Generation = input.simulatePlants(lookSize = 2, maxGen = 21)
  private val answer1 = finalGeneration.computeScore
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  /**
   *
   * I found that after 161 generations the increase in plant score doesn't change anymore.
   * Each new generation gets +73. I found this difference in the REPL with:
   *
   *  x.sliding(2).map(p => p(1) - p(0)).toList
   *
   * where x are plant scores for each generation up to 300 generations. I discovered in
   * the REPL that the generation where the increase goes stale is 161, with value 12130.
   * The stale increase is +73.
   *
   */

  private val staleGen: Int = 161
  private val staleValue: Int = 12130
  private val staleIncr: Int = 73
  private val answer2 = (50000000000L - staleGen) * staleIncr + staleValue
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
