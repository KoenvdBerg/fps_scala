import java.util.Date
import scala.annotation.tailrec
import scala.io.*
import scala.util.matching.Regex

/**
 *
 * PART 1:
 *
 * After the following realisations, the solution becomes a lot easier:
 * - you can call the .sorted method on the input file to instantly sort it.
 * - if you put all the guards in a case class named Schedule that tracks their sleeping window, both strategies
 * can be computed.
 *
 * The solution is based on two functions. 1 to compute the guard that was asleep the most overall, and another function
 * to compute the frequencies of the minutes on which any given guard were asleep the most.
 *
 * strategy 1 then becomes the multiplied result of the outputs for both functions.
 *
 * PART 2:
 *
 * The solution was to get all the unique guards, and use the frequency function from part 1 to compute for every guard
 * which minute they were asleep the most. Then take the max amount of times and that is the guard that solves this
 * puzzle.
 *
 */


object day04 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: List[String] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList

  case class Schedule(guard: Int, start: Int, end: Int)

  def makeSleepSchedule(agl: List[String]): List[Schedule] =
    @tailrec
    def go(agll: List[String], s: Int, e: Int, guard: Int, acc: List[Schedule]): List[Schedule] = agll match
      case Nil => acc ::: List(Schedule(guard, s, e))
      case h :: t =>
        if s > -1 & e > -1 then
          go(agll, -1, -1, guard, acc ::: List(Schedule(guard = guard, start = s, end = e)))
        else h match
          case s"${_} Guard #${id} begins shift" => go(t, -1, -1, id.toInt, acc)
          case s"${_}:${s}] falls asleep" => go(t, s.toInt, -1, guard, acc)
          case s"${_}:${e}] wakes up" => go(t, s, e.toInt, guard, acc)
    go(agl, -1, -1, -1, Nil)

  /**
   * Generates a Map of guards and the total amount of time they were asleep. Then takes the maximum
   * total amount of sleep time and returns the guard ID
   */
  def computeBiggestSleeper(scs: List[Schedule]): Int =
    @tailrec
    def go(schedules: List[Schedule], acc: Map[Int, Int]): Map[Int, Int] = schedules match
      case h :: t =>
        val nextAcc: Map[Int, Int] = acc.updated(h.guard, acc(h.guard) + (h.end - h.start))  // add sleeping window to acc
        go(t, nextAcc)  // as long as the schedule list is not empty, continue building the sleep map
      case Nil => acc   // exit condition --> all schedules are processed.

    val start: Map[Int, Int] = scs.map((f: Schedule) => (f.guard, 0)).toMap  // init map for all guards and set sleep time to 0
    go(scs, start).maxBy(_._2)._1  // take the guard that was asleep the most


  /**
   * Computes for any given guard the minute that they were asleep the most. Returns a tuple with
   * the minute and the number of times they were asleep on that minute.
   */
  def computeMinutesAsleep(scs: List[Schedule], guard: Int): (Int, Int) =
    val watches: List[Schedule] = scs.filter((p: Schedule) => p.guard == guard)  // filter for the relevant sleep schedules

    @tailrec
    def go(minute: Int = 0, acc: Map[Int, Int] = Map.empty[Int, Int]): Map[Int, Int] =
      if minute > 59 then acc  // exit condition, done all 59 minutes
      else
        val count: Int = watches.count((p: Schedule) => minute >= p.start && minute < p.end)
        go(minute + 1, acc.updated(minute, count))  // continue with count for minute added to acc
    go().maxBy(_._2)  // take the maximum (minute, times) tuple


  val schedules: List[Schedule] = makeSleepSchedule(input.sorted)
  val sleeper: Int = computeBiggestSleeper(schedules)
  val minuteFound: Int = computeMinutesAsleep(schedules, sleeper)._1

  private val answer1: Int = sleeper * minuteFound
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis


  private val guards: Set[Int] = schedules.map(_.guard).toSet
  // The (Int, (Int, Int)) below looks like this: (guardID, (minute, timesAsleep))
  private val freqs: Map[Int, (Int, Int)] = guards.map((g: Int) => (g, computeMinutesAsleep(schedules, g))).toMap
  private val res2: (Int, (Int, Int)) = freqs.maxBy(_._2._2)
  private val answer2 = res2._1 * res2._2._1
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
