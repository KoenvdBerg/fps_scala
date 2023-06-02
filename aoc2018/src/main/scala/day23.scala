import scala.io.*
import aoc2018.Grid2D.Point
import scala.util.Random

object day23 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  case class Bot(x: Int, y: Int, z: Int, r: Int):

    def distance(to: Bot): Int =
      math.abs(x - to.x) + math.abs(y - to.y) + math.abs(z - to.z)

    def adjacent(step: Int): Vector[Bot] =
      Vector(
        this.copy(x = x + step),
        this.copy(x = x - step),
        this.copy(y = y + step),
        this.copy(y = y - step),
        this.copy(z = z - step),
        this.copy(z = z - step),
        this.copy(x = x + step, y = y + step),
        this.copy(x = x + step, y = y - step),
        this.copy(x = x - step, y = y + step),
        this.copy(x = x - step, y = y - step),
        this.copy(z = z + step, y = y + step),
        this.copy(z = z + step, y = y - step),
        this.copy(z = z - step, y = y + step),
        this.copy(z = z - step, y = y - step),
        this.copy(z = z + step, x = x + step),
        this.copy(z = z + step, x = x - step),
        this.copy(z = z - step, x = x + step),
        this.copy(z = z - step, x = x - step),
        this.copy(x = x + step, y = y + step, z = z + step),
        this.copy(x = x + step, y = y + step, z = z - step),
        this.copy(x = x + step, y = y - step, z = z + step),
        this.copy(x = x + step, y = y - step, z = z - step),
        this.copy(x = x - step, y = y + step, z = z + step),
        this.copy(x = x - step, y = y + step, z = z - step),
        this.copy(x = x - step, y = y - step, z = z + step),
        this.copy(x = x - step, y = y - step, z = z - step),
      )

  private val input: Vector[Bot] =

    def parseBot(s: String): Bot = s match
      case s"pos=<$x,$y,$z>, r=$r" => Bot(x.toInt, y.toInt, z.toInt, r.toInt)
      case _                       => sys.error("BOOM")

    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector
      .map(parseBot)


  val biggestRad: Bot = input.maxBy((b: Bot) => b.r)

  val inRange: Vector[Int] = input
    .map((b: Bot) => b.distance(biggestRad))
    .filter((d: Int) => d <= biggestRad.r)

  private val answer1 = inRange.length
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis


  def scorePos(pos: Bot, bots: Vector[Bot]): Int =
    bots.count((b: Bot) => b.distance(pos) <= b.r)

//  def edgeSearch(bots: Vector[Bot]): Vector[Int] =
//    bots
//      .flatMap((b: Bot) => {val d: Int = b.distance(Bot(0,0,0,0)) ; Vector((d - b.r).max(0), d + b.r)})

  def lineSearch(bots: Vector[Bot], start: Bot, initStep: Int): Bot =

    def go(curBot: Bot, step: Int): Bot =
      val curScore: Int = scorePos(curBot, bots)
      val nextBots: Vector[Bot] = curBot.adjacent(step)
      val scores: Vector[(Bot, Int)] = nextBots.map((b: Bot) => (b, scorePos(b, bots)))
      val cont: Vector[(Bot, Int)] = scores.filter(_._2 > curScore)
      if cont.isEmpty && step == 1 then curBot
      else if cont.isEmpty then go(curBot, step / 2)
      else go(cont.maxBy(_._2)._1, step)

    go(start, initStep)

  def randomLineSearch(bots: Vector[Bot], times: Int): Vector[(Bot, Int)] =
    val rand: Random = new Random
    //val xAvg: Int = input.map(_.x).sum / input.length
    //val yAvg: Int = input.map(_.y).sum / input.length
    //val zAvg: Int = input.map(_.z).sum / input.length
    val x: Vector[Int] = input.map(_.x).sorted
    val y: Vector[Int] = input.map(_.y).sorted
    val z: Vector[Int] = input.map(_.z).sorted
    val xAvg: Int = math.abs(x(input.length / 2))
    val yAvg: Int = y(input.length / 2).max(1)
    val zAvg: Int = z(input.length / 2).max(1)
    println(xAvg)
    println(zAvg)
    println(yAvg)

    def loop(n: Int, acc: Vector[(Bot, Int)]): Vector[(Bot, Int)] =
      if n <= 0 then acc
      else
        val randomStart: Bot = Bot(-rand.nextInt(xAvg), rand.nextInt(yAvg), rand.nextInt(zAvg), 0)
        val randomResult: Bot = lineSearch(input, randomStart, initStep = rand.nextInt(zAvg).max(1))
        loop(n - 1, (randomResult, scorePos(randomResult, bots)) +: acc)

    loop(times, Vector.empty[(Bot, Int)])


  val res2 = randomLineSearch(input, 200)
  val maxInRange: Int = res2.maxBy(_._2)._2
  private val answer2 = res2
    .filter(_._2 == maxInRange)
    .map((f: (Bot, Int)) => (f._1, f._1.distance(Bot(0, 0, 0, 0))))
    .minBy(_._2)
    println(s"Answer day $day part 2: ${answer2._1} with in range: $maxInRange and distance: ${answer2._2} [${System.currentTimeMillis - start2}ms]")


//  val x = edgeSearch(input).groupBy(identity).map((i: (Int, Vector[Int])) => (i._1, i._2.length))
//  println(x)
//  println(edgeSearch(input).groupBy(identity).map((i: (Int, Vector[Int])) => (i._1, i._2.length)).maxBy(_._2))



//  private val answer2 = None
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")


// max: 919
// too high: 51429377
// too high: 51486118

// Answer day 23 part 2: Bot(17995929,19759667,15178554,0) with in range: 912 and distance: 52934150 [212ms]
// Answer day 23 part 2: Bot(18395395,18364865,15326285,0) with in range: 919 and distance: 52086545 [355ms]

//
