import scala.io.*
import math.*
import scala.collection.mutable
import scala.collection.mutable.Stack

/**
 * PART 1:
 *
 * The general process for solving this puzzle was the following:
 *
 *  1. read in the input.txt file and parse it to a Star case class
 *  2. create a function that updates a star based on its velocity on the x and y axes
 *  3. create a window with a variable width and height. This window has the following specs:
 *    a. it can be nicely printed to stdout.
 *    b. it checks whether or not any star enters the window. If so, the star coordinate is represented by #.
 *       All other coordinates are represented by ".".
 *  4. create an algorithm that updates the night sky and only prints if any star is present in the focus window.
 *     for every iteration, do:
 *     a. update every star.
 *     b. if no star has entered the focus window, keep on loading and reiterating.
 *     c. if any star is in the window, print time and the window.
 *
 *  Using the above code, I could see the stars forming the hidden message, and halt the code at the appropriate time.
 *  Then I just needed to check the message by scrolling through the output in the terminal.
 *
 */


object day10 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: Vector[Star] =

    val parser: String => Star = {
      case s"position=<${x}, ${y}> velocity=<${vx}, ${vy}>" => Star(Point(x.strip().toInt, y.strip().toInt),
                                                                    vx.strip().toInt, vy.strip().toInt)
      case _ => sys.error("boom")
    }

    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector
      .map(parser)

  case class Point(x: Int, y: Int)
  case class Star(p: Point, vx: Int, vy: Int):

    def updateCoords(): Star =
      Star(Point(this.p.x + this.vx, this.p.y + this.vy), this.vx, this.vy)

  object Sky:

    private val width: Int = 250
    private val height: Int = 250
    private val window: Vector[Point] =
      (for {
        x <- Range(0, width)
        y <- Range(0, height)
      } yield Point(y, x)).toVector

    private def printWindow(starPoints: Vector[Point]): Unit =
      def aligner(z: String, p: Point): String =
        if starPoints.contains(p) then
          z + "#"
        else
          z + "."

      def skyToStdOut(s: String): Unit =
        if s == "" then println()
        else
          val (h, t) = s.splitAt(width)
          println(h)
          skyToStdOut(t)

      val sky: String = window.foldLeft("")(aligner)
      skyToStdOut(sky)

    def alignStarsInSky(s: Vector[Star], loading: Boolean = true, t: Int = 0): Unit =
      val next: Vector[Star] = s.map(_.updateCoords())

      // stars have reached the window. So now it starts printing to stdout
      if math.abs(s.head.p.x) < width && math.abs(s.head.p.y) < width then
        val starPoints: Vector[Point] = s.map(_.p)
        println(t)
        printWindow(starPoints)
        alignStarsInSky(next, false, t + 1)

      // stars have not reached the window yet, so keep on loading and updating the nights sky
      else if loading then
        println("LOADING...")
        alignStarsInSky(next, true, t + 1)

      // exit condition --> loading has been finished and all stars have left the window
      else
        println()

  Sky.alignStarsInSky(input)
  private val answer1 = "HJBJXRAZ"
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  private val answer2 = "10641"
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
