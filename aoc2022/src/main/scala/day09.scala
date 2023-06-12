import scala.io.*
import math.*
import scala.annotation.tailrec

/**
 * PART 01:
 * 
 * manhattan distance between H and T > 1 then T needs to move
 * 
 * if H and T are not in the same column or row, then T joins H column if H goes upwards, or if H goes sideways, then 
 * T joins H row. 
 *
 * PART 02:
 *
 */


object day09 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: List[(Char, Int)] =
    
    def parseMove(s: String): (Char, Int) =
      val parts = s.split(" ")
      (parts.head.head, parts.last.toInt)
    
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList
      .map(parseMove)

  case class Point(x: Int, y: Int):
    def manhattan(that: Point): Int = math.abs(x - that.x).max(math.abs(y - that.y))
    def disjoint(that: Point): Boolean = x != that.x && y != that.y
    def +(that: Point): Point = Point(x + that.x, y + that.y)
    
  case class RopeMovement(H: Point, T: List[Point], seen: Set[Point]):
    
//    def moveRope(xDir: Int, yDir: Int): RopeMovement =
//      val newH: Point = Point(H.x + xDir, H.y + yDir)
//      val newT: List[Point] = T.scanLeft(newH)((thisH: Point, thisT: Point) =>
//        if thisT.manhattan(thisH) <= 1 then thisT 
//        else if thisT.disjoint(thisH) then Point(cT.x + dir, prev.y)
//        else Point(cT.x + dir, cT.y))

    // TODO: fix the movement of the Ts in the T List
    // TODO: if disjoint, then pattern match or ifelse on x and y coordinates to find out where to move next
    def moveX(dir: Int): RopeMovement =
      val newH: Point = Point(H.x + dir, H.y)
      val newTs: List[Point] = T.scanLeft(newH)((prev: Point, cT: Point) =>
        if cT.manhattan(prev) <= 1 then cT else if cT.disjoint(prev) then Point(cT.x + dir, prev.y)
        else Point(cT.x + dir, cT.y))
      RopeMovement(newH, newTs.drop(1), seen + newTs.last)

    def moveY(dir: Int): RopeMovement =
      val newH: Point = Point(H.x, H.y + dir)
      val newTs: List[Point] = T.scanLeft(newH)((prev: Point, cT: Point) =>
        if cT.manhattan(prev) <= 1 then cT else if cT.disjoint(prev) then Point(prev.x, cT.y + dir)
        else Point(cT.x, cT.y + dir))
      RopeMovement(newH, newTs.drop(1), seen + newTs.last)

  def simulate(rm: RopeMovement, move: (Char, Int)): RopeMovement =
    println(rm)
    
    def doMove(acc: RopeMovement, n: Int): RopeMovement =
      println(acc)
      if n <= 0 then acc
      else 
        move._1 match
          case 'R' => doMove(acc.moveX(1), n - 1)
          case 'L' => doMove(acc.moveX(-1), n - 1)
          case 'U' => doMove(acc.moveY(1), n - 1)
          case 'D' => doMove(acc.moveY(-1), n - 1)
    doMove(rm, move._2)
    
  val res1: RopeMovement = input.take(2).foldLeft(RopeMovement(Point(0, 0), List.fill(9)(Point(0,0)), Set.empty))(simulate)
  println(res1)
  private val answer1 = res1.seen.size
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  val answer2 = None
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")

//case class RopeMovement(H: Point, T: Point, seen: Set[Point]):
//  def moveX(dir: Int): RopeMovement =
//    val newH: Point = Point(H.x + dir, H.y)
//    val newT: Point = if newH.manhattan(T) <= 1 then T else if T.disjoint(H) then Point(T.x + dir, H.y)
//    else Point(T.x + dir, T.y)
//    RopeMovement(newH, newT, seen + newT)
//
//  def moveY(dir: Int): RopeMovement =
//    val newH: Point = Point(H.x, H.y + dir)
//    val newT: Point = if newH.manhattan(T) <= 1 then T else if T.disjoint(H) then Point(H.x, T.y + dir)
//    else Point(T.x, T.y + dir)
//    RopeMovement(newH, newT, seen + newT)