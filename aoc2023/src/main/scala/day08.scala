import scala.io.*
import math.*
import scala.annotation.tailrec
import aoc2022.SequenceUtils.CircularQueue

object day08 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis
    
  case class PathWay(l: String, r: String)

  private val (instructions, waypoints): (CircularQueue[Char], Map[String, PathWay]) =
    
    def parsePaths(s: String): Option[(String, PathWay)] = s match
      case s"$from = ($l, $r)" => Some((from, PathWay(l, r)))
      case _ => None
      
    val in = Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList
    
    val waypoints: Map[String, PathWay] = in.flatMap(parsePaths).toMap
    val instructions: CircularQueue[Char] = CircularQueue(in.head.toVector)
    (instructions, waypoints)
    
  def step(cur: String, ins: Char): String = ins match
    case 'L' => waypoints(cur).l
    case 'R' => waypoints(cur).r
    case _ => sys.error(s"cannot do $cur and $ins")

  def walk(ins: CircularQueue[Char], start: String): Long =

    @tailrec
    def go(cur: String, count: Long): Long =
      val instruction: Char = ins.dequeue
      val next: String = step(cur, instruction)
      if next.endsWith("Z") then count
      else go(next, count + 1)
  
    go(start, 1)
  
  private val answer1 = walk(instructions, "AAA")
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  def overlap(in: Long, that: Long): Long =

    @tailrec
    def go(i: Int): Long =
      val t = in * i
      if t % that == 0 then i
      else go(i + 1)
    
    go(1) * in

  private val start2: Long =
    System.currentTimeMillis
    
  private val starts: Vector[String] = waypoints.keys.filter(_.endsWith("A")).toVector
  private val res2: Vector[Long] = starts.map(s => walk(instructions, s))
  private val answer2 = res2.foldLeft(1L) { (res: Long, in: Long) => overlap(res, in) }
  println(s"Answer day $day part 1: ${answer2} [${System.currentTimeMillis - start2}ms]")
