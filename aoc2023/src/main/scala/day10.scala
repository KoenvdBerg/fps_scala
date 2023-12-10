import scala.io.*
import math.*
import scala.annotation.tailrec
import aoc2022.Algorithms.GraphTraversal.{Graph, shortestPath}
import aoc2022.Algorithms.GraphTraversal
import aoc2022.Algorithms.floodAlgorithm
import aoc2022.FlatGrid

object day10 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val (rowL, pipes): (Int, Vector[Char]) =
    val input = Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector

    (input.head.length, input.mkString.toVector)

  def determineS(si: Int, pipes: Vector[Char], rowSize: Int): Char =
    val Vector(right, left, up, down) = FlatGrid.neighbours4(si, rowSize, pipes.length).map(pipes)
    ("|JL".contains(down), "|F7".contains(up), "7-J".contains(right), "L-F".contains(left)) match
      case (true, true, false, false) => '|'
      case (true, false, true, false) => 'F'
      case (true, false, false, true) => '7'
      case (false, true, true, false) => 'L'
      case (false, true, false, true) => 'J'
      case (false, false, true, true) => '-'

  def walk(pipeField: Vector[Char], rowSize: Int, tiles: Int): Graph[Int] =
    (cur: Int) =>
      val neighbours: Vector[Int] = FlatGrid.neighbours4(cur, rowSize, tiles)
      val next: Vector[Int] = pipeField(cur) match
        case '|' => neighbours.filter(i => i == cur+rowSize | i == cur-rowSize)
        case '-' => neighbours.filter(i => i == cur+1 | i == cur-1)
        case 'L' => neighbours.filter(i => i == cur+1 | i == cur-rowSize)
        case 'J' => neighbours.filter(i => i == cur-1 | i == cur-rowSize)
        case '7' => neighbours.filter(i => i == cur-1 | i == cur+rowSize)
        case 'F' => neighbours.filter(i => i == cur+1 | i == cur+rowSize)
        case '.' | 'S' => sys.error("cannot have ground in pipeLoop")
      next.map(i => (i, 1)).toMap

  private val start: Int = pipes.indexOf('S')
  private val newPipes: Vector[Char] = pipes.updated(start, determineS(start, pipes, rowL))
  private val res1: (Map[Int, Int], Map[Int, Int]) = GraphTraversal.dijkstra(walk(newPipes, rowL, newPipes.length))(start)
  private val answer1: Int = res1._1.maxBy(_._2)._2
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  def scanRight(slice: Vector[Char]): Int =

    @tailrec
    def go(i: Int, count: Int, mode: Int): Int =
      if i >= slice.length then count
      else
        val t: Char = slice(i)
        (mode, t) match
          case (0, '|') => go(i + 1, count + 1, 0)
          case (0, 'F') => go(i + 1, count, 1)
          case (0, 'L') => go(i + 1, count, 2)
          case (0, _)   => go(i + 1, count, 0)
          case (1, 'J' | '|' | 'L') => go(i + 1, count + 1, mode)
          case (1, _) => go(i + 1, count, mode)
          case (2, 'F' | '|' | '7') => go(i + 1, count + 1, mode)
          case (2, _) => go(i + 1, count, mode)

    go(0, 0, 0)

  def isInside(cur: Int, pipeMap: Set[Int], rowL: Int, pipes: Vector[Char]): Boolean =

    val y: Int = cur / rowL
    if cur % rowL == 0 then false
    else
      val slice: Vector[Int] = pipes.indices.slice(cur, rowL * (y + 1)).toVector.filter(pipeMap.contains)
      val c: Int = scanRight(slice.map(pipes))
      // https://en.wikipedia.org/wiki/Point_in_polygon#Ray_casting_algorithm
      c % 2 == 1


  private val start2: Long =
    System.currentTimeMillis

  private val pipesIndices: Set[Int] = res1._2.keySet + start
  private val toScan: Vector[Int] = newPipes.indices.toSet.diff(pipesIndices).toVector
  private val inside: Vector[Boolean] = toScan.map(i => isInside(i, pipesIndices, rowL, newPipes))
  private val answer2: Int = inside.count(identity)
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")