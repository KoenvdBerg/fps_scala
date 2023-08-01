import scala.io.*
import math.*
import scala.annotation.tailrec
import aoc2022.Algorithms.GraphTraversal.*
import aoc2022.FlatGrid

/**
 * PART 01:
 * 
 * This puzzle was a suitable candidate for my earlier implemented Dijkstra algorithm. I made a function that 
 * computes the next nodes (type Graph[Int]) based on the index in the map Vector[Char]. 
 * 
 * The earlier implemented shortestDistance function was to the rescue here. 
 *
 * PART 02:
 * 
 * I took all the possible starting positions and then brute forced the answer using the same graph from part 01. 
 *
 */


object day12 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: Vector[String] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector

  def mountainGraph(map: Vector[Char], width: Int): Graph[Int] = 
    (i: Int) =>
      val currentHeight: Int = map(i).toInt
      val neighbours = FlatGrid.neighbours4(i, width, map.length)
      if neighbours.isEmpty then Map.empty
      else 
        val next: Vector[(Int, Int)] = neighbours
          .filter((n: Int) => math.abs(map(n).toInt) - currentHeight <= 1)
          .map((n: Int) => n -> 1)
        next.toMap
        
  private val mountainMap: Vector[Char] = input.flatMap(_.toVector)
  private val start: Int = mountainMap.indexWhere(_ == 'S')
  private val target: Int = mountainMap.indexWhere(_ == 'E')
  private val correctedMap: Vector[Char] = mountainMap.updated(start, 'a').updated(target, 'z')
  private val graph: Graph[Int] = mountainGraph(correctedMap, input.head.length)
  private val answer1: Int = shortestDistance(graph)(start, target).get
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis
    
  private val allStarts: Vector[Int] = correctedMap
    .zipWithIndex
    .filter(_._1 == 'a')
    .map(_._2)
  private val res2: Vector[Int] = allStarts.flatMap((s: Int) => shortestDistance(graph)(s, target))
  private val answer2: Int = res2.min
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
