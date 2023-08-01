import scala.io.*
import math.*
import scala.annotation.tailrec
import scala.collection.mutable
import aoc2022.FlatGrid
import aoc2022.VectorUtils.swap
import aoc2022.Grid2D.Point
import aoc2022.Algorithms.GraphTraversal.*
import day26.Amphipod

/**
 * PART 01:
 *
 * Easy foldRight
 *
 * PART 02:
 *
 * Sort and then take 3 and sum
 *
 */


object day26 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val (width: Int, input): (Int, Vector[Char]) =
    val infile = Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector

    (infile.head.length, infile.flatMap(_.toVector))
    
  object Amphipod: 
  
    // todo: for heuristic add option to ignore other amphipods perhaps...
    def distanceGraph(ground: Vector[Char], width: Int): Graph[Int] =
      (i: Int) =>
        val neighbours: Vector[Int] = FlatGrid.neighbours4(i, width, ground.length)
        if neighbours.isEmpty then Map.empty
        else
          neighbours
            .filter((n: Int) => ground(n) == '.')
            .map((f: Int) => f -> 1)
            .toMap
         
    val hall: Vector[Int] = Vector(14, 15, 17, 19, 21, 23, 24) 
    val room: Map[Char, Vector[Int]] = Map(
      'A' -> Vector(29, 42), 
      'B' -> Vector(31, 44), 
      'C' -> Vector(33, 46), 
      'D' -> Vector(35, 48))
    val weights: Map[Char, Int] = Map(
      'A' -> 1,
      'B' -> 10, 
      'C' -> 100,
      'D' -> 1000)
    val fin: Vector[Char] = Vector('#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '.', '.', '.', 
      '.', '.', '.', '.', '.', '.', '.', '.', '#', '#', '#', '#', 'A', '#', 'B', '#', 'C', '#', 'D', '#', '#', '#', ' ',
      ' ', '#', 'A', '#', 'B', '#', 'C', '#', 'D', '#', ' ', ' ', ' ', ' ', '#', '#', '#', '#', '#', '#', '#', '#', '#',
      ' ', ' ')
    
    def amphipodGraph(width: Int): Graph[Vector[Char]] =
      (ground: Vector[Char]) =>
        // hallway into room
        val inHallway: Vector[Int] = hall.filter((i: Int) => ground(i) != '.')
        val hallwayMoves: Map[Vector[Char], Int] =
          if inHallway.isEmpty then Map.empty
          else 
            val ts: Vector[(Vector[Char], Int)] = for {
              i    <- inHallway
              dest <- room(ground(i))
                .filter((f: Int) => ground(f) == '.')
                .lastOption
              dist = shortestDistance(distanceGraph(ground, width))(i, dest)
              if dist != None
            } yield (swap(ground, i, dest), dist.get * weights(ground(i)))
            ts.toMap
        
        // room into hallway
        val inPlace: Vector[Int] = room
          .toVector
          .flatMap((c: Char, ds: Vector[Int]) => ds.reverse.takeWhile((f: Int) => ground(f) == c))  // these amphipods are in the correct place already
        val inRoom: Vector[Int] = (for {
          r    <- room.values
          todo <- r.find((i: Int) => ground(i) != '.')
          if !inPlace.contains(todo)  // don't move the amphipods from their correct place
        } yield todo).toVector
        val roomMoves: Map[Vector[Char], Int] =
          if inRoom.isEmpty then Map.empty
          else 
            val rs = for {
              i    <- inRoom
              dest <- hall
              dist = shortestDistance(distanceGraph(ground, width))(i, dest)
              if dist != None
            } yield (swap(ground, i, dest), dist.get * weights(ground(i)))
            rs.toMap
        roomMoves ++ hallwayMoves
  
  end Amphipod

  // For animation of result uncomment code below
  // private val res1: List[Vector[Char]] = shortestPath(Amphipod.amphipodGraph(width))(input, Amphipod.fin).get
  // res1.foreach(f =>
  //   Thread.sleep(500)
  //   print("\u001b[2J")
  //   println(FlatGrid.printFlatGrid(f, width)(identity))
  // )

  // todo: part02
  // todo: finish part01 by adding a heuristic
  
  private val answer1 = shortestDistance(Amphipod.amphipodGraph(width))(input, Amphipod.fin)
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis
    
  private val answer2 = None
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
