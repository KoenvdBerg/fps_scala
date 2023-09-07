import scala.io.*
import aoc2021.FlatGrid
import aoc2021.VectorUtils.swap
import aoc2021.Algorithms.GraphTraversal.*

/**
 * PART 01:
 *
 * Part 1 uses Dijkstra. It's not the fastest implementation, but gets the job done in about 5s. 
 *
 * PART 02:
 *
 * Uses A*. It gets the job done in about 50s. This could be greatly improved using a better heuristic. 
 *
 */


object day23 extends App:

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

    def distanceGraph(ground: Vector[Char], width: Int, ignore: Boolean = false): Graph[Int] =
      (i: Int) =>
        val neighbours: Vector[Int] = FlatGrid.neighbours4(i, width, ground.length)
        if neighbours.isEmpty then Map.empty
        else
          if ignore then
            neighbours
              .filter((n: Int) => ground(n) != '#')
              .map((f: Int) => f -> 1)
              .toMap
          else
            neighbours
              .filter((n: Int) => ground(n) == '.')
              .map((f: Int) => f -> 1)
              .toMap

    private val hall: Vector[Int] = Vector(14, 15, 17, 19, 21, 23, 24)

    private val room: Map[String, Map[Char, Vector[Int]]] = Map(
      "part01" -> Map(
        'A' -> Vector(29, 42),
        'B' -> Vector(31, 44),
        'C' -> Vector(33, 46),
        'D' -> Vector(35, 48)),
      "part02" -> Map(
        'A' -> Vector(29, 42, 55, 68),
        'B' -> Vector(31, 44, 57, 70),
        'C' -> Vector(33, 46, 59, 72),
        'D' -> Vector(35, 48, 61, 74)))

    private val weights: Map[Char, Int] = Map(
      'A' -> 1,
      'B' -> 10,
      'C' -> 100,
      'D' -> 1000)

    val fin: Vector[Char] = Vector('#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '.', '.', '.',
      '.', '.', '.', '.', '.', '.', '.', '.', '#', '#', '#', '#', 'A', '#', 'B', '#', 'C', '#', 'D', '#', '#', '#', ' ',
      ' ', '#', 'A', '#', 'B', '#', 'C', '#', 'D', '#', ' ', ' ', ' ', ' ', '#', '#', '#', '#', '#', '#', '#', '#', '#',
      ' ', ' ')

    val fin2: Vector[Char] = Vector( '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '.', '.',
      '.', '.', '.', '.', '.', '.', '.', '.', '.', '#', '#', '#', '#', 'A', '#', 'B', '#', 'C', '#', 'D', '#', '#', '#',
      ' ', ' ', '#', 'A', '#', 'B', '#', 'C', '#', 'D', '#', ' ', ' ', ' ', ' ', '#', 'A', '#', 'B', '#', 'C', '#', 'D',
      '#', ' ', ' ', ' ', ' ', '#', 'A', '#', 'B', '#', 'C', '#', 'D', '#', ' ', ' ', ' ', ' ', '#', '#', '#', '#', '#',
      '#', '#', '#', '#', ' ', ' ')

    val insert: Vector[Char] = Vector(' ', ' ', '#', 'D', '#', 'C', '#', 'B', '#', 'A', '#', ' ', ' ', ' ', ' ', '#',
      'D', '#', 'B', '#', 'A', '#', 'C', '#', ' ', ' ')

    def amphipodHeuristic(ground: Vector[Char]): Int =
      val inHallway: Vector[Int] = hall.filter((i: Int) => ground(i) != '.')
      val inPlace: Vector[Int] = room("part02")
        .toVector
        .flatMap((c: Char, ds: Vector[Int]) => ds.reverse.takeWhile((f: Int) => ground(f) == c))
      val inRoom: Vector[Int] = room("part02").values.flatten.filter((i: Int) => ground(i) != '.' && !inPlace.contains(i)).toVector
      val scores: Vector[Int] = for {
        i <- inHallway ++ inRoom
        dest <- room("part02")(ground(i)).lastOption
        dist = shortestDistance(distanceGraph(ground, width, true))(i, dest)
      } yield dist.get * weights(ground(i))
      scores.sum


    def amphipodGraph(width: Int, part: String): Graph[Vector[Char]] =
      (ground: Vector[Char]) =>

        // hallway into room (H2R)
        val inHallway: Vector[Int] = hall.filter((i: Int) => ground(i) != '.')  // the amphipods in hallway that can move
        val H2R: Map[Vector[Char], Int] =
          if inHallway.isEmpty then Map.empty  // return no new nodes
          else
            val ts: Vector[(Vector[Char], Int)] = for {
              i    <- inHallway
              amphipod = ground(i)
              destinations = room(part)(amphipod)
              if destinations.forall((f: Int) => ground(f) == '.' || ground(f) == amphipod)  // target room may only contain empty or its own kind
              dest <- destinations.filter((f: Int) => ground(f) == '.').lastOption
              dist = shortestDistance(distanceGraph(ground, width))(i, dest)
              if dist != None  // make sure target destination is reachable
            } yield (swap(ground, i, dest), dist.get * weights(ground(i)))
            ts.toMap

        // room into hallway (R2H)
        val inPlace: Vector[Int] = room(part)
          .toVector
          .flatMap((c: Char, ds: Vector[Int]) => ds.reverse.takeWhile((f: Int) => ground(f) == c))  // these amphipods are in the correct place already
        val inRoom: Vector[Int] = (for {
          r    <- room(part).values
          todo <- r.find((i: Int) => ground(i) != '.')
          if !inPlace.contains(todo)  // don't move the amphipods from their correct place
        } yield todo).toVector
        val R2H: Map[Vector[Char], Int] =
          if inRoom.isEmpty then Map.empty
          else
            val rs = for {
              i    <- inRoom
              dest <- hall
              dist = shortestDistance(distanceGraph(ground, width))(i, dest)
              if dist != None  // make sure target destination is reachable
            } yield (swap(ground, i, dest), dist.get * weights(ground(i)))
            rs.toMap
        R2H ++ H2R

  end Amphipod


  // For animation of result uncomment code below
  //  private val res1 = shortestPath(Amphipod.amphipodGraph(width, "part01"))(input, Amphipod.fin, Amphipod.amphipodHeuristic("part01"))
  //  // For animation of result uncomment code below
  //   res1.get.foreach(f =>
  //     Thread.sleep(500)
  //     print("\u001b[2J")
  //     println(FlatGrid.printFlatGrid(f, width)(identity))
  //   )


  private val answer1 = shortestDistance(Amphipod.amphipodGraph(width, "part01"))(input, Amphipod.fin)
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  private val input2: Vector[Char] =
    val (left, rigtht) = input.splitAt(39)
    left ++ Amphipod.insert ++ rigtht

  private val answer2 = shortestDistance(Amphipod.amphipodGraph(width, "part02"))(input2, Amphipod.fin2, Amphipod.amphipodHeuristic)
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
