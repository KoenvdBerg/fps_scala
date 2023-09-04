import scala.annotation.tailrec
import scala.io.*
import aoc2021.Grid3D.Point3D


object day19 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: Vector[Scanner] =
    val infile = Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList
      .filter(_.nonEmpty)
    
    infile.foldLeft(Vector.empty[Scanner]) { (res: Vector[Scanner], in: String) =>
      in match
        case s"--- scanner $i ---" => res.appended(Scanner(Set.empty, i.toInt))
        case s"$x,$y,$z" =>
          val p: Point3D = Point3D(x.toInt, y.toInt, z.toInt)
          res.dropRight(1).appended(res.last.add(p))
    }
    
  case class Scanner(points: Set[Point3D], id: Int): 
    def add(p: Point3D): Scanner = copy(points + p)
    
    def findOverlap2(that: Scanner): Option[(Set[Point3D], Point3D)] =
      val res = for {
        transposed <- that.points.map(_.rotations).transpose.map(_.toSet)
        here       <- points
        remote     <- transposed
        pos = here - remote
        if transposed.map(pos + _).count(points) >= 12
      } yield (transposed.map(pos + _), pos)
      res.headOption
    
  object Scanner:
    def solveForAll(scanners: Vector[Scanner]): (Set[Point3D], Set[Point3D]) =

      def go(todo: Vector[Scanner], found: Set[Point3D], scanners: Set[Point3D]): (Set[Point3D], Set[Point3D]) = todo match
        case Vector() => (scanners, found)
        case h +: t       => 
          Scanner(found, -1).findOverlap2(h)
            .fold(go(t :+ h, found, scanners)) { (o: Set[Point3D], loc: Point3D) => 
              go(t, found ++ o, scanners + loc)
            }

      go(scanners.tail, scanners.head.points, Set.empty)
      
      
  private val res1 = Scanner.solveForAll(input)
  private val answer1 = res1._2.size
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis
    
  def manhattan(p: Set[Point3D]): Int =
    p.foldLeft((p, 0)) { (res, in) =>
      val dist: Int = res._1.map(_.manhattan(in)).max
      if dist > res._2 then (res._1 - in, dist) else (res._1 - in, res._2)
    }._2

  private val answer2 = manhattan(res1._1)
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
