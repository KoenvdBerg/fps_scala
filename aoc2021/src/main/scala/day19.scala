import scala.annotation.tailrec
import scala.io.*
import aoc2021.Algorithms.Matrix


object aay19 extends App:

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
        case s"--- scanner $i ---" => res.appended(Scanner(Vector.empty, i.toInt))
        case s"$x,$y,$z" =>
          val p: Point3D = Point3D(x.toInt, y.toInt, z.toInt)
          res.dropRight(1).appended(res.last.add(p))
    }
    
  // todo: move case class to Utils
  case class Point3D(x: Int, y: Int, z: Int):
    
    def distanceToPoint(that: Point3D): Double =
      math.sqrt(math.pow(that.x - x, 2) + math.pow(that.y - y, 2) + math.pow(that.z - z, 2))
      
    def -(that: Point3D): Point3D = Point3D(x - that.x, y - that.x, z - that.z)
    
    def rotations: Vector[Point3D] = ???
//
//    def orientations(pos: Vec3): Seq[Vec3] =
//      val Vec3(x, y, z) = pos
//      Seq(
//        Vec3(+x, +y, +z), Vec3(-y, +x, +z), Vec3(-x, -y, +z), Vec3(+y, -x, +z)
//        , Vec3(-x, +y, -z), Vec3(+y, +x, -z), Vec3(+x, -y, -z), Vec3(-y, -x, -z)
//        , Vec3(-z, +y, +x), Vec3(-z, +x, -y), Vec3(-z, -y, -x), Vec3(-z, -x, +y)
//        , Vec3(+z, +y, -x), Vec3(+z, +x, +y), Vec3(+z, -y, +x), Vec3(+z, -x, -y)
//        , Vec3(+x, -z, +y), Vec3(-y, -z, +x), Vec3(-x, -z, -y), Vec3(+y, -z, -x)
//        , Vec3(+x, +z, -y), Vec3(-y, +z, -x), Vec3(-x, +z, +y), Vec3(+y, +z, +x)
//      )
    // todo: user permutations here to get the rotations

      
  case class Scanner(points: Vector[Point3D], id: Int): 
    def add(p: Point3D): Scanner = copy(points.appended(p))
    
    def getInnerDistances: Map[IndexedSeq[Double], Point3D] =
      points.foldLeft((points, Map.empty)) { (res: (IndexedSeq[Point3D], Map[IndexedSeq[Double], Point3D]), in) =>
        (res._1, res._2 + (res._1.map(_.distanceToPoint(in)).sorted -> in))
      }._2
    
  object Scanner:

    // todo: move below functions to case class of Scanner
    def findOverlap(scannerA: Scanner, scannerB: Scanner): Map[Point3D, Point3D] =
      val innerDistA = scannerA.getInnerDistances
      val innerDistB = scannerB.getInnerDistances
      
      innerDistA.flatMap { (seq: IndexedSeq[Double], poi: Point3D) =>
        innerDistB
          .find(p => p._1.toSet.intersect(seq.toSet).size >= 12)
          .map(l => (poi, l._2))
      }
    
    def resolve(scannerA: Scanner, scannerB: Scanner): Option[Point3D] =
      val overlap: Map[Point3D, Point3D] = findOverlap(scannerA, scannerB)
      if overlap.isEmpty then None
      else 
        // todo: generate proper rotations functions
        // todo: performing subtraction on points should give the orientation that is constant
        val x = overlap.map((p1, p2) => Vector(p1.x + p2.x, p1.x - p2.x)).transpose.find(p => p.head == p.last).map(_.head)
        val y = overlap.map((p1, p2) => Vector(p1.y + p2.y, p1.y - p2.y)).transpose.find(p => p.head == p.last).map(_.head)
        val z = overlap.map((p1, p2) => Vector(p1.z + p2.z, p1.z - p2.z)).transpose.find(p => p.head == p.last).map(_.head)
        
        println(overlap.map((p1, p2) => Vector(p1.x + p2.x, p1.x - p2.x)))
        pprint.log(overlap)
        
        for { px <- x ; py <- y ; pz <- z } yield Point3D(px, py, pz)
        
    def solveForAll(scanners: Vector[Scanner]): Map[(Int, Int), Point3D] =
      scanners.foldLeft((scanners, Map.empty)){ (res: (Vector[Scanner], Map[(Int, Int), Point3D]), in: Scanner) =>
        val overlap = res._1
          .flatMap( (s: Scanner) => 
          if s.id == in.id then None else resolve(in, s).map(p => (in.id, s.id) -> p)).toMap
        (res._1, res._2 ++ overlap)
      }._2
    
      
      
  private val answer1 = Scanner.resolve(input(4), input(1))
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  private val answer2 = None
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
