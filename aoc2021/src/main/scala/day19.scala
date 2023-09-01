import scala.annotation.tailrec
import scala.io.*
import aoc2021.Grid3D.Point3D


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
//
    def solveForAll(scanners: Vector[Scanner]): Map[(Int, Int), Point3D] =

      // todo: fix here that teh found set gets expanded with all the find coordinates, relative to scanner 0.
      //       then we can append the other scanners to these points.  
      def go(todo: Set[Scanner], known: Set[Point3D], found: Set[Scanner], scanners: Set[Point3D]): (Set[Point3D], Set[Point3D]) =
        if todo.isEmpty then
          (scanners, found)
        else
      go(scanners.tail, Set.empty, scanners.head.points, Set.empty)
      


      
  private val answer1 = input(0).findOverlap2(input(1))
  
  pprint.log(answer1)
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  private val answer2 = None
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")

//def findOverlap(that: Scanner): Map[Point3D, Point3D] =
//  val innerDistA = this.getInnerDistances
//  val innerDistB = that.getInnerDistances
//
//  innerDistA.flatMap { (seq: IndexedSeq[Double], poi: Point3D) =>
//    innerDistB
//      .find(p => p._1.toSet.intersect(seq.toSet).size >= 12)
//      .map(l => (poi, l._2))
//  }
//def getInnerDistances: Map[IndexedSeq[Double], Point3D] =
//  points.foldLeft((points, Map.empty)) { (res: (IndexedSeq[Point3D], Map[IndexedSeq[Double], Point3D]), in) =>
//    (res._1, res._2 + (res._1.map(_.distanceToPoint(in)).sorted -> in))
//  }._2
//    def resolve(that: Scanner): Option[Point3D] =
//      // first generate the list like in the example, point from this scanner to equal point from that scanner
//      val overlap: Map[Point3D, Point3D] = this.findOverlap(that)
//      if overlap.isEmpty then None
//      else
//        // create every possible orientation for the found points from that scanner
//        // subtract that rotation from the source point, if pattern emerges then rotation is correct
//        val orientations: Vector[Vector[Point3D]] = overlap
//          .map((p1, p2) => (p1, p2.rotations.map(p1 - _)))
//          .values
//          .toVector
//          .transpose
//        // find the pattern i.e. for all x|y|z we find the same value. 
//        val x = orientations.find((vp: Vector[Point3D]) => vp.forall(_.x == vp.last.x)).map(_.head).map(_.x)
//        val y = orientations.find((vp: Vector[Point3D]) => vp.forall(_.y == vp.last.y)).map(_.head).map(_.y)
//        val z = orientations.find((vp: Vector[Point3D]) => vp.forall(_.z == vp.last.z)).map(_.head).map(_.z)
//        for {px <- x; py <- y; pz <- z} yield Point3D(px, py, pz)
//      scanners.foldLeft((scanners, Map.empty)){ (res: (Vector[Scanner], Map[(Int, Int), Point3D]), in: Scanner) =>
//        val overlap = res._1
//          .flatMap( (s: Scanner) => 
//          if s.id == in.id then None else in.resolve(s).map(p => (in.id, s.id) -> p)).toMap
//        (res._1, res._2 ++ overlap)
//      }._2