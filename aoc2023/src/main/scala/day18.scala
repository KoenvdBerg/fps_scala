import scala.io.*
import math.*
import scala.annotation.tailrec

object day18 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  
  case class Point(x: Long, y: Long)
  
  case class Plan(dir: Char, depth: Int, color: String):
    
    def fromColor: Plan =
      val d: Char = color.takeRight(2).head match
        case '0' => 'R'
        case '1' => 'D'
        case '2' => 'L'
        case '3' => 'U'
      val dist: Int = Integer.parseInt(color.slice(2, 7), 16)
      Plan(d, dist, color)
      
  private val input: Vector[Plan] =

    def parse(s: String): Plan = s match
      case s"$dir $n $color" => Plan(dir.head, n.toInt, color)
      case _ => sys.error(s"cannot parse $s")

    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector
      .map(parse)


  object Lagoon:

    def getLagoonEdges(plans: Vector[Plan]): Vector[Point] =

      plans.foldLeft(Vector(Point(0L, 0L))) { (res: Vector[Point], pl: Plan) =>
        val end: Point = res.last
        val next: Point = pl.dir match
          case 'U' => Point(end.x, end.y - pl.depth)
          case 'D' => Point(end.x, end.y + pl.depth)
          case 'L' => Point(end.x - pl.depth, end.y)
          case 'R' => Point(end.x + pl.depth, end.y)
        res.appended(next)
      }

    def polygonArea(polygonEdges: Vector[Point]): Double =
      // https://www.baeldung.com/cs/2d-polygon-area
      // https://en.wikipedia.org/wiki/Area_of_a_triangle
      val area: Vector[Long] = polygonEdges
        .sliding(2).toVector
        .map(f =>
          val a1 = f.head
          val a2 = f(1)
          a1.x * a2.y - a1.y * a2.x
        )
      area.sum * 0.5
      
    def wellSize(polygonEdges: Vector[Point], border: Int): Double =
      val a: Double = polygonArea(polygonEdges)
      // interior is the total surface - the area of half the border. Since the polygon triangulation included 
      // the surface of half the border, the true inner area is the area - half the border area. 
      border + (a - 0.5 * border + 1)

  private val lagoon: Vector[Point] = Lagoon.getLagoonEdges(input)
  private val answer1: Long = Lagoon.wellSize(lagoon, input.map(_.depth).sum).toLong
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")
  
  private val start2: Long =
    System.currentTimeMillis
  
  private val in2: Vector[Plan] = input.map(_.fromColor)
  private val lagoon2: Vector[Point] = Lagoon.getLagoonEdges(in2)
  private val answer2: Long = Lagoon.wellSize(lagoon2, in2.map(_.depth).sum).toLong
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
