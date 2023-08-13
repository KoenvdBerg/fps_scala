import scala.annotation.tailrec
import scala.io.*
import aoc2021.Grid2D.Point

object day13 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val (points, folds): (Set[Point], Vector[Point]) =
    val infile = Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector

    (
      infile.flatMap {
        case s"$x,$y" => Some(Point(x.toInt, y.toInt))
        case _        => None
      }.toSet,
      infile.flatMap {
        case s"fold along x=$i" => Some(Point(i.toInt, 0))
        case s"fold along y=$i" => Some(Point(0, i.toInt))
        case _                  => None
      }
    )

  def fold(ps: Set[Point], foldInstruction: Point): Set[Point] =
    foldInstruction match
      case Point(0, y) => ps.foldLeft(Set.empty[Point]) { (r: Set[Point], p: Point) =>
        if p.y < y then r + p
        else r + Point(p.x, y - (p.y - y))
      }
      case Point(x, 0) => ps.foldLeft(Set.empty[Point]) { (r: Set[Point], p: Point) =>
        if p.x < x then r + p
        else r + Point(x - (p.x - x), p.y)
      }

  private val res1: Set[Point] = fold(points, folds.head)
  private val answer1: Int = res1.size
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  private val res2: Set[Point] = folds.foldLeft(points)(fold)
  private val answer2 = "\n" + Point.gridPrintable(res2.toVector)(p => if res2.contains(p) then '#' else '.').reverse
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
