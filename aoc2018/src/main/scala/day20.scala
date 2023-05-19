import scala.io.*
import aoc2018.Grid2D.Point

object day20 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: List[Char] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList
      .head
      .toList

  case class Dist(x: Int, y: Int, dist: Int):
    def move(c: Char): Dist = c match
      case 'W' => Dist(x-1, y, dist)
      case 'E' => Dist(x+1, y, dist)
      case 'S' => Dist(x, y+1, dist)
      case 'N' => Dist(x, y-1, dist)

    def updateDist: Dist = this.copy(dist = dist + 1)

  def walk(route: List[Char], loc: Dist, wayPoint: List[Dist] = List(Dist(0,0,0)),
           branches: Map[Dist, Vector[Dist]] = Map(Dist(0,0,0) -> Vector.empty[Dist]),
           acc: Set[Dist] = Set.empty[Dist], prevChar: Char = '∆', max: Int = 0): (Int, Set[Dist]) =
    // println(s"$prevChar, $loc")
    route match
      case h :: t => h match

        // this case describes walking. Essentially this adds 1 to the distance & updates the current loc. It also
        // updates the branch it's on, starting on the starting branch. This update consists of adding its next
        // location to the branch for that wayPoint.
        case 'W' | 'E' | 'S' | 'N' =>
          val nextLoc: Dist = loc.move(h).updateDist
          walk(t, nextLoc, wayPoint, branches + ((wayPoint.head, nextLoc +: branches(wayPoint.head))), acc + nextLoc, h, max)

        // ignore these characters and do nothing
        case '^' | '$' => walk(t, loc, wayPoint, branches, acc, h, max)

        // here a branch starts. So the current location is remembered as a wayPoint and appended to the wayPoint list
        // head. The branch is initiated for this new wayPoint with an empty Vector.
        case '(' => walk(t, loc, loc :: wayPoint, branches + (loc -> Vector.empty[Dist]), acc, h, max)

        // here a branch ends. So either the maximum walked distance (a Dist with corresponding x and y) is selected or
        // the 'empty' option that looks like '|)'. The maximum found distance in the entire route is updated based on
        // this selection. The next call continues where the wayPoint is forgotten from the wayPoint list and
        // the branches hashmap.
        case ')' =>
          val thisWayPoint: Dist = wayPoint.head
          val selection: Dist = if prevChar == '|' then thisWayPoint else branches(thisWayPoint).maxBy(_.dist)
          val nMax: Int = selection.dist.max(max)
          walk(t, selection, wayPoint.tail, branches - thisWayPoint, acc, h, nMax)

        // here a new option is encountered. So return to the earlier saved wayPoint and continue from there.
        case '|' => walk(t, wayPoint.head, wayPoint, branches, acc, h, max)
        case _   => sys.error("boom")
      case Nil    => (max, acc)

  private val (dist, path): (Int, Set[Dist]) = walk(input, Dist(0,0,0))
  println(s"Answer day $day part 1: ${dist} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  private val res = path.filter((d: Dist) => d.dist > 1000).map((d: Dist) => (d.x, d.y))
  private val answer2 = res.size + 1  // the + 1 is because the startroom should also be counted
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")