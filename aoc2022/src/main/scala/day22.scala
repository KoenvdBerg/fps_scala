import scala.io.*
import math.*
import scala.annotation.tailrec
import aoc2022.Grid2D.Point
import day22.Step.*

/**
 * PART 01:
 *
 * This part was relatively straightforward to solve, because wrapping around the playing field meant warping to the 
 * other side depending on the direction. The parsing of the instruction string somehow was difficult for me.  
 *
 * PART 02:
 *
 * I created the drawing below for my input for how to warp the player when it moves across the cube, solved for 2d 
 * coordinates. I started with x-y bounds in the wrapCube() function, but saw a tip for using /50. See code below. 
 * 
 * 
 *            G     H
 *          ┌────┬────┐
 *         F│    │    │C
 *          │    │    │
 *          ├────┼────┘
 *         E│    │  A
 *       E  │    │A
 *     ┌────┼────┤
 *    F│    │    │C
 *     │    │    │
 *     ├────┼────┘
 *    G│    │ B
 *     │    │B
 *     └────┘
 *       H
 * 
 */


object day22 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val (walls, tiles, steps): (Set[Point], Set[Point], List[Step]) =
    def parseTile(s: Char, x: Int, y: Int): Option[Point] =
      s match
        case '.' => Some(Point(x, y))
        case _ => None

    def parseWall(s: Char, x: Int, y: Int): Option[Point] =
      s match
        case '#' => Some(Point(x, y))
        case _ => None
        
    def parseSteps(s: String): List[Step] =
      
      @tailrec
      def go(in: String, acc: List[Step]): List[Step] =
        if in.isEmpty then acc
        else 
          val dir: String = in.takeWhile(_.isLetter)
          if dir.isEmpty then 
            val stepSize: String = in.takeWhile(_.isDigit)
            go(in.drop(stepSize.length), Move(stepSize.toInt) :: acc)
          else dir match
            case "R" => go(in.drop(1), Right :: acc)
            case _   => go(in.drop(1), Left :: acc)
            
      go(s, Nil).reverse
    
    val infile = Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList
      .zipWithIndex
    
    (
      infile.flatMap((ss: String, y: Int) => ss.zipWithIndex.flatMap((cc: Char, x: Int) => parseWall(cc, x, y))).toSet,
      infile.flatMap((ss: String, y: Int) => ss.zipWithIndex.flatMap((cc: Char, x: Int) => parseTile(cc, x, y))).toSet,
      parseSteps(infile.last._1)
    )

  enum Step:
    case Left
    case Right
    case Move(steps: Int)
    
  case class Player(loc: Point, dir: Point):
  
    def score: Int =
      val facing: Int = dir match
        case Point(1, 0)  => 0 
        case Point(-1, 0) => 2
        case Point(0, 1)  => 1
        case Point(0, -1) => 3
      facing + (loc.y + 1) * 1000 + (loc.x + 1) * 4
        
    def turn(step: Step): Player = step match
      case Left    => copy(dir = Point(dir.y, -dir.x))
      case Right   => copy(dir = Point(-dir.y, dir.x))
      case Move(_) => this  // don't turn
      
    def move: Player = copy(loc = loc + dir)
    
    def wrapNormal(allObs: Set[Point]): Player = 
      val newLoc: Point = dir match
        case Point(1, 0) => allObs.filter(_.y == loc.y).toVector.minBy(_.x)
        case Point(-1, 0) => allObs.filter(_.y == loc.y).toVector.maxBy(_.x)
        case Point(0, 1) => allObs.filter(_.x == loc.x).toVector.minBy(_.y)
        case Point(0, -1) => allObs.filter(_.x == loc.x).toVector.maxBy(_.y)
      copy(loc = newLoc)
    
    def wrapCube: Player =
      val l = this.loc
      (this.dir, l.x / 50, l.y / 50) match
        case (Point(0, -1), 1, 0)  => Player(Point(0, l.x + 100), Point(1, 0))     // G
        case (Point(-1, 0), 1, 0)  => Player(Point(0, 149 - l.y), Point(1, 0))     // F 
        case (Point(0, -1), 2, 0)  => Player(Point(l.x - 100, 199), Point(0, -1))  // H
        case (Point(1, 0),  2, 0)  => Player(Point(99, 149 - l.y), Point(-1, 0))   // C
        case (Point(0, 1),  2, 0)  => Player(Point(99 , l.x - 50), Point(-1, 0))   // A
        case (Point(1, 0),  1, 1)  => Player(Point(l.y + 50 , 49), Point(0, -1))   // A
        case (Point(-1, 0), 1, 1)  => Player(Point(l.y - 50, 100), Point(0, 1))    // E 
        case (Point(0, -1), 0, 2)  => Player(Point(50, l.x + 50), Point(1, 0))     // E 
        case (Point(-1, 0), 0, 2)  => Player(Point(50, 149 - l.y), Point(1, 0))    // F 
        case (Point(1, 0),  1, 2)  => Player(Point(149, 149 - l.y), Point(-1, 0))  // C
        case (Point(0, 1),  1, 2)  => Player(Point(49, l.x + 100), Point(-1, 0))   // B
        case (Point(1, 0),  0, 3)  => Player(Point(l.y - 100, 149), Point(0, -1))  // B
        case (Point(0, 1),  0, 3)  => Player(Point(l.x + 100, 0), Point(0, 1))     // H
        case (Point(-1, 0), 0, 3)  => Player(Point(l.y - 100, 0), Point(0, 1))     // G
        case _ => sys.error("cannot wrap around")
        
  object Player:
    val all: Set[Point] = tiles ++ walls
    
    def moveN(amount: Int, part: Int, p: Player): Player =

      def go(n: Int, acc: Player): Player =
        if n <= 0 then acc
        else
          val next: Player = acc.move
          if walls(next.loc) then acc
          else if tiles(next.loc) then go(n-1, next)
          else  // out of bounds, needs wrapping
            val wrapped: Player = if part == 1 then acc.wrapNormal(all) else acc.wrapCube
            if tiles(wrapped.loc) then go(n-1, wrapped)
            else acc

      go(amount, p)
      
    def performStep(part: Int)(player: Player, step: Step): Player = step match
      case Move(n) => moveN(n, part, player)
      case turn    => player.turn(turn)
        
  
  private val startLoc: Point = tiles.minBy(_.toTuple.swap)
  private val res1: Player = steps.foldLeft(Player(startLoc, Point(1,0)))(Player.performStep(1))
  private val answer1 = res1.score
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis
  
  private val res2: Player = steps.foldLeft(Player(startLoc, Point(1,0)))(Player.performStep(2))
  private val answer2 = res2.score
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
