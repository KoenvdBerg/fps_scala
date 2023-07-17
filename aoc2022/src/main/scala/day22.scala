import scala.io.*
import math.*
import scala.annotation.tailrec
import aoc2022.Grid2D.Point
import day22.Step.*

/**
 * PART 01:
 *
 * Easy foldRight
 *
 * PART 02:
 *
 * Sort and then take 3 and sum
 * 
 * IDEA:
 * 
 * parsing strat:
 * use Point
 * There are three tiles: Walk, Walls
 * 
 * solving strat:
 * make function that executes a command.
 * 
 * result should be foldleft over the commands
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
      
    def move(tiles: Set[Point], walls: Set[Point], amount: Int): Player = 
      
      val all: Set[Point] = tiles ++ walls
      
      def go(n: Int, acc: Point): Point =
        //println(s"curPos: $acc, with dir: $dir")
        if n <= 0 then acc
        else 
          val nextLoc: Point = acc + dir
          if walls(nextLoc) then acc
          else if tiles(nextLoc) then go(n-1, nextLoc)
          else
            val wrapLoc: Point = dir match
              case Point(1, 0)  => all.filter(_.y == nextLoc.y).toVector.minBy(_.x)
              case Point(-1, 0) => all.filter(_.y == nextLoc.y).toVector.maxBy(_.x)
              case Point(0, 1)  => all.filter(_.x == nextLoc.x).toVector.minBy(_.y)
              case Point(0, -1) => all.filter(_.x == nextLoc.x).toVector.maxBy(_.y)
            //println(wrapLoc)
            if tiles(wrapLoc) then go(n-1, wrapLoc)
            else acc
      copy(loc = go(amount, loc))
        
  object Player:
    
    def performStep(player: Player, step: Step): Player = step match
      case Move(n) => player.move(tiles, walls, n)
      case turn    => 
        println(s"turning: $turn")
        player.turn(turn)
        
  
  private val startLoc: Point = tiles.minBy(_.toTuple.swap)
  private val res1: Player = steps.foldLeft(Player(startLoc, Point(1,0)))(Player.performStep)
  private val answer1 = res1.score
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis
    
  val cubeL: Int = 50

  private val answer2 = None
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
