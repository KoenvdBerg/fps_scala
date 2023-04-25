import scala.io.*
import math.*
import scala.collection.mutable
import scala.collection.mutable.Stack

/**
 *  IDEA: make list of Point that have + \ /
 *  cards are tuple of (coords, dir, intersection) e.g. (Point(1,1), (1,0), (0,0,1)) --> moving right (x + 1, y + 0) each tick
 *  / --> transforms (x+1, y-1)
 *  \ --> transforms (x-1, y+1)
 *  + --> transforms(x+0, y+0) or / \
 *  Use the State monad from scalaZ to implement the cart state for intersections
 *  perhaps also make: type Cart[A, S] or something like this
 *  TODO: make function that reads in file as grid
 *  compute the whole lot. If any coord point of card is identical, then return crash coords
 *  track cards only, + / \ are fixed and only operate if card has same coords
 */

object day13 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  def rotateVector[A](n: Int, s: Vector[A]): Vector[A] =
    if s.isEmpty then s
    else
      val nbound: Int = n % s.length // skipping the full rotation rounds
      if nbound < 0 then rotateVector(nbound + s.length, s)
      else s.drop(nbound) ++ s.take(nbound)

  case class Point(x: Int, y: Int):
    def add(p2: Point): Point = Point(x + p2.x, y + p2.y)
  sealed trait Switch
  case object Straight extends Switch
  case object Back extends Switch
  case object Forward extends Switch
  case object Intersect extends Switch

  case class Rail(switch: Switch, loc: Point)
  case class Cart(loc: Point, dir: Point, intersect: Vector[Switch]):

    def update: Cart =
      Cart(loc.add(dir), dir, intersect)

    def computeSwitch(turn: Switch): Cart =
      turn match
        case Straight => straight.update
        case Back => backwardSwitch.update
        case Forward => forwardSwitch.update
        case Intersect => handleIntersect.update
    def forwardSwitch: Cart =
      Cart(loc, Point(dir.y, dir.x), intersect)

    def backwardSwitch: Cart =
      Cart(loc, Point(-dir.y, -dir.x), intersect)

    def straight: Cart =
      Cart(loc, dir, intersect)

    def handleIntersect: Cart =
      val newDir: Point = (intersect.head, dir) match
        case (Back, Point(0, 1)) => backwardSwitch.dir
        case (Back, Point(0, -1)) => backwardSwitch.dir
        case (Back, Point(1, 0)) => forwardSwitch.dir
        case (Back, Point(-1, 0)) => forwardSwitch.dir
        case (Forward, Point(0, 1)) => forwardSwitch.dir
        case (Forward, Point(0, -1)) => forwardSwitch.dir
        case (Forward, Point(1, 0)) => backwardSwitch.dir
        case (Forward, Point(-1, 0)) => backwardSwitch.dir
        case (Straight, _) => straight.dir
        case _ => sys.error("unknown track cannot handle intersection")
      Cart(loc, newDir, rotateVector(1, intersect))

  private val (inrails, incarts): (Vector[Rail], Vector[Cart]) =
    def parseRail(s: Char, x: Int, y: Int): Option[Rail] =
      s match
        case '/' => Some(Rail(Back, Point(x, y)))
        case '\\' => Some(Rail(Forward, Point(x, y)))
        case '+' => Some(Rail(Intersect, Point(x, y)))
        case '|' | '-' | '>' | '<' | 'v' | '^' => Some(Rail(Straight, Point(x, y)))
        case _ => None

    def parseCart(s: Char, x: Int, y: Int): Option[Cart] =
      s match
        case '>' => Some(Cart(Point(x, y), Point(1, 0), Vector(Forward, Straight, Back)))
        case '<' => Some(Cart(Point(x, y), Point(-1, 0), Vector(Forward, Straight, Back)))
        case '^' => Some(Cart(Point(x, y), Point(0, -1), Vector(Forward, Straight, Back)))
        case 'v' => Some(Cart(Point(x, y), Point(0, 1), Vector(Forward, Straight, Back)))
        case _ => None

    val infile = Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector
      .zipWithIndex
    (
      infile.flatMap((ss, y) => ss.zipWithIndex.flatMap((cc, x) => parseRail(cc, x, y))),
      infile.flatMap((ss, y) => ss.zipWithIndex.flatMap((cc, x) => parseCart(cc, x, y)))
    )


  def simulateTrain(carts: Vector[Cart], rails: Vector[Rail]): Point =

    def go(cs: Vector[Cart], acc: Vector[Cart] = Vector.empty): (Option[Point], Vector[Cart]) =
      cs match
        case c +: t =>
          // check if current cart reached other cart location. In that case exit function
          if carts.map(_.loc).count(_ == c.loc) > 1 then (Some(c.loc), acc)

          // continue updating the current cart
          else
            val rail: Option[Rail] = rails.find(r => r.loc == c.loc)
            rail match
              case None => sys.error(s"cannot update because rail not found: $rail and $c, $rails")
              case Some(r) => go(t, c.computeSwitch(r.switch) +: acc)
        case Vector() => (None, acc.reverse)

    val (loc, newCarts): (Option[Point], Vector[Cart]) = go(carts)
    loc match
      case Some(p) => p
      case None => simulateTrain(newCarts.sortBy(_.loc.x), rails)


  private val answer1 = simulateTrain(incarts, inrails)
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  def simulateTrain2(carts: Vector[Cart], rails: Vector[Rail]): Cart =

    def go(cs: Vector[Cart], acc: Vector[Cart] = Vector.empty): Vector[Cart] =
      cs match
        case c +: t =>
          // check if current cart reached other cart location. In that case delete carts and continue
          if carts.map(_.loc).count(_ == c.loc) > 1 then go(t.filter(_.loc != c.loc), acc)

          // continue updating the current cart
          else
            val rail: Option[Rail] = rails.find(r => r.loc == c.loc)
            rail match
              case None => sys.error(s"cannot update because rail not found: $rail and $c, $rails")
              case Some(r) => go(t, c.computeSwitch(r.switch) +: acc)
        case Vector() => acc.reverse

    val newCarts: Vector[Cart] = go(carts)
    if newCarts.length == 1 then
      newCarts.head
    else
      simulateTrain2(newCarts.sortBy(_.loc.x), rails)


  private val start2: Long =
    System.currentTimeMillis

  private val answer2 = simulateTrain2(incarts, inrails)
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
