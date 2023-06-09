import scala.io.*
import math.*
import scala.collection.mutable
import scala.collection.mutable.Stack
import aoc2018.VectorUtils.rotateVector


/**
 *
 * PART 1:
 *
 * This was a really fun puzzle to solve. The core of the puzzle is to work with moving points and to be able to
 * change the movement of these points based on a certain rail switch. The movement looks like this:
 *
 * `>` : x=1, y=0
 * `<` : x=-1, y=0
 * `^` : x=0, y=-1
 * `v` : x=0, y=1
 *
 * The tree movements perform the following operation on a moving cart:
 *
 * `/` : x=-y, y=-x
 * `\` : x=y, y=x
 * `_` : x=x, y=y (i.e. go straight)
 * `+` : either `/`, `\`, `_` based on tracker per cart.
 *
 * If a cart hits an intersection, it's important to know where the cart came from the handle the correct form of
 * left, right or straight.
 *
 * The simulate function below ties al the rules for the cart model together and sees when a cart crashes. If that
 * happens, it returns the location of the cart crash.
 *
 * PART 2:
 *
 * Here I could reuse large parts of the code for part 1, but I changed the simulate function to remove crashed carts
 * and keep continuing until the last cart.
 *
 */

object day13 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis


  case class Point(x: Int, y: Int):
    def +(p2: Point): Point = Point(x + p2.x, y + p2.y)


  sealed trait Switch
  case object Straight extends Switch
  case object Back extends Switch
  case object Forward extends Switch
  case object Intersect extends Switch

  case class Rail(switch: Switch, loc: Point)
  case class Cart(loc: Point, dir: Point, intersect: Vector[Switch]):

    def update: Cart =
      Cart(loc + dir, dir, intersect)

    def computeSwitch(turn: Switch): Cart =
      turn match
        case Straight  => straight.update
        case Back      => backwardSwitch.update
        case Forward   => forwardSwitch.update
        case Intersect => handleIntersect.update
    def forwardSwitch: Cart =
      Cart(loc, Point(dir.y, dir.x), intersect)

    def backwardSwitch: Cart =
      Cart(loc, Point(-dir.y, -dir.x), intersect)

    def straight: Cart =
      Cart(loc, dir, intersect)

    def handleIntersect: Cart =

      val newDir: Point = (intersect.head, dir) match
        case (Back, Point(0, _)) => backwardSwitch.dir
        case (Back, Point(_, 0)) => forwardSwitch.dir
        case (Forward, Point(0, _)) => forwardSwitch.dir
        case (Forward, Point(_, 0)) => backwardSwitch.dir
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
          // update this cart based on rail it's on
          val rail: Option[Rail] = rails.find(r => r.loc == c.loc)
          rail match
            case None => sys.error(s"cannot update because rail not found: $rail and $c, $rails")
            case Some(r) =>
              val nextCart = c.computeSwitch(r.switch)
              if acc.map(_.loc).contains(nextCart.loc) || t.map(_.loc).contains(nextCart.loc) then
                (Some(nextCart.loc), acc)  // crash happens so return location
              else
                go(t, nextCart +: acc)     // continue updating carts movement
        case _ => (None, acc.reverse)

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
          // update this cart based on rail it's on
          val rail: Option[Rail] = rails.find(r => r.loc == c.loc)
          rail match
            case None => sys.error(s"cannot update because rail not found: $rail and $c, $rails")
            case Some(r) =>
              // if cart is at same location of other cart then delete this cart and other cart. else continue
              val nextCart = c.computeSwitch(r.switch)
              if acc.map(_.loc).contains(nextCart.loc) || t.map(_.loc).contains(nextCart.loc) then
                go(t.filter(_.loc != nextCart.loc), acc.filter(_.loc != nextCart.loc))
              else
                go(t, nextCart +: acc)
        case _ => acc.reverse

    if carts.length == 1 then carts.head
    else
      val newCarts: Vector[Cart] = go(carts)
      simulateTrain2(newCarts.sortBy(_.loc.x), rails)


  private val start2: Long =
    System.currentTimeMillis

  private val answer2 = simulateTrain2(incarts, inrails)
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
