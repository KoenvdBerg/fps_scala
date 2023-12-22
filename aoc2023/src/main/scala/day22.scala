import scala.io.*
import math.*
import scala.annotation.tailrec

object day22 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: Vector[Brick] =

    def parse(s: String): Brick = s match
      case s"$x1,$y1,$z1~$x2,$y2,$z2" => Brick(Range(x1.toInt, x2.toInt + 1), Range(y1.toInt, y2.toInt + 1), Range(z1.toInt, z2.toInt + 1))
      case _ => sys.error(s"cannot parse s")

    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector
      .map(parse)

  case class Brick(xr: Range, yr: Range, z: Range):
    def noHitX(that: Brick): Boolean =
      xr.max < that.xr.min
        || xr.min > that.xr.max

    def noHitY(that: Brick): Boolean =
      yr.min > that.yr.max
        || yr.max < that.yr.min

  object Brick:
    def fallOne(brick: Brick, bricksBelow: Vector[Brick]): Brick =

      @tailrec
      def go(cur: Brick): Brick =

        if cur.z.min <= 1 then cur
        else
          val zBelow: Int = cur.z.min - 1
          val layer: Vector[Brick] = bricksBelow.filter(_.z.max == zBelow)
          if layer.forall(p => cur.noHitX(p) || cur.noHitY(p)) then go(cur.copy(z = Range(cur.z.min - 1, cur.z.max)))
          else cur

      if brick.z.min <= 1 then brick
      else go(brick)

    def fallDown(bricks: Vector[Brick]): Vector[Brick] =
      val bricksSorted: Vector[Brick] = bricks.sortBy(_.z.max)
      bricksSorted.foldLeft(Vector.empty) { (res: Vector[Brick], in: Brick) =>
        res.appended(fallOne(in, res))
      }

    def buildSupportStructure(bricks: Vector[Brick]): Map[Brick, Vector[Brick]] =
      bricks.foldLeft(Map.empty[Brick, Vector[Brick]]) {(res: Map[Brick, Vector[Brick]], in: Brick) =>
        val layerAbove: Vector[Brick] = bricks.filter(_.z.min == in.z.max + 1)
        val supports: Vector[Brick] = layerAbove.filterNot(b => b.noHitX(in) || b.noHitY(in))
        res.updated(in, supports)
      }

    def buildSupportedByStructure(bricks: Vector[Brick]): Map[Brick, Vector[Brick]] =
      bricks.foldLeft(Map.empty[Brick, Vector[Brick]]) { (res: Map[Brick, Vector[Brick]], in: Brick) =>
        val layerBelow: Vector[Brick] = bricks.filter(_.z.max == in.z.min - 1)
        val supportedBy: Vector[Brick] = layerBelow.filterNot(b => b.noHitX(in) || b.noHitY(in))
        res.updated(in, supportedBy)
      }

    def checkDisintegrate(stackedBricks: Vector[Brick]): Vector[Brick] =

      val supported: Map[Brick, Vector[Brick]] = buildSupportStructure(stackedBricks)
      val supportedBy: Map[Brick, Vector[Brick]] = buildSupportedByStructure(stackedBricks)

      stackedBricks.foldLeft(Vector.empty[Brick]) { (res: Vector[Brick], b: Brick) =>
        val supports: Vector[Brick] = supported(b)
        if supports.forall(p => supportedBy(p).exists(_ != b)) then res.appended(b)
        else res
      }

    def howManyFallDown(stackedBricks: Vector[Brick], toRemove: Vector[Brick]): Int =

      val supports: Map[Brick, Vector[Brick]] = buildSupportStructure(stackedBricks)
      val init: Map[Brick, Vector[Brick]] = buildSupportedByStructure(stackedBricks)

      @tailrec
      def go(brick: Vector[Brick], supportedBy: Map[Brick, Vector[Brick]], count: Int): Int =
        val areSupportedBy: Vector[Brick] = brick.flatMap(supports).distinct
        val updatedSupportedBy: Map[Brick, Vector[Brick]] = supportedBy.map((b, vv) => (b, vv.filterNot(brick.contains)))
        val willFall: Vector[Brick] = areSupportedBy.filter(br =>
          val otherSupports: Vector[Brick] = updatedSupportedBy(br).filterNot(brick.contains)
          otherSupports.isEmpty).distinct
        if willFall.isEmpty then count
        else
          go(willFall, updatedSupportedBy, count + willFall.length)

      toRemove.map(bb => go(Vector(bb), init, 0)).sum

  private val fallenDown: Vector[Brick] = Brick.fallDown(input)
  private val bricksThatCan: Vector[Brick] = Brick.checkDisintegrate(fallenDown)
  private val answer1: Int = bricksThatCan.length
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis


  private val bestBricks: Vector[Brick] = fallenDown.filterNot(bricksThatCan.contains)
  private val answer2: Int = Brick.howManyFallDown(fallenDown, bestBricks)
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
