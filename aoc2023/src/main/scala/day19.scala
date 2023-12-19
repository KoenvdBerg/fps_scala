import com.sun.tools.javac.api.JavacTaskPool.Worker

import scala.io.*
import math.*
import scala.annotation.tailrec

object aday19 extends App:

  private val day: String =
    this.getClass.getName.drop(4).init

  private val start1: Long =
    System.currentTimeMillis

  case class Rating(x: Long, m: Long, a: Long, s: Long):
    def get(key: String): Long = key match
      case "x" => x
      case "m" => m
      case "a" => a
      case "s" => s
      case _ => sys.error(s"cannot get $key")

    def total: Long = x + m + a + s

  object Rating:

    def focusOn(partName: String, v: Long): Rating = partName match
      case "x" => Rating(v, 0, 0, 0)
      case "m" => Rating(0, v, 0, 0)
      case "a" => Rating(0, 0, v, 0)
      case "s" => Rating(0, 0, 0, v)
      case _ => sys.error(s"cannot focus on $partName")

  case class Operation(part: String, op: Rating => Boolean, ret: String, comparison: Char, v: Long)

  private val (ratings, workflows): (List[Rating], Map[String, List[Operation]]) =

    def parseRating(s: String): Option[Rating] = s match
      case s"{x=$x,m=$m,a=$a,s=$s}" => Some(Rating(x.toLong, m.toLong, a.toLong, s.toLong))
      case _ => None

    def parseWorkflow(s: String): Option[(String, List[Operation])] = s match
      case s"$id{$ops}" if !ops.contains('=') => Some(id -> parseOp(ops))
      case _ => None

    def parseOp(s: String): List[Operation] =
      s.split(",").toList.map{
        case s"$part<$lo:$ret" => Operation(part, (r: Rating) => r.get(part) < lo.toLong, ret, '<', lo.toLong)
        case s"$part>$lo:$ret" => Operation(part, (r: Rating) => r.get(part) > lo.toLong, ret, '>', lo.toLong)
        case s"$ret" => Operation("all", (r: Rating) => true, ret, '=', -1)
      }

    val in = Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList

    (in.flatMap(parseRating), in.flatMap(parseWorkflow).toMap)


  def processRating(rating: Rating, workflows: Map[String, List[Operation]]): String =

    @tailrec
    def go(flowId: String): String =
      val work: List[Operation] = workflows(flowId)
      val i = work.indexWhere(operation => operation.op(rating))
      val nextId = work(i).ret
      if nextId == "R" || nextId == "A" then nextId
      else go(nextId)

    go("in")

  private val res1 = ratings.filter(r => processRating(r, workflows) == "A")
  private val answer1 = res1.map(_.total).sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis



  def disect(workflows: Map[String, List[Operation]]): Int =

    def go(flowId: String, stack: Map[String, Range]): Map[String, Range] =
      val work: List[Operation] = workflows(flowId)






  private val answer2 = "l;ds"
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")

//  def focus(rating: Rating, partName: String, workflows: Map[String, List[Operation]]): String =
//
//    @tailrec
//    def go(flowId: String): String =
//      val work: List[Operation] = workflows(flowId)
//      val i = work.indexWhere(operation => operation.part == partName && operation.op(rating))
//      val nextId = if i == -1 then work.last.ret else work(i).ret
//      if nextId == "R" || nextId == "A" then nextId
//      else go(nextId)
//
//    go("in")


//  val x = (1 to 4000).map(i => focus(Rating(i, 1, 0, 538), "x", workflows))
//  val m = (1 to 4000).map(i => focus(Rating(0, i, 0, 0), "m", workflows))
//  val a = (1 to 4000).map(i => focus(Rating(0, 1, i, 538), "a", workflows))
//  val s = (1 to 4000).map(i => focus(Rating(0, 0, 0, i), "s", workflows))
//
//  println((x.contains("A"), m.contains("A"), a.contains("A"), s.contains("A")))
//
//  val test = for {
//    i <- 0 to 4000
//    j <- 0 to 4000
//  } yield Rating(0, 0, j, i)
//
//  test.map(r => processRating(r, workflows))