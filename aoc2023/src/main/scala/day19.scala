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
      case _ => 1L

    def total: Long = x + m + a + s

  case class Operation(part: String, op: Long => Boolean, ret: String)

  private val (ratings, workflows): (List[Rating], Map[String, List[Operation]]) =

    def parseRating(s: String): Option[Rating] = s match
      case s"{x=$x,m=$m,a=$a,s=$s}" => Some(Rating(x.toLong, m.toLong, a.toLong, s.toLong))
      case _ => None

    def parseWorkflow(s: String): Option[(String, List[Operation])] = s match
      case s"$id{$ops}" if !ops.contains('=') => Some(id -> parseOp(ops))
      case _ => None

    def parseOp(s: String): List[Operation] =
      s.split(",").toList.map{
        case s"$part<$lo:$ret" => Operation(part, (l: Long) => l < lo.toLong, ret)
        case s"$part>$lo:$ret" => Operation(part, (l: Long) => l > lo.toLong, ret)
        case s"$ret" => Operation("all", (_: Long) => true, ret)
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
      val i = work.indexWhere(operation => operation.op(rating.get(operation.part)))
      val nextId = work(i).ret
      if nextId == "R" || nextId == "A" then nextId
      else go(nextId)

    go("in")

  private val res1 = ratings.filter(r => processRating(r, workflows) == "A")
  private val answer1 = res1.map(_.total).sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  enum WorkTree:
    case End(result: String)
    case Node(key: String, ops: List[Operation], childs: List[WorkTree])

  object WorkTree:

    def fromWorkflows(cur: String, workflows: Map[String, List[Operation]]): WorkTree =
      val work = workflows.get(cur)
      work match
        case Some(w) => Node(cur, w, w.map(op => fromWorkflows(op.ret, workflows)))
        case None => End(cur)

    def processNode(operations: List[Operation], input: Map[String, Vector[Long]]): Vector[Map[String, Vector[Long]]] =
      operations.foldLeft(Vector(input)) { (res: Vector[Map[String, Vector[Long]]], o: Operation) =>
        if o.part == "all" then res
        else
          val r: Map[String, Vector[Long]] = res.last
          val nextV1 = r.updated(o.part, r(o.part).takeWhile(o.op))
          val nextV2 = r.updated(o.part, r(o.part).dropWhile(o.op))
          res.dropRight(1) ++ Vector(nextV1, nextV2)
      }



    def processWorkTree(input: Map[String, Vector[Long]], workTree: WorkTree): Map[String, Vector[WorkTree]] = workTree match
      case Node(key, ops, childs) =>
        val goDown = processNode(ops, input)
        println(goDown)
        //val total = goDown.indices.map(i => processWorkTree(goDown(i), childs(i)))
        //pprint.pprintln(total)
        Map("k" -> Vector(WorkTree.End("kjd")))

      case End(r) => input.map((s, vv) => (s, vv.map(l => End(r))))





  private val test = WorkTree.fromWorkflows("in", workflows)
  private val start: Map[String, Range] = Map(
    "s" -> (1 to 4000),
    "a" -> (1 to 4000),
    "m" -> (1 to 4000),
    "x" -> (1 to 4000))
  private val res2 = WorkTree.processWorkTree(start, test)
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

//case class PartValue(i: Int, ret: String, affected: Boolean)
//
//def disectNext(op: Operation, stack: Vector[PartValue], part: String): Vector[PartValue] = op match
//  case Operation.Op(p, op, ret) if p == part =>
//    stack.map(pv => if !pv.affected && op(pv.i) then PartValue(pv.i, ret, true) else pv)
//  case Operation.Op(_, _, _) => stack
//  case Operation.Ret(_, ret) => stack.map(pv => if !pv.affected then PartValue(pv.i, ret, true) else pv)
//
//
//// todo: remove int from Vector[(Int, String)]
//def disect(workflows: Map[String, List[Operation]]): Map[String, Vector[PartValue]] =
//
//  val startStack: Map[String, Vector[PartValue]] = Vector("x", "m", "a", "s").map(p => (p, (1 to 4000).toVector.map(i => PartValue(i, "", false)))).toMap
//
//
//  def go(flowId: String, stack: Map[String, Vector[PartValue]]): Map[String, Vector[PartValue]] =
//    val work: List[Operation] = workflows(flowId)
//    val next: Map[String, Vector[PartValue]] = work.foldLeft(stack) { (res: Map[String, Vector[PartValue]], op: Operation) =>
//      Vector("x", "m", "a", "s").foldLeft(res) { (innerRes: Map[String, Vector[PartValue]], part: String) =>
//        innerRes.updated(part, disectNext(op, innerRes(part), part))
//      }
//    }
//    println(next)
//
//
//    Map("koen" -> Vector(PartValue(1, "sdkl", true)))
//
//  go("in", startStack)