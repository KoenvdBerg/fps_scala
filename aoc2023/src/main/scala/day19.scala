import scala.io.*
import math.*
import scala.annotation.tailrec

object day19 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

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

  case class Operation(part: String, ret: String, comp: Char, v: Int):
    def getOp: Long => Boolean = 
      if v == -1 then (_: Long) => true
      else if comp == '>' then (l: Long) => l > v
      else (l: Long) => l < v

  private val (ratings, workflows): (List[Rating], Map[String, List[Operation]]) =

    def parseRating(s: String): Option[Rating] = s match
      case s"{x=$x,m=$m,a=$a,s=$s}" => Some(Rating(x.toLong, m.toLong, a.toLong, s.toLong))
      case _ => None

    def parseWorkflow(s: String): Option[(String, List[Operation])] = s match
      case s"$id{$ops}" if !ops.contains('=') => Some(id -> parseOp(ops))
      case _ => None

    def parseOp(s: String): List[Operation] =
      s.split(",").toList.map {
        case s"$part<$lo:$ret" => Operation(part, ret, '<', lo.toInt)
        case s"$part>$lo:$ret" => Operation(part, ret, '>', lo.toInt)
        case s"$ret" => Operation("all", ret, '-', -1)
      }

    val in: List[String] = Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList

    (in.flatMap(parseRating), in.flatMap(parseWorkflow).toMap)


  def processRating(rating: Rating, workflows: Map[String, List[Operation]]): String =

    @tailrec
    def go(flowId: String): String =
      val work: List[Operation] = workflows(flowId)
      val i: Int = work.indexWhere(operation => operation.getOp(rating.get(operation.part)))
      val nextId: String = work(i).ret
      if nextId == "R" || nextId == "A" then nextId
      else go(nextId)

    go("in")

  private val res1: List[Rating] = ratings.filter(r => processRating(r, workflows) == "A")
  private val answer1: Long = res1.map(_.total).sum
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
        
    def splitRanges(operations: List[Operation], input: Map[String, Range]): Vector[Map[String, Range]] =
      operations.foldLeft(Vector(input)) { (res: Vector[Map[String, Range]], o: Operation) =>
        if o.part == "all" then res
        else
          val r: Map[String, Range] = res.last
          val thisr: Range = r(o.part)
          val rmin: Int = thisr.min
          val rmax: Int = thisr.max
          val cur: Map[String, Range] = 
            if o.comp == '<' then r.updated(o.part, rmin until o.v)
            else r.updated(o.part, (o.v+1) to rmax)
          val next: Map[String, Range] = 
            if o.comp == '<' then r.updated(o.part, o.v to rmax)
            else r.updated(o.part, rmin to o.v)
          res.dropRight(1) ++ Vector(cur, next)
      }
    
    
    def processWorkTree(input: Map[String, Range], workTree: WorkTree): Long =

      def recur(in: Map[String, Range], workTree: WorkTree): Long = workTree match
        case Node(key, ops, childs) =>
          val goDown: Vector[Map[String, Range]] = splitRanges(ops, in)
          goDown.indices.map(i => recur(goDown(i), childs(i))).sum
        case End(r) =>
          if r == "A" then in.values.map(_.length.toLong).product
          else 0

      recur(input, workTree)

  private val test: WorkTree = WorkTree.fromWorkflows("in", workflows)
  private val start: Map[String, Range] = Map(
    "s" -> (1 to 4000),
    "a" -> (1 to 4000),
    "m" -> (1 to 4000),
    "x" -> (1 to 4000))
  private val answer2: Long = WorkTree.processWorkTree(start, test)
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
