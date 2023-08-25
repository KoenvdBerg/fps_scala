import scala.annotation.tailrec
import scala.io.*
import aoc2021.Combinator.Parser
import aoc2021.Combinator.Parser.*

object day18 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: List[SnailNumber] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList
      .map(SnailNumber.parseSnailNumber)

  enum SnailNumber: 
    case Pair(left: SnailNumber, right: SnailNumber)
    case Num(number: Int)
    
    def asString: String = this match
      case Pair(l, r) => "[" + l.asString + "," + r.asString + "]"
      case Num(n)     => s"$n"
    
    def ++(that: SnailNumber): SnailNumber = Pair(this, that)
    
    def addLeft(n: Int): SnailNumber = this match
      case Num(m)     => Num(n + m)
      case Pair(l, r) => Pair(l.addLeft(n), r)

    def addRight(n: Int): SnailNumber = this match
      case Num(m) => Num(n + m)
      case Pair(l, r) => Pair(l, r.addRight(n))
    
    def explode: SnailNumber = 
      
      def go(sn: SnailNumber, level: Int): (Int, SnailNumber, Int) = sn match
        case Pair(Num(x), Num(y)) if level == 4 => (x, Num(0), y) 
        case Pair(Num(x), r) =>
          val (addL, ssn, addR): (Int, SnailNumber, Int) = go(r, level + 1)
          (0, Pair(Num(x + addL), ssn), addR)
        case Pair(l, Num(y)) =>
          val (addL, ssn, addR): (Int, SnailNumber, Int) = go(l, level + 1)
          (addL, Pair(ssn, Num(y + addR)), 0)
        case Pair(l, r) => 
          val (laddL, lsn, laddR) = go(l, level + 1)   // first explode left
          val (raddL, rsn, raddR) = go(r.addLeft(laddR), level + 1)  // add result of left explosion to right, then explode
          (laddL, Pair(lsn.addRight(raddL), rsn), raddR)  // add result of right explosion back to left
        case Num(n) => (0, Num(n), 0)
      
      go(this, 0)._2
    
    def splitOne: SnailNumber = 
      
      def go(sn: SnailNumber): (SnailNumber, Boolean) = sn match
        case Pair(l, r) => 
          val (left, done) = go(l)
          if done then (Pair(left, r), true)
          else 
            val (right, d) = go(r)
            (Pair(left, right), d)
        case Num(n) if n >= 10 => (Pair(Num(n / 2), Num((n + 1) / 2)), true)
        case Num(n)            => (Num(n), false)
      
      go(this)._1

    @tailrec
    final def reduce: SnailNumber =
      val x = this.explode
      val next = x.splitOne
      if next == this then next else next.reduce
      
    def magnitude: Int = this match
      case Num(n)     => n
      case Pair(l, r) => 3 * l.magnitude + 2 * r.magnitude 
      
    
  object SnailNumber: 
    
    val n: Parser[Num] = Parser.number.map((s: String) => Num(s.toInt))
    val p: Parser[Pair] = for {
      _  <- Parser.char('[')
      s1 <- snailnumber
      _  <- Parser.char(',')
      s2 <- snailnumber
      _  <- Parser.char(']')
    } yield Pair(s1, s2)
    val snailnumber: Parser[SnailNumber] = n | p

    def parseSnailNumber(s: String): SnailNumber = snailnumber.run(s) match
      case Right(v) => v
      case Left(e) => sys.error(e)
  
  private val res1 = input.tail.foldLeft(input.head) { (res: SnailNumber, in: SnailNumber) => (res ++ in).reduce}
  private val answer1 = res1.magnitude  
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis
  
  private val res2 = input.foldLeft(List.empty[Int]){ (res: List[Int], in: SnailNumber) => 
    input.flatMap{ (f: SnailNumber) => 
      List((f ++ in).reduce.magnitude, (in ++ f).reduce.magnitude)
    } ++ res
  }
  private val answer2 = res2.max
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
