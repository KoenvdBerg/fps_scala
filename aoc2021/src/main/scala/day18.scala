import scala.annotation.tailrec
import scala.io.*
import aoc2021.Combinator.Parser
import aoc2021.Combinator.Parser.*

object aay18 extends App:

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
          if done then (Pair(left, r), done)
          else (Pair(left, go(r)._1), done)
        case Num(n) if n >= 10 => (Pair(Num(n / 2), Num((n + 1) / 2)), true)
        case Num(n)            => (Num(n), false)
      
      go(this)._1
    
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
      
    @tailrec
    def deduce(sn: SnailNumber): SnailNumber =
      val x = sn.explode
      val next = x.splitOne
      if next == sn then next else deduce(next)
  
//  private val res1 = input.tail.foldLeft(input.head) { (res: SnailNumber, in: SnailNumber) =>
//    println(res)
//    println("-------")
//    SnailNumber.deduce(res ++ in)
//  }
  println(SnailNumber.deduce(input.head))
  private val answer1 = None
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  private val answer2 = None
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")

