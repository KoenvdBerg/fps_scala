import scala.annotation.tailrec
import scala.io.*

object day10 extends App:
  
  private val day: String =
    this.getClass.getName.drop(3).init
  
  private val start1: Long =
    System.currentTimeMillis
  
  private val input: List[String] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList

  val scores: Map[Char, Int] = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)

  def flip(c: Char): Char = c match
    case '(' => ')'
    case '[' => ']'
    case '{' => '}'
    case '<' => '>'
    
  def getCorrupted(c: String): Option[Char] =
       
    @tailrec
    def go(cc: List[Char], stack: List[Char] = List.empty): Option[Char] = cc match
      case Nil => None
      case h :: t => 
        if ")>}]".contains(h) then 
          if stack.head == h then go(t, stack.tail)
          else Some(h)
        else go(t, flip(h) :: stack)
        
    go(c.toList)
  
  private val answer1 = input.flatMap(getCorrupted).map(scores).sum 
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")
  
  private val start2: Long =
    System.currentTimeMillis

  val scores2: Map[Char, Int] = Map(')' -> 1, ']' -> 2, '}' -> 3, '>' -> 4)
  
  def score(c: String): Long = c.foldLeft(0L)((z, cc) => z * 5 + scores2(cc))
  
  def autoComplete(c: String): Option[String] =

    @tailrec
    def go(cc: List[Char], stack: List[Char] = List.empty): Option[String] = cc match
      case Nil => Some(stack.mkString(""))
      case h :: t =>
        if ")>}]".contains(h) then
          if stack.head == h then go(t, stack.tail)
          else None
        else go(t, flip(h) :: stack)

    go(c.toList)
    
  private val res2: List[Long] = input.flatMap(autoComplete).map(score).sorted
  private val answer2: Long = res2.drop(res2.length / 2).head 
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")