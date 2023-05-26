import scala.io.*

/**
 *
 * PART 1:
 *
 * The difficult part is parsing in the input. This code parses the input to claims, and then computes for every position
 * how many unique claim IDs are present on that position. It counts the positions with 2 or more claims and that's
 * the answer to this part.
 *
 * PART 2:
 *
 * The solution is to take all the positions on which 2 or more claims are present, and then to take all their IDs.
 * The difference between the set of those IDs with all the IDs in the input is the answer. This works, because
 * the problem states that 1 claim goes completely without overlap.
 *
 */


object day03 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val claims: List[Claim] =

    def parseClaim(s: String): List[Claim] = s match
      case s"#$id @ $xstart,$ystart: ${width}x$length" =>
        (for {
          x <- Range(xstart.toInt, xstart.toInt + width.toInt)
          y <- Range(ystart.toInt, ystart.toInt + length.toInt)
        } yield Claim(x, y, id.toInt)).toList
      case _ => sys.error("BOOM!!!")

    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList
      .flatMap(parseClaim)

  case class Claim(x: Int, y: Int, id: Int)

  private val answer1: Int = claims
    .groupBy((x: Claim) => (x.x, x.y))  // group by positions for each claim
    .map((_, y) => y.size)              // find per position how many times it's occupied
    .count(_ > 1)                       // count claims that are occupied more than once
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  val allIds: Set[Int] = claims.map(_.id).toSet

  val idswithmultipleclaims: Set[Int] = claims
    .groupBy((x: Claim) => (x.x, x.y))          // group by positions for each claim
    .filter((_, y: List[Claim]) => y.size > 1)  // select every position with two or more claims
    .flatMap((_, y: List[Claim]) => y)          // make list of claims
    .map(_.id)                                  // take all the ids for positions that have two or more claims
    .toSet
  private val answer2 = allIds.diff(idswithmultipleclaims)  // the result is the 1 claim that has no overlap
  println(s"Answer day $day part 2: ${answer2.head} [${System.currentTimeMillis - start2}ms]")
