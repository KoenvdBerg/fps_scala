import scala.annotation.tailrec
import scala.io.*
import aoc2021.Algorithms.GraphTraversal.*

import scala.collection.immutable.Queue

object day12 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: Map[String, Vector[String]] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .map { (s: String) =>
        val line = s.split("-")
        (line.head, line.last)
      }
      .foldLeft(Map.empty[String, Vector[String]]) { (m, cc) =>
      (m.get(cc._1), m.get(cc._2)) match
        case (None, None)         => m.updated(cc._1, Vector(cc._2)).updated(cc._2, Vector(cc._1))
        case (Some(v), None)      => m.updated(cc._1, cc._2 +: v).updated(cc._2, Vector(cc._1))
        case (None, Some(v))      => m.updated(cc._1, Vector(cc._2)).updated(cc._2, cc._1 +: v)
        case (Some(v1), Some(v2)) => m.updated(cc._1, cc._2 +: v1).updated(cc._2, cc._1 +: v2)
      }

  case class Path(pos: String, seen: Map[String, Int], seenTwice: Boolean)

  def walk(cave: Map[String, Vector[String]])(part2: Boolean): Int =

    @tailrec
    def go(q: Queue[Path], acc: Int): Int =
      if q.isEmpty then acc
      else
        val (current, rem): (Path, Queue[Path]) = q.dequeue
        if current.pos == "end" then go(rem, acc + 1)
        else
          val next = for {
            c <- cave(current.pos)
            visits = current.seen.getOrElse(c, 0)
            maxVisits = if part2 && !current.seenTwice then 2 else 1
            if visits < maxVisits && c != "start"
            nextSeen = if c.forall(_.isLower) then current.seen.updated(c, visits + 1) else current.seen
          } yield Path(c, nextSeen, current.seenTwice || visits + 1 >= 2)
          go(rem.enqueueAll(next), acc)

    go(Queue(Path("start", Map.empty, false)), 0)


  private val answer1 = walk(input)(false)
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  private val answer2 = walk(input)(true)
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
