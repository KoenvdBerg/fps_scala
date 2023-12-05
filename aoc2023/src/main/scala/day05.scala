import day05.MapPart

import javax.tools.Tool
import scala.io.*
import math.*
import scala.annotation.tailrec

object day05 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis
    
  case class MapPart(dest: Long, source: Long, range: Long):
    
    def lookup(in: Long): Long =
      if in < source || in > source + range then in
      else
        in - (source - dest)
    
  private val (seeds, maps): (Vector[Long], Vector[Vector[MapPart]]) =
    
    def parseSeed(s: String): Vector[Long] =
      s.split("\\s").drop(1).map(_.trim.toLong).toVector
      
    def parseMaps(s: String): Vector[Vector[MapPart]] =
      s.split("\\n\\n")
        .toVector
        .map(ss => ss.split("\n").toVector.drop(1).map(m => 
          val map = m.trim.split("\\s").toVector
          MapPart(map(0).toLong, map(1).toLong, map(2).toLong)
        ))
    
    val in = Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList

    (parseSeed(in.head), parseMaps(in.drop(2).mkString("\n")))
    
  def processSeed(in: Long, chapter: Vector[MapPart]): Long =
    chapter.foldLeft((in, 0)){ (res: (Long, Int), mp: MapPart) =>
      if res._2 == 0 then 
        val l = mp.lookup(res._1)
        if l != res._1 then (l, 1)
        else (l, 0)
      else (res._1, 1)
    }._1
    
    
  private val res1 = seeds.map(s =>
    maps.foldLeft(s) { (res: Long, chapter: Vector[MapPart]) =>
      processSeed(res, chapter)
    }  
  )
  private val answer1 = res1.min
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  case class R(min: Long, max: Long):

    def applyOp(op: Long => Long): R = R(op(min), op(max))
    
    def overlap(that: R): Option[R] =
      if max < that.min then None
      else if min > that.max then None
      else Some(R(Vector(min, that.min).max, Vector(max, that.max).min))
      
    def filter(hits: Vector[R]): Vector[R] =
      if hits.isEmpty then Vector(this)
      else if hits.length == 1 then nonOverlap(hits.head) 
      else 
        val before = if min < hits.head.min then Some(R(min, hits.head.min - 1)) else None
        val after = if max > hits.last.max then Some(R(hits.last.max + 1, max)) else None
        val between = hits.sliding(2).toVector.flatMap(v =>
          val diff = v.last.min - v.head.max
          if diff <= 1 then None
          else Some(R(v.head.max + 1, v.last.min - 1))
        )
        
        Vector(before, after).flatten ++ between
      
    def nonOverlap(that: R): Vector[R] =
      if min >= that.min && max <= that.max then Vector.empty
      else if min < that.min && max > that.max then Vector(R(min, that.min -1), R(that.max + 1, max))
      else if min < that.min then Vector(R(min, that.min - 1))
      else Vector(R(that.max + 1, max))
        
    def merge(those: Vector[(R, Long => Long)]): Vector[R] =
      val hits: Vector[(R, Long => Long)] = those.flatMap((that, op) => overlap(that).map(r => (r, op)))
      val noHits: Vector[R] = filter(hits.map(_._1))      
      hits.map((r, op) => r.applyOp(op)) ++ noHits

  object R:
    def fromLongs(in: Vector[Long]): R = R(in.head, in.head + in.last - 1)

    def fromMap(m: MapPart): (R, Long => Long) =
      (R(m.source, m.source + m.range), (l: Long) => l - (m.source - m.dest))
      
    def processSeedR(r: R, chapter: Map[R, Long => Long]): Vector[R] = r.merge(chapter.toVector)

  
  val mapRanges: Seq[Map[R, Long => Long]] = maps.map(_.map(R.fromMap).toMap)
  val seedRanges: Vector[R] = seeds.grouped(2).toVector.map(R.fromLongs)
  
  val res2 = mapRanges.foldLeft(seedRanges) { (res: Vector[R], in: Map[R, Long => Long]) => 
    res.flatMap(r => R.processSeedR(r, in))
  }

  // below filtering out the 0 range because that's the last range to have no hit. The range right after that has the answer.
  val answer2 = res2.filter(_.min != 0).minBy(_.min).min
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")