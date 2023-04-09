import java.util.Date
import scala.io.*
import scala.util.matching.Regex

object day04 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: List[String] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList

  case class Agenda(date: Int, time: Int, min: Int, rem: String)
  case class Schedule(guard: Int, start: Int, end: Int)
  case class Hit(id: Int, vals: List[Int])

  def agendaFromString(gl: String): Agenda =
    val dtt = gl.take(18).split(" ")
    val d = dtt.head.substring(1).split("-").mkString
    val h = dtt.last.take(2)
    val m = dtt.last.takeRight(3).take(2)
    val rem = gl.split("]").last
    Agenda(d.toInt, (h+m).toInt, m.toInt, rem)

  def makeSleepSchedule(agl: List[Agenda]): List[Schedule] =
    def go(agll: List[Agenda], s: Int, e: Int, guard: Int, acc: List[Schedule]): List[Schedule] = agll match
      case Nil => acc ::: List(Schedule(guard, s, e))
      case h :: t =>
        if s > -1 & e > -1 then
          go(agll, -1, -1, guard, acc ::: List(Schedule(guard = guard, start = s, end = e)))
        else if h.rem.contains("Guard") then
          val guard = h.rem.split(" ")(2).substring(1).toInt
          go(t, -1, -1, guard, acc)
        else if h.rem.contains("asleep") then
          val s = h.min
          go(t, s, -1, guard, acc)
        else
          val e = h.min
          go(t, s, e, guard, acc)
    go(agl, -1, -1, -1, Nil)

  val agendas = input.map(agendaFromString(_)).sortBy(r => (r.date, r.time))
  val schedules = makeSleepSchedule(agendas)
  val biggestsleeper = schedules.groupBy(_.guard).map((g, y) => Vector(y.map(s => s.end - s.start).sum, g)).max
  val freqs = schedules.groupBy(_.guard).map((x, y) => Hit(x, y.map(r => Range(r.start, r.end)).flatMap(_.toList).groupBy(identity).map((x, y) => List(y.size, x)).max))
  val res1 = freqs.filter(_.id == biggestsleeper.last).head
  private val answer1 = res1.id * res1.vals(1)
  println(s"Answer day $day part 1: ${res1.id} * ${res1.vals(1)} = ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long = System.currentTimeMillis
  private val res2 = freqs.maxBy(_.vals.head)
  private val answer2 = res2.id * res2.vals.last
  println(s"Answer day $day part 2: ${res2.id} * ${res2.vals.last} = ${answer2} [${System.currentTimeMillis - start2}ms]")
