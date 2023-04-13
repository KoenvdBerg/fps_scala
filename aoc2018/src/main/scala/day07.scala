import scala.io.*
import math.*

object day07 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val input: List[String] =
    Source
      .fromResource(s"day$day.txt")
      .getLines
      .map {
        case s"Step ${v1} must be finished before step ${v2} can begin." => v1 + v2
      }
      .toList

  def findNextStep(steps: Set[Char], pairs: List[String]): String =
    steps.filter(step => pairs.forall(s => s.last != step)).mkString.sorted

  def findOrder(steps: Set[Char], pairs: List[String], acc: String): String =
    if steps.isEmpty then acc
    else
      val next = findNextStep(steps, pairs).head
      val nextsteps = steps.filter(_ != next)
      val nextpairs = pairs.filter(p => p.head != next)
      println(s"$nextsteps <> $next --> $acc + $next")
      findOrder(nextsteps, nextpairs, acc + next)

  private val answer1 = findOrder(input.mkString.toSet, input, "")
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  case class Worker(step: String, time: Int)

  def stepTime(s: Char): Int =
    (s.toInt - 65 + 1) // + 60

  def assignSteps(s: String, ws: List[Worker]): List[Worker] =
    val (sh, st) = s.splitAt(1)
    val (wh, wt) = ws.splitAt(1)
    if sh == "" then ws
    else if ws.length <= 0 then ws
    else
      if wh.head.time == 0 then
        Worker(sh, stepTime(sh.head)) :: assignSteps(st, wt)
      else
        wh.head :: assignSteps(s, wt)


  def simulate(time: Int, workers: List[Worker], steps: Set[Char], pairs: List[String]): Int =
    if steps.isEmpty & workers.forall(_.time <= 0) then time
    else

      println(s"$time: $workers with $steps")

      // decrement time:
      val updated_workers = workers.map(worker =>
        Worker(worker.step, (worker.time-1).max(0)))

      // handle completed steps:
      val finished_workers = updated_workers.filter(worker =>
        worker.time == 0 & worker.step != "")

      if finished_workers.nonEmpty then

        // remove completed steps from uncompleted steps Set and
        // remove the completed steps from the dependencies (in pairs)
        val next = findNextStep(steps, pairs)
        val nextsteps = steps.filter(!next.contains(_))
        val nextpairs = pairs.filter(p => !next.contains(p.head.toString))

        // handle giving new steps to finished workers:
        val newworkers = assignSteps(next, updated_workers)
        simulate(time+1, newworkers, nextsteps, nextpairs)

      else if updated_workers.forall(w => w.time == 0 & w.step == "") then
        val next = findNextStep(steps, pairs)
        val newworkers = assignSteps(next, updated_workers)
        val nextsteps = steps.filter(!next.contains(_))
        val nextpairs = pairs.filter(p => !next.contains(p.head.toString))
        simulate(time + 1, newworkers, nextsteps, nextpairs)

      else
        // handle giving new steps to finished workers:
        simulate(time+1, updated_workers, steps, pairs)



  private val start2: Long =
    System.currentTimeMillis

  private val workers = Range(0,2).toList.map(x => Worker("", 0))
  private val answer2 = simulate(-1, workers, input.mkString.toSet, input)
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")



//  def findFirstNode(cs: List[String]): String = cs match
//    case h :: t =>
//      val tr = t.filter(r => h.head == r.last)
//      if tr.length >= 1 then findFirstNode(t)
//      else h
//    case Nil => ""
//
//  def makeChain(cs: List[String]): String =
//    def go(css: List[String], acc: String): String =
//      if css.length <= 0 then acc
//      else
//        val f = findFirstNode(css)
//        val insertLoc = acc.indexOf(f.head)
//        val tt = if insertLoc < acc.length - 1 then
//          if acc.contains(f.last) then
//            acc
//          else
//            val fn = acc.replace(s"${f.head}", f)
//            val sl = fn.slice(insertLoc+1, insertLoc+3)
//            fn.replace(sl, sl.sorted)
//        else
//          val fn = acc.replace(s"${f.last}", "").replace(s"${f.head}", f)
//          fn
//        println(s"$acc :: $f :: $insertLoc --> $css => $tt")
//        go(css.filter(_ != f), tt)
//
//    val first = findFirstNode(cs)
//    go(cs.filter(_ != first), first)

// IDEA: sort and do per letter and make a chain for that letter only. See if this gives possibilites
//
//def chainPerChar(cs: List[String]): String =
//  if cs.isEmpty then ""
//  else
//    println(s"${cs.head.last} --> $cs")
//    cs.map(_.head).mkString.sorted + cs.head.last
//
//def makeCharChains(ca: String, cs: List[String]): List[String] =
//  val x = ca.map(f => cs.filter(_.last == f))
//  x.map(chainPerChar(_)).toList


//def makeChain2(cs: List[String], avs: String, acc: String): String =
//  if avs == "" then acc
//  else
//    // taking the first of the available items to process next
//    val (av, rem) = avs.sorted.splitAt(1)
//
//    // removing already available items from list
//    val csnext = cs.filter(d => d.head.toString != av)
//
//    // finding the next available items
//    val find = cs.filter(d => av == d.head.toString)
//    val nextav = find.map(_.last).mkString
//
//    // checking if available items are really available and don't have other dependencies
//    val nextavtrue = nextav.filter(av => !csnext.map(_.last).contains(av))
//
//    //      println(s"$av and $rem --> $rem + $nextavtrue ||| $csnext")
//    makeChain2(csnext, (rem + nextavtrue).toSet.mkString, av + acc)
//
//def simulate(time: Int, workers: List[Worker], steps: Set[Char], pairs: List[String]): Int =
//  if steps.isEmpty & workers.forall(_.time <= 0) then time
//  else
//
//    println(s"$time: $workers with $steps")
//
//
//    // decrement time:
//    val updated_workers = workers.map(worker =>
//      Worker(worker.step, (worker.time - 1).max(0)))
//
//    // handle completed steps:
//    val finished_workers = updated_workers.filter(worker =>
//      worker.time == 0 & worker.step != "")
//
//    if finished_workers.nonEmpty then
//
//      // remove completed steps from uncompleted steps Set and
//      // remove the completed steps from the dependencies (in pairs)
//      val next = findNextStep(steps, pairs)
//      val nextsteps = steps.filter(!next.contains(_))
//      val nextpairs = pairs.filter(p => !next.contains(p.head.toString))
//
//      // handle giving new steps to finished workers:
//      val newworkers = assignSteps(next, finished_workers) ++ updated_workers.filter(w => w.step == "")
//      println(s"$next, $newworkers")
//      simulate(time + 1, newworkers, nextsteps, nextpairs)
//
//    else if updated_workers.forall(w => w.time == 0 & w.step == "") then
//      val next = findNextStep(steps, pairs)
//      val newworkers = assignSteps(next, updated_workers)
//      val nextsteps = steps.filter(!next.contains(_))
//      val nextpairs = pairs.filter(p => !next.contains(p.head.toString))
//      simulate(time + 1, newworkers, nextsteps, nextpairs)
//    else
//      simulate(time + 1, updated_workers, steps, pairs)
