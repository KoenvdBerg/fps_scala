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

  def findNextStep(todo: Set[Char], deps: List[String]): List[Char] =
    todo.filter(step => deps.forall(s => s.last != step)).toList.sorted

  def findOrder(todo: Set[Char], deps: List[String], acc: String): String =
    if todo.isEmpty then acc                           // exit condition --> all steps finished
    else
      val next = findNextStep(todo, deps).head         // find available steps
      val nextsteps = todo.filter(_ != next)           // remove from steps to be done
      val nextdeps = deps.filter(p => p.head != next)  // remove from dependency list
      findOrder(nextsteps, nextdeps, acc + next)       // continue and append next to acc string

  private val answer1 = findOrder(input.mkString.toSet, input, "")
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  case class Worker(step: Option[Char], time: Int)
  def stepTime(s: Char): Int =
    s.toInt - 4
  def assignSteps(upnext: List[Char], workers: List[Worker], inprogress: List[Char]): List[Worker] =
      val (th, tt) = upnext.splitAt(1)        // head and tail of upnext
      val (wh, wt) = workers.splitAt(1)       // head and tail of workers
      if th.isEmpty then workers              // exit condition --> no more steps upnext
      else if workers.isEmpty then workers    // exit condition --> no more viable workers to assign to
      
      // in this case assign step to worker if worker is done (time == 0) 
      // and no other worker is working on that step already  
      else if wh.head.time == 0 & !inprogress.contains(th.head) then
        Worker(Some(th.head), stepTime(th.head)) :: assignSteps(tt, wt, inprogress)
      else
        wh.head :: assignSteps(upnext, wt, inprogress)   // skip assignation
  def simulate(time: Int, workers: List[Worker], todo: Set[Char], dependencies: List[String]): Int =

    val inprogress = workers.flatMap(worker => worker.step)

//    println(s"$time --> $workers working on $inprogress")

    // decrement time for every worker
    val updated_workers = workers.map(worker =>
      Worker(worker.step, (worker.time - 1).max(0)))

    // find finished workers
    val finished_workers = updated_workers.filter(worker =>
      worker.time == 0 & worker.step != None)

    // if the time is -1, that means that every worker does nothing. In that case, start
    // the process
    if time < 0 then
      val next = findNextStep(todo, dependencies)
      val newworkers = assignSteps(next, updated_workers, inprogress)
      simulate(time + 1, newworkers, todo, dependencies)

    // exit condition:
    else if todo.isEmpty & workers.forall(_.time <= 0) then time

    // the other case is that there are finished workers:
    else if finished_workers.nonEmpty then
      // removing finished steps from steps Set and dependencies List
      val finished_steps = finished_workers.flatMap(w => w.step)
      val nextsteps = todo.filter(uu => !finished_steps.contains(uu))
      val nextdeps = dependencies.filter(p => !finished_steps.contains(p.head))

      // finding the next steps available and distributing those steps:
      val next = findNextStep(nextsteps, nextdeps).filter(n => !inprogress.contains(n))
      val newworkers = assignSteps(next, updated_workers, inprogress)
      simulate(time + 1, newworkers, nextsteps, nextdeps)

    // if none is finished, then continue working:
    else
      simulate(time + 1, updated_workers, todo, dependencies)



  private val start2: Long =
    System.currentTimeMillis

  private val workers = Range(0,5).toList.map(x => Worker(None, 0))
  private val answer2 = simulate(-1, workers, input.mkString.toSet, input)
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
