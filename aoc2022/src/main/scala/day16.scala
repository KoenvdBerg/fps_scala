import scala.io.*
import aoc2022.Algorithms.GraphTraversal.*

import scala.collection.immutable.Queue

/**
 * 
 * PART 01:
 * 
 * I used Dijkstra to get a graph of all the locations of valves that matter (i.e. have a flow rate > 0). This allowed
 * the exploreValves() function to quickly walk through the valves. This function walks all the possible routes and 
 * returns all the end Nodes that track the cumulative score. The function halts if all the valves are opened or if 
 * the remaining time is 0. Also, Nodes that have a lower score than already existing nodes and fewer time left are 
 * trimmed.
 * 
 * PART 02:
 * 
 * Simply used the algorithm of part01 twice, leaving out the opened valves from the first iteration. 
 * 
 */


object day16 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val (routes, valves): (Map[String, List[String]], Map[String, Int]) =

    case class Route(from: String, to: List[String])
    case class Valve(loc: String, rate: Int)
  
    def parse(s: String): (Valve, Route) = s match
      case s"Valve $valve has flow rate=$rate; tunnels lead to valves $routes" => (Valve(valve, rate.toInt), Route(valve, routes.split(',').map(_.trim).toList))
      case s"Valve $valve has flow rate=$rate; tunnel leads to valve $route"   => (Valve(valve, rate.toInt), Route(valve, List(route)))
      case _ => sys.error(s"couldn't parse valve string: $s")

    val infile: List[(Valve, Route)] = Source
      .fromResource(s"day$day.txt")
      .getLines
      .toList
      .map(parse)

    val routes: Map[String, List[String]] = infile.map((_, r) => (r.from, r.to)).toMap
    val valves: Map[String, Int] = infile.map((v, _) => (v.loc, v.rate)).toMap
    (routes, valves)

  case class Node(loc: String, time: Int, cumScore: Int, open: Set[String]):

    def openValve(valves: Map[String, Int]): Node =
      valves.get(loc) match
        case None => this
        case Some(r) => copy(cumScore = cumScore + r * (time - 1), open = open + loc, time = time - 1)
  
  def routeGraph(routes: Map[String, List[String]]): Graph[String] = (loc: String) => routes(loc).map(_ -> 1).toMap
        
  def exploreValves(valves: Map[String, Int], waypoints: Graph[String], start: Node): List[Node] =

    def go(queue: Queue[Node], acc: List[Node]): List[Node] =
      if queue.isEmpty then acc
      else
        val (n, further): (Node, Queue[Node]) = queue.dequeue
        if n.open.size == valves.size then go(further, n :: acc)  // all valves open, stop and add to acc
        else if n.time <= 0 then go(further, n :: acc)            // no time left, stop and add to acc
        else
          val openValve: Node = n.openValve(valves)  // always open the valve at current loc
          val toExplore: Set[String] = valves.keySet.diff(openValve.open)  // next valves to check
          if toExplore.isEmpty then go(further.enqueue(openValve), acc)    // in case this was the last possible valve to open
          else if further.exists((t: Node) => t.cumScore > openValve.cumScore && t.time > openValve.time) then go(further, acc)  // skip this one 
          else 
            val next: List[Node] = toExplore.map((tgt: String) =>
              val timeRequired: Int = shortestDistance(waypoints)(openValve.loc, tgt).getOrElse(sys.error(s"couldn't compute distance for: $openValve and $tgt"))
              openValve.copy(loc = tgt, time = openValve.time - timeRequired)).toList
            go(further.enqueueAll(next), acc)
            
    go(Queue(start), List.empty)
    
    
  private val waypoints: Graph[String] = routeGraph(routes)
  private val valvesThatMatter: Map[String, Int] = valves.filter(_._2 != 0)
  private val scores: List[Node] = exploreValves(valvesThatMatter, waypoints, Node("AA", 30, 0, Set.empty[String]))
  private val answer1: Node = scores.maxBy(_.cumScore)
  println(s"Answer day $day part 1: ${answer1.cumScore} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis
    
  private val scores2a: List[Node] = exploreValves(valvesThatMatter, waypoints, Node("AA", 26, 0, Set.empty))
  private val elephant: Node = scores2a.maxBy(_.cumScore)
  private val scores2b: List[Node] = exploreValves(valvesThatMatter.filterNot(v => elephant.open.contains(v._1)), waypoints, Node("AA", 26, 0, Set.empty))
  private val elve: Node = scores2b.maxBy(_.cumScore)
  private val answer2 = elve.cumScore + elephant.cumScore
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
