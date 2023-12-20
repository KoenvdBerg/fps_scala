import scala.io.*
import math.*
import scala.annotation.tailrec
import scala.collection.immutable.Queue

object day20 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis


  case class Pulse(receiver: String, signal: String, sender: String)

  enum Module:
    case FlipFlop(name: String, state: String)
    case Conjunction(name: String, state: Map[String, String])
    case Broadcast(name: String)

    def getState: Option[Map[String, String]] = this match
      case Conjunction(n, st) => Some(st)
      case _ => None

    def receivePulse(p: Pulse): Module = this match
      case FlipFlop(n, st) =>
        if p.signal == "high" then this
        else
          if st == "off" then FlipFlop(n, "on")
          else FlipFlop(n, "off")
      case Conjunction(n, st) =>
        val nextState = st.updated(p.sender, p.signal)
        Conjunction(n, nextState)
      case b: Broadcast => b

    def sentPulse: String = this match
      case FlipFlop(n, st) =>
        if st == "off" then "high" else "low"
      case Conjunction(n, st) =>
        if st.values.forall(_ == "high") then "low" else "high"
      case b: Broadcast => "low"

  object Module:
    val (modules, network): (Map[String, Module], Map[String, List[String]]) =

      def parseMap(s: String): (String, List[String]) = s match
        case s"broadcaster -> $output" => ("broadcaster", output.split(",").toList.map(_.trim))
        case s"$name -> $output" => (name.drop(1), output.split(",").toList.map(_.trim))
        case _ => sys.error(s"cannot parse $s")

      def parseState(s: String): (String, Module) = s match
        case s"broadcaster -> $output" => ("broadcaster", Broadcast("broadcaster"))
        case s"$name -> $output" if name.contains("%") => (name.drop(1), FlipFlop(name.drop(1), "off"))
        case s"$name -> $output" if name.contains("&") => (name.drop(1), Conjunction(name.drop(1), Map.empty[String, String]))
        case _ => sys.error(s"cannot parse $s")

      val in = Source
        .fromResource(s"day$day.txt")
        .getLines
        .toList

      (in.map(parseState).toMap, in.map(parseMap).toMap)

    def determineConjunctionStartStates(mods: Map[String, Module], wiring: Map[String, List[String]]): Map[String, Module] =
      val conjunctions: Iterable[String] = mods.filter((s, m) => m match
        case Conjunction(_, _) => true
        case _ => false
      ).keys
      val dependencies: Iterable[(String, Iterable[String])] = conjunctions.map(c => c -> wiring.filter((name, to) => to.contains(c)).keys)
      dependencies.foldLeft(mods) {(res: Map[String, Module], in: (String, Iterable[String])) =>
        val startState: Module = Conjunction(in._1, in._2.map(s => s -> "low").toMap)
        res.updated(in._1, startState)
      }

    case class Result(low: Long, high: Long):
      def add(signal: String, n: Int): Result = if signal == "low" then copy(low = low + n) else copy(high = high + n)
      def +(that: Result): Result = Result(low + that.low, high + that.high)

    def pressButton(mods: Map[String, Module], wiring: Map[String, List[String]], interest: Option[String]): (Map[String, Module], Result) =

      @tailrec
      def go(queue: Queue[Pulse], states: Map[String, Module], res: Result): (Map[String, Module], Result) =

        if queue.isEmpty then (states, res)
        else
          val (q, nextq) = queue.dequeue
          if !states.contains(q.receiver) then go(nextq, states, res)
          else if interest.isDefined && states("mf").getState.get(interest.get) == "high" then (states, res)
          else
            val updated: Map[String, Module] = states.updated(q.receiver, states(q.receiver).receivePulse(q))
            val toSent: Option[String] = states(q.receiver) match
              case FlipFlop(_, _) if q.signal == "high" => None
              case FlipFlop(_, _) if q.signal == "low" => Some(states(q.receiver).sentPulse)
              case _              => Some(updated(q.receiver).sentPulse)
            if toSent.isEmpty then go(nextq, updated, res)
            else
              val nextReceivers: List[String] = wiring(q.receiver)
              val nextQueue: Queue[Pulse] = nextq.enqueueAll(nextReceivers.map(s => Pulse(s, toSent.get, q.receiver)))
              go(nextQueue, updated, res.add(toSent.get, nextReceivers.length))

      val startPulse: Queue[Pulse] = Queue(Pulse("broadcaster", "low", "button"))
      go(startPulse, mods, Result(1, 0))


    def pressButtonN(mods: Map[String, Module], wiring: Map[String, List[String]], N: Int): Result =

      @tailrec
      def loop(i: Int, res: Result, state: Map[String, Module]): Result =
        if i >= N then res
        else
          val (nextState: Map[String, Module], nextRes: Result) = pressButton(state, wiring, None)
          loop(i+1, res + nextRes, nextState)

      loop(0, Result(0L, 0L), mods)

    def pressButtonGotcha(mods: Map[String, Module], wiring: Map[String, List[String]], interest: String): Long =

      @tailrec
      def loop(i: Long, state: Map[String, Module]): Long =
        val (nextState, _) = pressButton(state, wiring, Some(interest))
        if nextState("mf").getState.get(interest) == "high" then i
        else loop(i + 1, nextState)

      loop(1, mods)

  val test: Map[String, Module] = Module.determineConjunctionStartStates(Module.modules, Module.network)
  private val res1: Module.Result = Module.pressButtonN(test, Module.network, 1000)
  private val answer1: Long = res1.low * res1.high
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  private val start2: Long =
    System.currentTimeMillis

  def overlap(in: Long, that: Long): Long =

    @tailrec
    def go(i: Int): Long =
      val t: Long = in * i
      if t % that == 0 then i
      else go(i + 1)

    go(1) * in

  // note: "mf" is the producer of rx.
  private val conj: String = Module.network.filter((_, la) => la.contains("rx")).keys.head
  private val starts: Iterable[String] = test(conj).getState.get.keys
  private val res2: Iterable[Long] = starts.map(s => Module.pressButtonGotcha(test, Module.network, s))
  private val answer2: Long = res2.foldLeft(1L) { (res: Long, in: Long) => overlap(res, in) }
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
