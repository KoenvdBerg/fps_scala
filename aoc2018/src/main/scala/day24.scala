import scala.io.*
import math.*
import scala.annotation.tailrec

/**
 *
 *
 */


object day24 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val start1: Long =
    System.currentTimeMillis

  private val (imm, inf): (Army, Army) =

    def parseGroup(s: String, side: String): Group = s match
      case s"$units units each with $hp hit points $defence with an attack that does $atk $atkType damage at initiative $in" =>
        Group(units.toInt, hp.toInt, parseDefence(defence)._1, parseDefence(defence)._2, atk.toInt, atkType, in.toInt, side)
      case _ => sys.error("boom")

    def processDef(s: String): List[String] =
      s.split(',').toList.map(_.trim)

    def parseDefence(s: String): (List[String], List[String]) = s match
      case s"(immune to $imms; weak to $weaks)"  => (processDef(imms), processDef(weaks))
      case s"(weak to $weaks)" => (Nil, processDef(weaks))
      case s"(immune to $imms)" => (processDef(imms), Nil)
      case _ => (Nil, Nil)

    val lines: Vector[String] = Source
      .fromResource(s"day$day.txt")
      .getLines
      .toVector
      .filter(_.nonEmpty)

    val i: Int = lines.indexWhere(_ == "Infection:")
    val (imm, inf): (Vector[String], Vector[String]) = lines.splitAt(i)
    (
      imm.drop(1).map((s: String) => parseGroup(s, "imm")).toSet,
      inf.drop(1).map((s: String) => parseGroup(s, "inf")).toSet
    )

  type Army = Set[Group]

  case class Group(units: Int, hp: Int, immune: List[String],
                   weak: List[String], atk: Int, atkType: String, initiative: Int, side: String)

  object Group:

    def effectivePwr(g: Group): Int = g.atk * g.units

    def determineAtk(g: Group, opponent: Group): Int =
      if opponent.immune.contains(g.atkType) then 0
      else if opponent.weak.contains(g.atkType) then effectivePwr(g) * 2
      else effectivePwr(g)

    def makeSelection(opponents: Army)(strategy: Group => Int): Army =
      val strat: Set[(Group, Int)] = opponents.map((g: Group) => (g, strategy(g)))
      val m: Int = strat.maxBy(_._2)._2
      strat.filter(_._2 == m).map(_._1)

    def selectTarget(g: Group, opponents: Army): Option[Group] =
      val strat1: Army = makeSelection(opponents)(determineAtk.curried(g))
      if strat1.size <= 0 then None
      else if strat1.size == 1 then Some(strat1.head)
      else
        val strat2: Army = makeSelection(strat1)(effectivePwr)
        if strat2.size == 1 then Some(strat2.head)
        else
          Some(makeSelection(strat2)((g: Group) => g.initiative).head)

    def selectionPhase(side: Army, opponents: Army): Vector[(Group, Group)] =
      val groupInOrder: Vector[Group] = side  // sort based on effective power first, then initiative
        .toVector
        .map((g: Group) => (g, (effectivePwr(g), g.initiative)))
        .sortBy(_._2).reverse
        .map(_._1)

      @tailrec
      def go(orderedArmy: Vector[Group], opps: Army, acc: Vector[(Group, Group)]): Vector[(Group, Group)] = orderedArmy match
        case h +: t if opps.nonEmpty =>
          val selection: Option[Group] = selectTarget(h, opps)
          selection match
            case Some(sel) => go(t, opps - sel, (h, sel) +: acc)
            case None    => go(t, opps, acc)
        case _ => acc

      go(groupInOrder, opponents, Vector.empty)

  // TODO: implement attacking phase
  // TODO: groups attack in decreasing order of initiative, ordered over all sides
  // TODO: if group dies during attacking phase, remove it can no longer attack
  // TODO: units killed = dmg / hp_per_unit


  val test = Group.selectionPhase(imm, inf)
  println(test.mkString("\n\n"))

  val test2 = Group.selectionPhase(inf, imm)
  println("###################")
  println(test2.mkString("\n\n"))

  private val answer1 = None
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  private val answer2 = None
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
