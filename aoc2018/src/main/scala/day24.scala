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
      case s"$units units each with $hp hit points with an attack that does $atk $atkType damage at initiative $in" =>
        Group(units.toInt, hp.toInt, Nil, Nil, atk.toInt, atkType, in.toInt, side)
      case _ => {println(s);sys.error("boom")}

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

    def determineDmg(g: Group, opponent: Group): Int =
      if opponent.immune.contains(g.atkType) then 0
      else if opponent.weak.contains(g.atkType) then effectivePwr(g) * 2
      else effectivePwr(g)

    def attack(g: Group, opponent: Group): Group =
      val dmg: Int = determineDmg(g, opponent)
      val unitsKilled: Int = dmg / opponent.hp
      println(s"${g.initiative} ${g.side} attacked with $dmg (${g.atkType}) and killed $unitsKilled from ${opponent.initiative}")
      opponent.copy(units = opponent.units - unitsKilled)

    def selectionPhase(side: Army, opponents: Army): Vector[(Group, Option[Group])] =
      val groupInOrder: Vector[Group] = side  // sort based on effective power first, then initiative
        .toVector
        .map((g: Group) => (g, (effectivePwr(g), g.initiative)))
        .sortBy(_._2).reverse
        .map(_._1)

      @tailrec
      def go(orderedArmy: Vector[Group], opps: Army, acc: Vector[(Group, Option[Group])]): Vector[(Group, Option[Group])] = orderedArmy match
        case h +: t if opps.nonEmpty =>
          val selection: Group = opps.maxBy((opponent: Group) => (determineDmg(h, opponent), effectivePwr(opponent), opponent.initiative))
          if determineDmg(h, selection) > 0 then go(t, opps - selection, (h, Some(selection)) +: acc)
          else go(t, opps, (h, None) +: acc)
        case h +: t if opps.isEmpty =>  // in case that no opponents are left, remaining ally groups will fight None
          go(t, opps, (h, None) +: acc)
        case _ => acc

      go(groupInOrder, opponents, Vector.empty)


    def attackingPhase(selections: Vector[(Group, Option[Group])]): Army =
      val orderedAttack: Vector[(Group, Option[Group])] = selections
        .sortBy(_._1.initiative)
        .reverse                  // group with highest initiative attacks first

      @tailrec
      def doAttack(s: Vector[(Group, Option[Group])], acc: Army = Set.empty[Group]): Army = s match
        case h +: t => h match
          case (g, Some(opp)) if g.units > 0 => // attack is possible because units is positive integer
            val updatedOpp: Group = attack(g, opp)
            val oppIndex: Int = t.indexWhere(_._1.initiative == updatedOpp.initiative)
            if oppIndex <= -1 then
              // update the opponent in the acc because the opponent itself had already attacked
              doAttack(t, acc.filter(_.initiative != updatedOpp.initiative) + updatedOpp + g)
            else
               // here the opponent still will attack. It's updated so that if the opponent has les than 1 unit, it won't attack
              doAttack(t.updated(oppIndex, (updatedOpp, t(oppIndex)._2)), acc + g)
          case (g, _) if g.units <= 0 => doAttack(t, acc)
          case (g, None)      => doAttack(t, acc + g)
          case _              => sys.error("no attacker available")
        case _ => acc

      doAttack(orderedAttack).filter(_.units > 0)

    @tailrec
    def fight(imm: Army, inf: Army, n: Int = 0): Army =
      if imm.isEmpty then inf
      else if n >= 1 then imm
      else if inf.isEmpty then imm
      else
        //imm.foreach(g => println(s"imm: ${g.units} remaining for ${g.initiative}"))
        //inf.foreach(g => println(s"inf: ${g.units} remaining for ${g.initiative}"))
        val selImm: Vector[(Group, Option[Group])] = selectionPhase(imm, inf)
        val sellInf: Vector[(Group, Option[Group])] = selectionPhase(inf, imm)
        val fightResult: Army = attackingPhase(selImm ++ sellInf)

        println(fightResult.filter(_.side == "imm").mkString("\n"))

        fight(fightResult.filter(_.side == "imm"), fightResult.filter(_.side == "inf"), n + 1)

  // too low: 14722

  val res1: Army = Group.fight(imm, inf)
  private val answer1 = res1.map(_.units).sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  private val answer2 = None
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
