import scala.io.*
import math.*
import scala.annotation.tailrec

/**
 * PART 01:
 *
 * I spent a really long time looking for a bug, as the answer was wrong each time. Turns out that the parsing of the
 * input went slightly wrong. The weakness and the immunity can be swapped between the () of the group string.
 *
 * The remainder of the puzzle was simply following the puzzle specifications as in the description.
 *
 * PART 02:
 *
 * This was a straightforward search for encountering the lowest boost for the immune army to win. Due to having optimised
 * code for part 01, this was a simple implementation of a brute force method (see code below).
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
      case s"(weak to $weaks; immune to $imms)"  => (processDef(imms), processDef(weaks))
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
      opponent.copy(units = opponent.units - unitsKilled)

    def selectionPhase(army: Army): Map[Group, Group] =
      @tailrec
      def go(orderedArmy: List[Group], allGroups: Army, acc: Map[Group, Group]): Map[Group, Group] = orderedArmy match
        case h :: t =>
          val opponents: Army = allGroups.filter(_.side != h.side)
          if opponents.isEmpty then go(t, allGroups, acc) // in case that no opponents are left, remaining ally groups won't fight
          else
            val selection: Group = opponents.maxBy((opp: Group) => (determineDmg(h, opp), effectivePwr(opp), opp.initiative))
            if determineDmg(h, selection) > 0 then go(t, allGroups - selection, acc + (h -> selection))
            else go(t, allGroups, acc)
        case Nil                     => acc

      val groupInOrder: List[Group] = army // sort based on effective power first, then initiative
        .toList
        .sortBy((g: Group) => (-effectivePwr(g), -g.initiative))
      go(groupInOrder, army, Map.empty[Group, Group])


    def attackingPhase(selections: Map[Group, Group], all: Army): Army =

      @tailrec
      def doAttack(attackers: List[Group], opponents: Map[Group, Group], allGroups: Army): Army = attackers match
        case h :: t =>
          opponents.get(h) match
            case None      => doAttack(t, opponents, allGroups)
            case Some(opp) if h.units > 0 =>
              val updatedOpp: Group = attack(h, opp)
              if updatedOpp.units > 0 then
                doAttack(
                  t.map((g: Group) => if g == opp then updatedOpp else g),
                  if opponents.contains(opp) then opponents - opp + (updatedOpp -> opponents(opp)) else opponents,
                  allGroups - opp + updatedOpp
                )
              else  // opponent died
                doAttack(
                  t.filter((g: Group) => g != opp),
                  opponents - opp,
                  allGroups - opp
                )
            case _ => sys.error("CANNOT PROCESS ATTACK")
        case Nil => allGroups


      val orderedAttack: List[Group] = selections
        .keys.toList
        .sortBy(-_.initiative) // group with highest initiative attacks first
      doAttack(orderedAttack, selections, all)

    @tailrec
    def fight(army: Army, n: Int = 0): (String, Army) =
      if !army.exists(_.side == "inf") then ("imm won", army)
      else if !army.exists(_.side == "imm") then ("inf won", army)
      else if n >= 20000 then ("stalemate", Set.empty[Group])
      else
        val selections: Map[Group, Group] = selectionPhase(army)
        val fightResult: Army = attackingPhase(selections, army)
        fight(fightResult, n + 1)

  val (_, res1): (String, Army) = Group.fight(imm ++ inf)
  private val answer1: Int = res1.map(_.units).sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  @tailrec
  def searchWinForImm(boost: Int): Army =
    val boostedImm: Army = imm.map((g: Group) => g.copy(atk = g.atk + boost))
    val (outcome, winner): (String, Army) = Group.fight(boostedImm ++ inf)
    outcome match
      case "imm won"   => winner
      case "inf won"   => searchWinForImm(boost + 1)
      case "stalemate" => searchWinForImm(boost + 1)
      case _           => sys.error("Couldn't find a win for Imm")


  val res2: Army = searchWinForImm(30)
  val answer2: Int = res2.map(_.units).sum
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
