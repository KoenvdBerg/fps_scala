package ai.paip

import ai.paip.SchoolStates.*
import ai.paip.SchoolActions.*

import scala.collection.mutable

// https://github.com/norvig/paip-lisp/tree/main

case class Op(action: Action, precons: Set[State], addList: Set[State], delList: Set[State] = Set.empty[State]):

  def isAppropriate(goal: State): Boolean = addList.contains(goal)

class GPS(ops: Vector[Op], debug: Boolean = false):

  private val state: scala.collection.mutable.Set[State] = scala.collection.mutable.Set.empty
  private val done: mutable.ListBuffer[Action] = mutable.ListBuffer.empty[Action]

  def run(startingState: Set[State], goals: Set[State]): Vector[Action] =
    state.addAll(startingState)
    val performed = if goals.forall(achieve(Nil)) && goals.forall(state) then done.toVector else Vector.empty
    done.clear()
    state.clear()
    performed

  def achieve(goalStack: List[State])(goal: State): Boolean =
    indentPrinter(goalStack.length, s"Goal: $goal")
    if state.contains(goal) then true
    else if goalStack.contains(goal) then false
    else
      val filtered = ops.filter(_.isAppropriate(goal))
      filtered.forall(applyOp(goal, goalStack)) && filtered.nonEmpty

  def applyOp(goal: State, goalStack: List[State])(op: Op): Boolean =
    indentPrinter(goalStack.length, s"Considering: ${op.action}")
    if op.precons.forall(achieve(goal :: goalStack)) then
      done.addOne(op.action)
      indentPrinter(goalStack.length, s"Action: ${op.action}")
      op.delList.foreach(d => state.remove(d))
      state.addAll(op.addList)
      true
    else false

  private def indentPrinter(l: Int, msg: String): Unit = if debug then println(s"${" " * l}$msg")

trait State
trait Action

case object Start extends State

enum SchoolActions extends Action:
  case DriveSonToSchool
  case GiveShopMoney
  case TelephoneShop
  case LookUpPhoneNumber
  case TellShopProblem
  case ShopInstallsBattery
  case AskPhoneNumber


enum SchoolStates extends State:
  // dad states:
  case HavePhoneBook
  case HaveMoney
  case KnowPhoneNumber

  // son states:
  case SonAtSchool
  case SonAtHome

  // car states:
  case CarWorks
  case CarNeedsBattery

  // carshop states:
  case ShopKnowsProblem
  case InCommunicationWithShop
  case ShopHasMoney


object SchoolStates:

  val schoolOps: Vector[Op] = Vector(
    Op(DriveSonToSchool, Set(SonAtHome, CarWorks), Set(SonAtSchool), Set(SonAtHome)),
    Op(ShopInstallsBattery, Set(CarNeedsBattery, ShopKnowsProblem, ShopHasMoney), Set(CarWorks)),
    Op(TellShopProblem, Set(InCommunicationWithShop), Set(ShopKnowsProblem)),
    Op(TelephoneShop, Set(KnowPhoneNumber), Set(InCommunicationWithShop)),
    Op(LookUpPhoneNumber, Set(HavePhoneBook), Set(KnowPhoneNumber)),  // comment this to obtain infinite loop
    Op(GiveShopMoney, Set(HaveMoney), Set(ShopHasMoney), Set(HaveMoney)),
    Op(AskPhoneNumber, Set(InCommunicationWithShop), Set(KnowPhoneNumber))
  )



@main def test_gps: Unit =

  val gps = GPS(SchoolStates.schoolOps, true)
//  val result = gps.run(Set(SonAtHome, CarNeedsBattery, HaveMoney, HavePhoneBook), Set(SonAtSchool))

//   val r2 = gps.run(Set(SonAtHome, CarWorks), Set(SonAtSchool))
//  val r3 = gps.run(Set(SonAtHome, CarNeedsBattery, HaveMoney, HavePhoneBook), Set(HaveMoney, SonAtSchool))
//val r4 = gps.run(Set(SonAtHome, CarNeedsBattery, HaveMoney, HavePhoneBook), Set(SonAtSchool, HaveMoney))
  val r5 = gps.run(Set(SonAtHome, CarNeedsBattery, HaveMoney), Set(SonAtSchool))
  println(r5)
  // println(s"Having done: $result")




/**
class GPS(ops: Vector[Op]):

  def gps(state: Set[Domain], goals: List[Domain]): Set[Domain] =
    // add remove-if #'atom
    achieveAll(state + Start, goals, List.empty[Domain])

  def achieveAll(state: Set[Domain], goals: List[Domain], goalStack: List[Domain]): Set[Domain] =
    val thisState: Set[Domain] = goals.flatMap(g => achieve(state, g, goalStack)).toSet
    if goals.forall(thisState) then thisState else Set.empty

  def achieve(state: Set[Domain], goal: Domain, goalStack: List[Domain]): Set[_ <: Domain] =
    if state.contains(goal) then state
    else if goalStack.contains(goal) then Set.empty
    else
      ops.filter(_.isAppropriate(goal)).flatMap(op => applyGoal(op, state, goal, goalStack)).toSet

  def applyGoal(op: Op, state: Set[Domain], goal: Domain, goalStack: List[Domain]): Set[Domain] =
    val nextState = achieveAll(state, op.precons, goal :: goalStack)
    if nextState.nonEmpty then
      println(s"doing: ${op.action}")
      state.diff(op.delList.toSet) ++ op.addList.toSet
    else nextState
**/