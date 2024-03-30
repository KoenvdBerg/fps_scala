package ai.paip

import ai.paip.SchoolStates.*
import ai.paip.SchoolActions.*

// https://github.com/norvig/paip-lisp/tree/main

case class Op(action: Action, precons: List[State], addList: List[State], delList: List[State] = Nil):

  def isAppropriate(goal: State): Boolean = addList.contains(goal)

case class Tally(state: Set[State], done: Vector[State])

class GPS(ops: Vector[Op]):

  def gps(state: Set[State], goals: List[State]): Set[State] =
    // add remove-if #'atom
    achieveAll(state + Start, goals, List.empty[State])

  def achieveAll(state: Set[State], goals: List[State], goalStack: List[State]): Set[State] =
    val thisState: Set[State] = goals.flatMap(g => achieve(state, g, goalStack)).toSet
    if goals.forall(thisState) then thisState else Set.empty

  def achieve(state: Set[State], goal: State, goalStack: List[State]): Set[_ <: State] =
    if state.contains(goal) then state
    else if goalStack.contains(goal) then Set.empty
    else
      ops.filter(_.isAppropriate(goal)).flatMap(op => applyGoal(op, state, goal, goalStack)).toSet

  def applyGoal(op: Op, state: Set[State], goal: State, goalStack: List[State]): Set[State] =
    val nextState = achieveAll(state, op.precons, goal :: goalStack)
    if nextState.nonEmpty then
      println(s"doing: ${op.action}")
      state.diff(op.delList.toSet) ++ op.addList.toSet
    else nextState

trait State
trait Action

case object Start extends State

enum SchoolActions extends Action:
  case DriveSonToSchool(done: Boolean = false)
  case GiveShopMoney(done: Boolean = false)
  case TelephoneShop(done: Boolean = false)
  case LookUpPhoneNumber(done: Boolean = false)
  case TellShopProblem(done: Boolean = false)
  case ShopInstallsBattery(done: Boolean = false)


enum SchoolStates extends State:
  // dad states:
  case HavePhoneBook
  case HaveMoney
  case KnowPhoneNumber

  // actions:
//  case DriveSonToSchool
//  case GiveShopMoney
//  case TelephoneShop
//  case LookUpPhoneNumber
//  case TellShopProblem


  // son states:
  case SonAtSchool
  case SonAtHome

  // car states:
  case CarWorks
  case CarNeedsBattery

  // carshop states:
  case ShopKnowsProblem
//  case ShopInstallsBattery
  case InCommunicationWithShop
  case ShopHasMoney


object SchoolStates:

  val schoolOps: Vector[Op] = Vector(
    Op(DriveSonToSchool(), List(SonAtHome, CarWorks), List(SonAtSchool), List(SonAtHome)),
    Op(ShopInstallsBattery(), List(CarNeedsBattery, ShopKnowsProblem, ShopHasMoney), List(CarWorks)),
    Op(TellShopProblem(), List(InCommunicationWithShop), List(ShopKnowsProblem)),
    Op(TelephoneShop(), List(KnowPhoneNumber), List(InCommunicationWithShop)),
    Op(LookUpPhoneNumber(), List(HavePhoneBook), List(KnowPhoneNumber)),
    Op(GiveShopMoney(), List(HaveMoney), List(ShopHasMoney), List(HaveMoney))
  )



@main def test_gps: Unit =

  val gps = GPS(SchoolStates.schoolOps).gps(Set(SonAtHome, CarNeedsBattery, HaveMoney, HavePhoneBook), List(SonAtSchool))
  println(gps)




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