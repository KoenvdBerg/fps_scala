package ai.paip

import ai.paip.SchoolStates.*
import ai.paip.SchoolActions.*

import scala.collection.mutable

case class Op(action: Action, preconditions: Seq[State], addToState: Seq[State], delFromState: Seq[State] = Seq.empty[State]):

  def isDependendOn(goal: State): Boolean = addToState.contains(goal)

class GPS(ops: Vector[Op], debug: Boolean = false):

  private val state: scala.collection.mutable.Set[State]   = scala.collection.mutable.Set.empty
  private val performedActions: mutable.ListBuffer[Action] = mutable.ListBuffer.empty[Action]

  def run(startingState: Set[State], goals: Set[State]): Vector[Action] =
    state.addAll(startingState)
    val performed = if goals.forall(achieve(Nil)) && goals.forall(state) then performedActions.toVector else Vector.empty
    performedActions.clear()
    state.clear()
    performed

  private def achieve(goalStack: List[State])(goal: State): Boolean =
    indentPrinter(goalStack.length, s"Goal: $goal")
    if state(goal) then true
    else if goalStack.contains(goal) then false
    else
      val filtered: Vector[Op] = ops.filter(_.isDependendOn(goal))
      filtered.nonEmpty && filtered.exists(applyOp(goal, goalStack))

  private def applyOp(goal: State, goalStack: List[State])(op: Op): Boolean =
    indentPrinter(goalStack.length, s"Considering: ${op.action}")
    val canPerformAction: Boolean = op.preconditions.forall(achieve(goal :: goalStack))
    if canPerformAction then
      indentPrinter(goalStack.length, s"Action: ${op.action}")
      performedActions.addOne(op.action)
      op.delFromState.foreach(d => state.remove(d))
      state.addAll(op.addToState)
    canPerformAction

  private def indentPrinter(l: Int, msg: String): Unit = if debug then println(s"${" " * l}$msg")


// -------------------------- DOMAINS ----------------------------
trait State
trait Action

// School Domain
enum SchoolActions extends Action:
  case DriveSonToSchool
  case GiveShopMoney
  case TelephoneShop
  case LookUpPhoneNumber
  case TellShopProblem
  case ShopInstallsBattery
  case AskPhoneNumber


enum SchoolStates extends State:
  case HavePhoneBook
  case HaveMoney
  case KnowPhoneNumber
  case SonAtSchool
  case SonAtHome
  case CarWorks
  case CarNeedsBattery
  case ShopKnowsProblem
  case InCommunicationWithShop
  case ShopHasMoney


object SchoolStates:

  val schoolOps: Vector[Op] = Vector(
    Op(DriveSonToSchool, Seq(SonAtHome, CarWorks), Seq(SonAtSchool), Seq(SonAtHome)),
    Op(ShopInstallsBattery, Seq(CarNeedsBattery, ShopKnowsProblem, ShopHasMoney), Seq(CarWorks)),
    Op(TellShopProblem, Seq(InCommunicationWithShop), Seq(ShopKnowsProblem)),
    Op(TelephoneShop, Seq(KnowPhoneNumber), Seq(InCommunicationWithShop)),
    Op(LookUpPhoneNumber, Seq(HavePhoneBook), Seq(KnowPhoneNumber)),  // comment this to obtain infinite loop
    Op(GiveShopMoney, Seq(HaveMoney), Seq(ShopHasMoney), Seq(HaveMoney)),
    Op(AskPhoneNumber, Seq(InCommunicationWithShop), Seq(KnowPhoneNumber))
  )

// Maze Domain

case class Move(from: Int, to: Int) extends Action
case class Loc(at: Int) extends State

object Maze:

  def makeMazeOp(from: Int, to: Int): Op = Op(
    action = Move(from, to),
    preconditions = Seq(Loc(from)),
    addToState = Seq(Loc(to)),
    delFromState = Seq(Loc(from)))
  def mazeOps: Vector[Op] =
    val coords: Vector[(Int, Int)] = Vector((1, 2) ,(2, 3),(3, 4) ,(4, 9) ,(9, 14),(9, 8),(8, 7),(7, 12),(12, 13),(12, 11),(11, 6),(11, 16) ,(16, 17) ,(17, 22),(22, 21),(22, 23),(23, 18),(23, 24) ,(24, 19) ,(19, 20) ,(20, 15) ,(15, 10) ,(10, 5) ,(20, 25))
    coords.flatMap((from, to) => Vector(makeMazeOp(from, to), makeMazeOp(to, from)))



@main def test_gps: Unit =

  val gps = new GPS(SchoolStates.schoolOps, true)
//  val result = gps.run(Set(SonAtHome, CarNeedsBattery, HaveMoney, HavePhoneBook), Set(SonAtSchool))
//  val r2 = gps.run(Set(SonAtHome, CarWorks), Set(SonAtSchool))
//  val r3 = gps.run(Set(SonAtHome, CarNeedsBattery, HaveMoney, HavePhoneBook), Set(HaveMoney, SonAtSchool))
//  val r4 = gps.run(Set(SonAtHome, CarNeedsBattery, HaveMoney, HavePhoneBook), Set(SonAtSchool, HaveMoney))
//  val r5 = gps.run(Set(SonAtHome, CarNeedsBattery, HaveMoney), Set(SonAtSchool))

  val mazeGPS = new GPS(Maze.mazeOps, true)
  println(mazeGPS.run(startingState = Set(Loc(1)), goals = Set(Loc(25))))
