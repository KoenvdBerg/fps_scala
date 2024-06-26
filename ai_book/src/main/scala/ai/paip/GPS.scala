package ai.paip

import ai.paip.SchoolStates.*
import ai.paip.SchoolActions.*
import ai.paip.Block.*
import Gas.*
import GasStates.*

import scala.collection.mutable

case class Op(action: Action, preconditions: Seq[State], addToState: Seq[State], delFromState: Seq[State] = Seq.empty[State])

class GPS(ops: Seq[Op], debug: Boolean = false):

  private val state: scala.collection.mutable.Set[State]   = scala.collection.mutable.Set.empty
  private val performedActions: mutable.ListBuffer[Action] = mutable.ListBuffer.empty[Action]

  def solve(startingState: Set[State], goals: Seq[State], tryReverse: Boolean = true): Seq[Action] =
    state.clear()
    performedActions.clear()
    state.addAll(startingState)
    val goalsAchieved: Boolean = goals.forall(achieve(Nil)) && goals.forall(state)
    if !goalsAchieved && tryReverse then solve(startingState, goals.reverse, false)  // try the reverse order of goals once
    else performedActions.toSeq

  private def achieve(goalStack: List[State])(goal: State): Boolean =
    indentPrinter(goalStack.length, s"Goal: $goal")
    if state(goal) then true
    else if goalStack.count(_ == goal) >= 30 then false
    else
      val nextGoals: Seq[Op] = ops
        .filter(_.addToState.contains(goal))
        .sortBy(-_.preconditions.count(state))  // sort by the most likely goal to achieve first i.e. least preconditions to execute
      nextGoals.nonEmpty && nextGoals.exists(applyOp(goal, goalStack))

  private def applyOp(goal: State, goalStack: List[State])(op: Op): Boolean =
    indentPrinter(goalStack.length, s"Considering: ${op.action}")
    if op.preconditions.forall(achieve(goal :: goalStack)) then
      indentPrinter(goalStack.length, s"Action: ${op.action}")
      performedActions.addOne(op.action)
      state.subtractAll(op.delFromState).addAll(op.addToState)
      true
    else false

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

  val schoolOps: Seq[Op] = Seq(
    Op(DriveSonToSchool, Seq(SonAtHome, CarWorks), Seq(SonAtSchool), Seq(SonAtHome)),
    Op(ShopInstallsBattery, Seq(CarNeedsBattery, ShopKnowsProblem, ShopHasMoney), Seq(CarWorks)),
    Op(TellShopProblem, Seq(InCommunicationWithShop), Seq(ShopKnowsProblem)),
    Op(TelephoneShop, Seq(KnowPhoneNumber), Seq(InCommunicationWithShop)),
    Op(LookUpPhoneNumber, Seq(HavePhoneBook), Seq(KnowPhoneNumber)),  // comment this to obtain infinite loop
    Op(GiveShopMoney, Seq(HaveMoney), Seq(ShopHasMoney), Seq(HaveMoney)),
    Op(AskPhoneNumber, Seq(InCommunicationWithShop), Seq(KnowPhoneNumber))
  )

// Infinite Gas Domain
enum Gas extends Action:
  case DriveToGasStation
  case TankGas

enum GasStates extends State:
  case GasTankEmpty
  case GasTankFilled
  case AtGasStation
  case NotAtGasStation

object Gas:
  val gasOps: Seq[Op] = Seq(
    Op(TankGas, Seq(AtGasStation), Seq(GasTankFilled)),
    Op(DriveToGasStation, Seq(GasTankFilled), Seq(AtGasStation))
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
  def mazeOps: Seq[Op] =
    val coords: Seq[(Int, Int)] = Seq((1, 2) ,(2, 3),(3, 4) ,(4, 9) ,(9, 14),(9, 8),(8, 7),(7, 12),(12, 13),(12, 11),(11, 6),(11, 16) ,(16, 17) ,(17, 22),(22, 21),(22, 23),(23, 18),(23, 24) ,(24, 19) ,(19, 20) ,(20, 15) ,(15, 10) ,(10, 5) ,(20, 25))
    coords.flatMap((from, to) => Seq(makeMazeOp(from, to), makeMazeOp(to, from)))

// Block Domain
case class BlockMove(which: Block, from: Block, on: Block) extends Action
case class BlockState(which: Block, on: Block) extends State
enum Block:
  case A, B, C, Space, Table

object Block:

  def makeMovesForBlocks(blocks: Block*): Seq[Op] = makeTableMoves(blocks) ++ makeBlockMoves(blocks)
  private def makeBlockMoves(blocks: Seq[Block]): Seq[Op] =
    for
      b1 <- blocks
      b2 <- blocks
      if b1 != b2
      b3 <- blocks
      if !(b3 == b1 || b3 == b2)
    yield makeMove(b1, b2, b3)

  private def makeTableMoves(blocks: Seq[Block]): Seq[Op] =
    for
      b1 <- blocks
      b2 <- blocks
      if b1 != b2
      tableMoves <- Seq(makeMove(b1, Table, b2), makeMove(b1, b2, Table))
    yield tableMoves

  private def makeMove(b1: Block, b2: Block, b3: Block): Op =
    Op(
      action = BlockMove(b1, b2, b3),
      preconditions = Seq(BlockState(Space, b1), BlockState(Space, b3), BlockState(b1, b2)),
      addToState = nextBlockStates(b1, b2, b3),
      delFromState = nextBlockStates(b1, b3, b2)
    )
  private def nextBlockStates(b1: Block, b2: Block, b3: Block): Seq[State] =
    if b2 == Table then Seq(BlockState(b1, b3))
    else Seq(BlockState(b1, b3), BlockState(Space, b2))

def printClean(solution: Seq[Action]): Unit =
  val toPrint = solution.zipWithIndex.map((act, i) => s"   Step ${i+1}: ${act.toString}")
  if toPrint.isEmpty then println("No solution possible for current operations")
  else println("\nThe following steps have to be taken in order:\n" + toPrint.mkString("\n"))



@main def test_gps: Unit =

  // SCHOOL
   val gps = new GPS(SchoolStates.schoolOps, true)
   val result = gps.solve(Set(SonAtHome, CarNeedsBattery, HaveMoney, HavePhoneBook), Seq(SonAtSchool))
   printClean(result)
//   val r2 = gps.solve(Set(SonAtHome, CarWorks), Set(SonAtSchool))
//   val r3 = gps.solve(Set(SonAtHome, CarNeedsBattery, HaveMoney, HavePhoneBook), Set(HaveMoney, SonAtSchool))
//   val r4 = gps.solve(Set(SonAtHome, CarNeedsBattery, HaveMoney, HavePhoneBook), Set(SonAtSchool, HaveMoney))
//   val r5 = gps.solve(Set(SonAtHome, CarNeedsBattery, HaveMoney), Set(SonAtSchool))

  // INFINITE GAS
//  val gasGPS = new GPS(Gas.gasOps, true)
//  val gasResult = gasGPS.solve(Set(GasTankEmpty, NotAtGasStation), Seq(GasTankFilled), false)
//  printClean(gasResult)

  // MAZE
//   val mazeGPS = new GPS(Maze.mazeOps, true)
//   val mazeResult = mazeGPS.solve(startingState = Set(Loc(1)), goals = Seq(Loc(25)))
//   printClean(mazeResult)

  // BLOCKS
//  val solverThreeBlocks = new GPS(Block.makeMovesForBlocks(A, B, C), true)
//  val solution3_4 = solverThreeBlocks.solve(
//    Set(BlockState(C, A),
//      BlockState(A, Table), 
//      BlockState(B, Table),
//      BlockState(Space, C), 
//      BlockState(Space, B), 
//      BlockState(Space, Table)), 
//    Seq(BlockState(C, Table), BlockState(A, B)))
//  printClean(solution3_4)
//  val solution3_1 = solverThreeBlocks.solve(Set(BlockState(A, B), BlockState(B, C), BlockState(C, Table), BlockState(Space, A), BlockState(Space, Table)), Seq(BlockState(B, A), BlockState(C, B)))
//  val solution3_2 = solverThreeBlocks.solve(Set(BlockState(A, B), BlockState(B, C), BlockState(C, Table), BlockState(Space, A), BlockState(Space, Table)), Seq(BlockState(C, B), BlockState(B, A)))
//  val solution3_3 = solverThreeBlocks.solve(Set(BlockState(C, A), BlockState(A, Table), BlockState(B, Table), BlockState(Space, C), BlockState(Space, B), BlockState(Space, Table)), Seq(BlockState(C, Table)))

//  val solverTwoBlocks = new GPS(Block.makeMovesForBlocks(A, B), true)
//  val solution2_1 = solverTwoBlocks.solve(Set(BlockState(A, Table), BlockState(B, Table), BlockState(Space, A), BlockState(Space, B), BlockState(Space, Table)), Seq(BlockState(A, B), BlockState(B, Table)))
//  val solution2_2 = solverTwoBlocks.solve(Set(BlockState(A, B), BlockState(B, Table), BlockState(Space, A), BlockState(Space, Table)),Seq(BlockState(B, A)))
