package ai.paip

import ai.paip.SchoolStates.*
import ai.paip.SchoolActions.*
import ai.paip.Block.*

import scala.collection.mutable

case class Op(action: Action, preconditions: Seq[State], addToState: Seq[State], delFromState: Seq[State] = Seq.empty[State])

class GPS(ops: Seq[Op], debug: Boolean = false):

  private val state: scala.collection.mutable.Set[State]   = scala.collection.mutable.Set.empty
  private val performedActions: mutable.ListBuffer[Action] = mutable.ListBuffer.empty[Action]

  def run(startingState: Set[State], goals: Seq[State], tryReverse: Boolean = true): Seq[Action] =
    state.clear()
    performedActions.clear()
    state.addAll(startingState)
    val goalsAchieved: Boolean = goals.forall(achieve(Nil)) && goals.forall(state)
    if !goalsAchieved && tryReverse then run(startingState, goals.reverse, false)  // try the reverse order of goals once
    else performedActions.toSeq

  private def achieve(goalStack: List[State])(goal: State): Boolean =
    indentPrinter(goalStack.length, s"Goal: $goal")
    if state(goal) then true
    else if goalStack.contains(goal) then false
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

  private def indentPrinter(l: Int, msg: String): Unit = if debug then pprint.pprintln(s"${" " * l}$msg")


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





@main def test_gps: Unit =

  // SCHOOL
  // val gps = new GPS(SchoolStates.schoolOps, true)
  // val result = gps.run(Set(SonAtHome, CarNeedsBattery, HaveMoney, HavePhoneBook), Set(SonAtSchool))
  // val r2 = gps.run(Set(SonAtHome, CarWorks), Set(SonAtSchool))
  // val r3 = gps.run(Set(SonAtHome, CarNeedsBattery, HaveMoney, HavePhoneBook), Set(HaveMoney, SonAtSchool))
  // val r4 = gps.run(Set(SonAtHome, CarNeedsBattery, HaveMoney, HavePhoneBook), Set(SonAtSchool, HaveMoney))
  // val r5 = gps.run(Set(SonAtHome, CarNeedsBattery, HaveMoney), Set(SonAtSchool))

  // MAZE
  // val mazeGPS = new GPS(Maze.mazeOps, true)
  // println(mazeGPS.run(startingState = Set(Loc(1)), goals = Set(Loc(25))))

  // BLOCKS
  // val solverTwoBlocks = new GPS(Block.makeMovesForBlocks(A, B), true)
  // val solution2_1 = solverTwoBlocks.run(Set(BlockState(A, Table), BlockState(B, Table), BlockState(Space, A), BlockState(Space, B), BlockState(Space, Table)), Set(BlockState(A, B), BlockState(B, Table)))
  // val solution2_2 = solverTwoBlocks.run(Set(BlockState(A, B), BlockState(B, Table), BlockState(Space, A), BlockState(Space, Table)),Set(BlockState(B, A)))

  val solverThreeBlocks = new GPS(Block.makeMovesForBlocks(A, B, C), true)
  val solution3_1 = solverThreeBlocks.run(Set(BlockState(A, B), BlockState(B, C), BlockState(C, Table), BlockState(Space, A), BlockState(Space, Table)), Seq(BlockState(B, A), BlockState(C, B)))
  val solution3_2 = solverThreeBlocks.run(Set(BlockState(A, B), BlockState(B, C), BlockState(C, Table), BlockState(Space, A), BlockState(Space, Table)), Seq(BlockState(C, B), BlockState(B, A)))
  val solution3_3 = solverThreeBlocks.run(Set(BlockState(C, A), BlockState(A, Table), BlockState(B, Table), BlockState(Space, C), BlockState(Space, B), BlockState(Space, Table)), Seq(BlockState(C, Table)))
  val solution3_4 = solverThreeBlocks.run(Set(BlockState(C, A), BlockState(A, Table), BlockState(B, Table), BlockState(Space, C), BlockState(Space, B), BlockState(Space, Table)), Seq(BlockState(C, Table), BlockState(A, B)))
