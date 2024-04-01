package ai.paip

import GPSConcrete.SchoolDomain.*
import scala.collection.mutable

object GPSAbstract:

  trait State
  type GlobalState = Set[State]

  trait Operation

  type OldState = GlobalState
  type NewState = GlobalState
  type OnAction = (OldState, Operation) => NewState

object GPSConcrete:

  trait Action
  trait State

  case class Op(action: Action, preconditions: Seq[State], addToState: Seq[State], delFromState: Seq[State] = Seq.empty[State]):

    def onAction(globalState: Set[State]): Set[State] = if preconditions.forall(globalState) then (globalState ++ addToState).removedAll(delFromState) else globalState

  class GPS(ops: Seq[Op], startState: Set[State]):

    private val globalState: mutable.Set[State] = mutable.Set.from(startState)

    def canAchieveGoal(goal: State): Boolean =
      if globalState(goal) then true
      else ops
        .filter(_.addToState.contains(goal))  // retrieving actions to solve subgoals
        .exists(op =>
          if op.preconditions.forall(canAchieveGoal) then
            globalState.addAll(op.addToState).subtractAll(op.delFromState)
            true
          else false)

  case object DriveSonToSchool extends Action
  enum SchoolDomain extends State:
    case SonAtHome, CarWorks, SonAtSchool

  val exampleOperation: Op = Op(
    action = DriveSonToSchool,
    preconditions = Seq(SonAtHome, CarWorks),
    addToState = Seq(SonAtSchool),
    delFromState = Seq(SonAtHome))
