package ai.paip

import ai.paip.SchoolDomain.*

// https://github.com/norvig/paip-lisp/tree/main

case class Op(action: Domain, precons: List[Domain], addList: List[Domain], delList: List[Domain] = Nil)

class GPS(startingState: List[Domain], ops: Vector[Op]):

  val state: scala.collection.mutable.Set[Domain] = scala.collection.mutable.Set.empty

  def run(goals: Vector[Domain]): String =
    state.addAll(startingState)
    if goals.forall(achieve) then "solved" else "failed"

  def achieve(goal: Domain): Boolean = state.contains(goal) || ops.filter(appropriateOp(goal)).forall(applyOp)

  def appropriateOp(goal: Domain)(op: Op): Boolean = op.addList.contains(goal)

  def applyOp(op: Op): Boolean =
    if op.precons.forall(achieve) then
      println(s"Executing ${op.action}")
      op.delList.foreach(d => state.remove(d))
      state.addAll(op.addList)
      true
    else false


trait Domain

enum SchoolDomain extends Domain:
  // dad states:
  case HavePhoneBook
  case HaveMoney
  case KnowPhoneNumber

  // actions:
  case DriveSonToSchool
  case GiveShopMoney
  case TelephoneShop
  case LookUpPhoneNumber  
  case TellShopProblem

  // son states:
  case SonAtSchool
  case SonAtHome
  
  // car states:
  case CarWorks
  case CarNeedsBattery
  
  // carshop states:
  case ShopKnowsProblem
  case ShopInstallsBattery
  case InCommunicationWithShop
  case ShopHasMoney
  

object School:

  val schoolOps: Vector[Op] = Vector(
    Op(DriveSonToSchool, List(SonAtHome, CarWorks), List(SonAtSchool), List(SonAtHome)),
    Op(ShopInstallsBattery, List(CarNeedsBattery, ShopKnowsProblem, ShopHasMoney), List(CarWorks)),
    Op(TellShopProblem, List(InCommunicationWithShop), List(ShopKnowsProblem)),
    Op(TelephoneShop, List(KnowPhoneNumber), List(InCommunicationWithShop)),
    Op(LookUpPhoneNumber, List(HavePhoneBook), List(KnowPhoneNumber)),
    Op(GiveShopMoney, List(HaveMoney), List(ShopHasMoney), List(HaveMoney))
  )



@main def atest_gps: Unit =

  val gps = GPS(List(SonAtHome, CarNeedsBattery, HaveMoney, HavePhoneBook), School.schoolOps)

  val x = gps.run(Vector(SonAtSchool))
  println(x)