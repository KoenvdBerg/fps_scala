package ai.paip

// https://github.com/norvig/paip-lisp/tree/main

case class Op(action: String, precons: List[String], addList: List[String], delList: List[String] = Nil)

class GPS(startingState: List[String], ops: Vector[Op]):

  val state: scala.collection.mutable.Set[String] = scala.collection.mutable.Set.empty

  def run(goals: Vector[String]): String =
    state.addAll(startingState)
    if goals.forall(achieve) then "solved" else "failed"

  def achieve(goal: String): Boolean = state.contains(goal) || ops.filter(appropriateOp(goal)).forall(applyOp)

  def appropriateOp(goal: String)(op: Op): Boolean = op.addList.contains(goal)

  def applyOp(op: Op): Boolean =
    if op.precons.forall(achieve) then
      println(s"Executing ${op.action}")
      op.delList.foreach(d => state.remove(d))
      state.addAll(op.addList)
      true
    else false


object School:

  val schoolOps: Vector[Op] = Vector(
    Op("drive son to school", List("son at work", "car works"), List("son at school"), List("son at home")),
    Op("shop installs battery", List("car needs battery", "shop knows problem", "shop has money"), List("car works")),
    Op("tell shop problem", List("in communication with shop"), List("shop knows problem")),
    Op("telephone shop", List("know phone number"), List("in communication with shop")),
    Op("look up phone number", List("have phone book"), List("know phone number")),
    Op("give shop money", List("have money"), List("shop has money"), List("have money"))
  )



@main def atest_gps: Unit =

  val gps = GPS(List("son at home", "car needs battery", "have money", "have phone book"), School.schoolOps)

  val x = gps.run(Vector("son at school"))
  println(x)