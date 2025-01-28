package wacc

import parsley.generic
import parsley.Parsley

case class Program(funcs: List[Func], stmt: Stmt)
object Program extends generic.ParserBridge2[List[Func], Stmt, Program]

case class Func(
  t: Type, 
  name: String, 
  paramList: Option[List[Param]], 
  stmt: Stmt
)
object Func extends generic.ParserBridge4[
  Type, String, Option[List[Param]], Stmt, Func
]

//  Param & Param list
//  Param list no need AST because it can be in List[Param]
case class Param(t: Type, name: String)
object Param extends generic.ParserBridge2[Type, String, Param]

// Statements
sealed trait Stmt

case object SkipStmt extends Stmt

case class DeclAssignStmt(t: Type, name: String, value: RValue) extends Stmt
object DeclAssignStmt extends generic.ParserBridge3[Type, String, RValue, Stmt]

case class AssignStmt(lvalue: LValue, rvalue: RValue) extends Stmt
object AssignStmt extends generic.ParserBridge2[LValue, RValue, Stmt]

case class ReadStmt(value: LValue) extends Stmt
object ReadStmt extends generic.ParserBridge1[LValue, Stmt]

case class FreeStmt(expr: Expr) extends Stmt
object FreeStmt extends generic.ParserBridge1[Expr, Stmt]

case class ReturnStmt(expr: Expr) extends Stmt
object ReturnStmt extends generic.ParserBridge1[Expr, Stmt]

case class ExitStmt(expr: Expr) extends Stmt
object ExitStmt extends generic.ParserBridge1[Expr, Stmt]

case class PrintStmt(expr: Expr) extends Stmt
object PrintStmt extends generic.ParserBridge1[Expr, Stmt]

case class PrintlnStmt(expr: Expr) extends Stmt
object PrintlnStmt extends generic.ParserBridge1[Expr, Stmt]

case class IfStmt(cond: Expr, thenStmt: Stmt, elseStmt: Stmt) extends Stmt
object IfStmt extends generic.ParserBridge3[Expr, Stmt, Stmt, Stmt]

case class WhileStmt(cond: Expr, body: Stmt) extends Stmt
object WhileStmt extends generic.ParserBridge2[Expr, Stmt, Stmt]

case class BodyStmt(body: Stmt) extends Stmt
object BodyStmt extends generic.ParserBridge1[Stmt, Stmt]

case class SeqStmt(left: Stmt, right: Stmt) extends Stmt
object SeqStmt extends generic.ParserBridge2[Stmt, Stmt, Stmt]

//  Values
enum LValue {
  case LName(name: String)
  case LArray(array: ArrayElem)
  case LPair(pair: PairElem)
}

object LValue {
  object LName extends generic.ParserBridge1[String, LValue]
  object LArray extends generic.ParserBridge1[ArrayElem, LValue]
  object LPair extends generic.ParserBridge1[PairElem, LValue]
}

enum RValue {
  case RExpr(expr: Expr)
  case RArrayLiter(arrayLiter: ArrayLiter)
  case RNewPair(lExpr: Expr, rExpr: Expr)
  case RPair(pair: PairElem)
  case RCall(name: String, argList: Option[List[Expr]])
}

object RValue {
  object RExpr extends generic.ParserBridge1[Expr, RValue]
  object RArrayLiter extends generic.ParserBridge1[ArrayLiter, RValue]
  object RNewPair extends generic.ParserBridge2[Expr, Expr, RValue]
  object RPair extends generic.ParserBridge1[PairElem, RValue]
  object RCall extends generic.ParserBridge2[String, Option[List[Expr]], RValue]
}

//  Arg list no need AST, because it can be in List[Expr]

enum PairElem {
  case FstElem(value: LValue)
  case SndElem(value: LValue)
}

object PairElem {
  object FstElem extends generic.ParserBridge1[LValue, PairElem]
  object SndElem extends generic.ParserBridge1[LValue, PairElem]
}

case class ArrayLiter(elements: Option[List[Expr]])
object ArrayLiter extends generic.ParserBridge1[Option[List[Expr]], ArrayLiter]