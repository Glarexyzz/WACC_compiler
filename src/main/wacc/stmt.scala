package wacc

import parsley.generic
import parsley.Parsley

case class Program(funcs: List[Func], stmt: Stmt)

case class Func(t: Type, name: String, paramList: List[Param], stmt: Stmt)

//  Param & Param list
//  Param list no need AST because it can be in List[Param]
case class Param(t: Type, nameL: String)

// Statements
sealed trait Stmt

case object SkipStmt extends Stmt
case class DeclAssignStmt(t: Type, name: String, value: RValue) extends Stmt
case class AssignStmt(lvalue: LValue, rvalue: RValue) extends Stmt
case class ReadStmt(value: LValue) extends Stmt
case class FreeStmt(expr: Expr) extends Stmt
case class ReturnStmt(expr: Expr) extends Stmt
case class ExitStmt(expr: Expr) extends Stmt
case class PrintStmt(expr: Expr) extends Stmt
case class PrintlnStmt(expr: Expr) extends Stmt
case class IfStmt(cond: Expr, thenStmt: Stmt, elseStmt: Stmt) extends Stmt
case class WhileStmt(cond: Expr, body: Stmt) extends Stmt
case class BodyStmt(body: Stmt) extends Stmt
case class SeqStmt(left: Stmt, right: Stmt) extends Stmt

//  Values
enum LValue {
  case LName(name: String)
  case LArray(array: ArrayElem)
  case LPair(pair: PairElem)
}

enum RValue {
  case RExpr(expr: Expr)
  case RArrayLiter(arrayLiter: ArrayLiter)
  case RNewPair(lExpr: Expr, rExpr: Expr)
  case RPair(pair: PairElem)
  case RCall(name: String, argList: List[Expr])
}

//  Arg list no need AST, because it can be in List[Expr]

enum PairElem {
  case FstElem(value: LValue)
  case SndElem(value: LValue)
}

case class ArrayLiter(elements: List[Expr])