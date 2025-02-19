package wacc

import parsley.generic

// 'begin' <func>* <stmt> 'end'
case class Program(funcs: List[Func], stmt: Stmt)
object Program extends generic.ParserBridge2[List[Func], Stmt, Program]

// <type> <ident> '(' <paramList>? ')''is' <stmt> 'end'
case class Func(
  t: Type, 
  name: String, 
  paramList: Option[List[Param]], 
  stmt: Stmt
)
object Func extends generic.ParserBridge4[
  Type, String, Option[List[Param]], Stmt, Func
]

// Param & Param list
// Param list no need AST because it can be in List[Param]
// <type> <ident>
case class Param(t: Type, name: String)
object Param extends generic.ParserBridge2[Type, String, Param]

// Statements
sealed trait Stmt

// 'skip'
case object SkipStmt extends Stmt

// <type> <ident> '=' <rValue>
case class DeclAssignStmt(t: Type, name: String, value: RValue) extends Stmt
object DeclAssignStmt extends generic.ParserBridge3[Type, String, RValue, Stmt]

// <lValue> '=' <rValue>
case class AssignStmt(lvalue: LValue, rvalue: RValue) extends Stmt
object AssignStmt extends generic.ParserBridge2[LValue, RValue, Stmt]

// 'read' <lValue>
case class ReadStmt(value: LValue) extends Stmt
object ReadStmt extends generic.ParserBridge1[LValue, Stmt]

// 'free' <expr>
case class FreeStmt(expr: Expr) extends Stmt
object FreeStmt extends generic.ParserBridge1[Expr, Stmt]

// 'return' <expr>
case class ReturnStmt(expr: Expr) extends Stmt
object ReturnStmt extends generic.ParserBridge1[Expr, Stmt]

// 'exit' <expr>
case class ExitStmt(expr: Expr) extends Stmt
object ExitStmt extends generic.ParserBridge1[Expr, Stmt]

// 'print' <expr>
case class PrintStmt(expr: Expr) extends Stmt
object PrintStmt extends generic.ParserBridge1[Expr, Stmt]

// 'println' <expr>
case class PrintlnStmt(expr: Expr) extends Stmt
object PrintlnStmt extends generic.ParserBridge1[Expr, Stmt]

// 'if' <expr> 'then' <stmt> 'else' <stmt> 'fi'
case class IfStmt(cond: Expr, thenStmt: Stmt, elseStmt: Stmt) extends Stmt
object IfStmt extends generic.ParserBridge3[Expr, Stmt, Stmt, Stmt] 

// 'while' <expr> 'do' <stmt> 'done'
case class WhileStmt(cond: Expr, body: Stmt) extends Stmt
object WhileStmt extends generic.ParserBridge2[Expr, Stmt, Stmt]

// 'begin' <stmt> 'end'
case class BodyStmt(body: Stmt) extends Stmt
object BodyStmt extends generic.ParserBridge1[Stmt, Stmt]

// <stmt> ';' <stmt>
case class SeqStmt(left: Stmt, right: Stmt) extends Stmt
object SeqStmt extends generic.ParserBridge2[Stmt, Stmt, Stmt]

// Values
// <ident> | <arrayElem> | <pairElem>
enum LValue extends Expr{
  case LName(name: String)
  case LArray(array: ArrayElem)
  case LPair(pair: PairElem)
}

object LValue {
  object LName extends generic.ParserBridge1[String, LValue]
  object LArray extends generic.ParserBridge1[ArrayElem, LValue]
  object LPair extends generic.ParserBridge1[PairElem, LValue]
}

// <expr> | <arrayLiter> | 'newpair' '(' <expr> ',' <expr> ')' | <pairElem> | 'call' <ident> '(' <argList>? ')'
enum RValue extends Expr{
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

// Arg list no need AST, because it can be in List[Expr]
// 'fst' <lvalue> | 'snd' <lvalue>
enum PairElem {
  case FstElem(value: LValue)
  case SndElem(value: LValue)
}

object PairElem {
  object FstElem extends generic.ParserBridge1[LValue, PairElem]
  object SndElem extends generic.ParserBridge1[LValue, PairElem]
}

// '[' (<expr> (',' <expr>)* )?']'
case class ArrayLiter(elements: Option[List[Expr]])
object ArrayLiter extends generic.ParserBridge1[Option[List[Expr]], ArrayLiter]