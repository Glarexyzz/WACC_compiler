package wacc

sealed trait Expr
case class IntLiteral(value: BigInt) extends Expr
case class BoolLiteral(value: Boolean) extends Expr
case class CharLiteral(value: Char) extends Expr
case class StrLiteral(value: String) extends Expr
case class PairLiteral() extends Expr
case class Identifier(name: String) extends Expr
case class ArrayElem(name: String, indices: List[Expr]) extends Expr
case class UnaryOp(op: String, expr: Expr) extends Expr
case class BinaryOp(op: String, left: Expr, right: Expr) extends Expr
case class ParenExpr(expr: Expr) extends Expr
