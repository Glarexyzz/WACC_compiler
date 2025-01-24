package wacc

import parsley.generic

sealed trait Expr

case class IntLiteral(value: BigInt) extends Expr
case class BoolLiteral(value: Boolean) extends Expr
case class CharLiteral(value: Char) extends Expr
case class StrLiteral(value: String) extends Expr
case object PairLiteral extends Expr  // Potential problematic? need to recheck
case class Identifier(name: String) extends Expr
case class ArrayElem(name: String, indices: List[Expr]) extends Expr
case class UnaryOp(op: UnaryOperator, expr: Expr) extends Expr
case class BinaryOp(left: Expr, op: BinaryOperator, right: Expr) extends Expr
//case class ParenExpr(expr: Expr) extends Expr

// Strongest Precedence

// Unary Operators
enum UnaryOperator {
    case Not
    case Negate
    case Length
    case Ord
    case Chr
}

// Binary Operators
enum BinaryOperator {
    case Multiply, Divide, Modulus
    case Add, Subtract
    case Greater, GreaterEqual, Less, LessEqual
    case Equal, NotEqual
    case And
    case Or
}

// Weakest Precedence
