package wacc

import parsley.generic

sealed trait Expr

case class IntLiteral(value: BigInt) extends Expr
object IntLiteral extends generic.ParserBridge1[BigInt, Expr]

case class BoolLiteral(value: Boolean) extends Expr
object BoolLiteral extends generic.ParserBridge1[Boolean, Expr]

case class CharLiteral(value: Char) extends Expr
object CharLiteral extends generic.ParserBridge1[Char, Expr]

case class StrLiteral(value: String) extends Expr
object StrLiteral extends generic.ParserBridge1[String, Expr]

case object PairLiteral extends Expr

case class Identifier(name: String) extends Expr
object Identifier extends generic.ParserBridge1[String, Expr]

// <ident> ('[ <expr> ']')+
case class ArrayElem(name: String, indices: List[Expr]) extends Expr
object ArrayElem extends generic.ParserBridge2[String, List[Expr], ArrayElem]

case class UnaryOp(op: UnaryOperator, expr: Expr) extends Expr
case class BinaryOp(left: Expr, op: BinaryOperator, right: Expr) extends Expr
//case class ParenExpr(expr: Expr) extends Expr

// Unary Operators by precedence
enum UnaryOperator {
    case Not
    case Negate
    case Length
    case Ord
    case Chr
}

// Binary Operators by precedence
enum BinaryOperator {
    case Multiply, Divide, Modulus
    case Add, Subtract
    case Greater, GreaterEqual, Less, LessEqual
    case Equal, NotEqual
    case And
    case Or
}