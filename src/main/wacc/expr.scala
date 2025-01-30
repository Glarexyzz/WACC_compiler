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

case class ArrayElem(name: String, indices: List[Expr]) extends Expr
object ArrayElem extends generic.ParserBridge2[String, List[Expr], ArrayElem]

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

// Types 
sealed trait Type
sealed trait BaseType extends Type

object BaseType {
    case object IntType extends BaseType
    case object BoolType extends BaseType
    case object CharType extends BaseType
    case object StringType extends BaseType
}

case object ErasedPairType extends Type

case class PairType(first: Type, second: Type) extends Type
object PairType extends generic.ParserBridge2[Type, Type, PairType]

case class ArrayType(inner: Type) extends Type
object ArrayType extends generic.ParserBridge1[Type, ArrayType]


// Weakest Precedence

// Statements

case class Program(funcs: List[Function], body: Statement)

case class Function(
    returnType: Type,
    name: String,
    params: List[Param],
    body: Statement
)

case class Param(paramType: Type, name: String)

sealed trait Statement

case object Skip extends Statement
case class VarDecl(varType: Type, name: String, value: RValue) extends Statement
case class Assign(target: LValue, value: RValue) extends Statement
case class Read(toRead: LValue) extends Statement
case class Free(expr: Expr) extends Statement
case class Return(expr: Expr) extends Statement
case class Exit(expr: Expr) extends Statement
case class Print(expr: Expr, newline: Boolean) extends Statement
case class If(cond: Expr, thenBranch: Statement, elseBranch: Statement) extends Statement
case class While(cond: Expr, body: Statement) extends Statement
case class Block(body: Statement) extends Statement
case class Seq(left: Statement, right: Statement) extends Statement

sealed trait LValue
case class LIdent(name: String) extends LValue
case class LArrayElem(name: String, indices: List[Expr]) extends LValue
case class LPairElem(elem: PairElem) extends LValue // check this

sealed trait RValue
case class RExpr(expr: Expr) extends RValue
case class RArrayLit(elements: List[Expr]) extends RValue
case class RNewPair(first: Expr, second: Expr) extends RValue
case class RPairElem(elem: PairElem) extends RValue // check this
case class RCall(name: String, args: List[Expr]) extends RValue

sealed trait PairElem
case class First(value: LValue) extends PairElem
case class Second(value: LValue) extends PairElem
