// Define AST types for expressions
sealed trait Expr
case class Val(value: Any) extends Expr
case class Var(name: String) extends Expr
case class UnaryOp(op: String, expr: Expr) extends Expr
case class BinaryOp(op: String, left: Expr, right: Expr) extends Expr
case class ArrayElem(name: String, indices: List[Expr]) extends Expr
