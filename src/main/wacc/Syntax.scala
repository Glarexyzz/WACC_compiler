sealed trait Expr extends RValue
sealed trait LValue
sealed trait RValue 
case class Identifier extends Expr, LValue, RValue