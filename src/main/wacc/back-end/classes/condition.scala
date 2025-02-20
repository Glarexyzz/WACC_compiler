package wacc

sealed trait Condition

case object EQ extends Condition {
    override def toString: String = "eq"
}
case object NE extends Condition {
    override def toString: String = "ne"
}
case object GT extends Condition {
    override def toString: String = "gt"
}
case object LT extends Condition {
    override def toString: String = "lt"
}
case object GE extends Condition {
    override def toString: String = "ge"
}
case object LE extends Condition {
    override def toString: String = "le"
}