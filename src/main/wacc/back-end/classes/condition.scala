package wacc

sealed trait Condition {
  override def toString: String = this.getClass.getSimpleName.toLowerCase.stripSuffix("$")
}

case object EQ extends Condition 

case object NE extends Condition 

case object GT extends Condition 

case object LT extends Condition 

case object GE extends Condition 

case object LE extends Condition 