package wacc

case class SemanticError(message: String) extends Exception(message)

class SemanticErrorBuilder {
  def typeMismatch(expected: String, actual: String): SemanticError =
    SemanticError(s"Type mismatch: expected $expected, got $actual")

  def undeclaredVariable(name: String): SemanticError =
    SemanticError(s"Undeclared variable: $name")

  def duplicateDeclaration(name: String): SemanticError =
    SemanticError(s"Duplicate declaration: $name")

  def invalidFree(target: String): SemanticError =
    SemanticError(s"Cannot free non-heap object of type $target")

  def invalidOperator(op: String, t: String): SemanticError =
    SemanticError(s"Invalid operator '$op' for type $t")

  def invalidReturnType(expected: String, actual: String): SemanticError =
    SemanticError(s"Invalid return type: expected $expected, got $actual")

}
