package wacc
import scala.collection.mutable

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

sealed trait SymbolEntry

case class VariableEntry(varType: String, scopeLevel: Int) extends SymbolEntry
case class FunctionEntry(returnType: String, paramTypes: List[String]) extends SymbolEntry


class SymbolTable {
  private val table: mutable.Map[String, SymbolEntry] = mutable.Map()
  private var scopeLevel: Int = 0

  
  def enterScope(): Unit = {
    scopeLevel += 1
  }

  
  def exitScope(): Unit = {
    table.filterInPlace { case (_, entry) =>
      entry match {
        case VariableEntry(_, level) => level < scopeLevel
        case _ => true
      }
    }
    scopeLevel -= 1
  }

  
  def addVariable(name: String, varType: String): Boolean = {
    if (table.contains(name)) return false 
    table(name) = VariableEntry(varType, scopeLevel)
    true
  }

  def addFunction(name: String, returnType: String, paramTypes: List[String]): Boolean = {
    if (table.contains(name)) return false
    table(name) = FunctionEntry(returnType, paramTypes)
    true
  }

  def lookup(name: String): Option[SymbolEntry] = table.get(name)
}
