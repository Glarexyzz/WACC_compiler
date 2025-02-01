package wacc
import scala.collection.mutable

case class SemanticError(message: String) extends Exception(message)


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

class SemanticChecker {
  
  // Top-level check for the program
  def checkProgram(program: Program): Unit = {
    // Check function declarations
    program.funcs.foreach(checkFunctionDeclaration)
    
    // Check the top-level statement (stmt)
    checkStatement(program.stmt)
  }

  // Check function declarations
  def checkFunctionDeclaration(func: Func): Unit = {
    println(s"Checking function: ${func.name}")
    // Example: Check function body (statements) for semantic errors
    checkStatement(func.stmt)
  }

  def checkStatement(stmt: Stmt): Unit = stmt match {
    case _ => 
  }

  def areTypesCompatible(t1: Type, t2: Type): Boolean = (t1, t2) match {
    case (ArrayType(BaseType.CharType), BaseType.StrType) => true // Weakening rule for char[] and string
    case (BaseType.StrType, ArrayType(BaseType.CharType)) => false // String cannot be treated as char[]
    case _ if t1 == t2 => true // Exact match
    case _ => false  // return semantic error for false cases??
  }

  def checkArrayElementType(elementType: Type): Boolean = elementType match {
    case PairKeyword => false // Arrays cannot hold pairs
    case _ => true
}


}
