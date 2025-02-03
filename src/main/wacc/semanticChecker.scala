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
    // char[] can be treated as string
    case (ArrayType(BaseType.CharType), BaseType.StrType) => true 
    // string cannot be treated as char[]
    case (BaseType.StrType, ArrayType(BaseType.CharType)) => false // string to char[] is not allowed
    
    // arrays with compatible inner types
    case (ArrayType(innerType1), ArrayType(innerType2)) =>
      areTypesCompatible(innerType1, innerType2) // recursively check if inner types are compatible

    case (BaseType.IntType, BaseType.IntType) => true
    case (BaseType.BoolType, BaseType.BoolType) => true
    case (BaseType.CharType, BaseType.CharType) => true
    case (BaseType.StrType, BaseType.StrType) => true

    case (PairType(leftElem1 : Type, rightElem1 : Type), PairType(leftElem2 : Type, rightElem2 : Type)) =>
      areTypesCompatible(leftElem1, leftElem2) && areTypesCompatible(rightElem1, rightElem2)

    case _ => false
  }


  


  def checkArrayElementType(elementType: Type): Boolean = elementType match {
    case PairKeyword => false // Arrays cannot hold pairs
    case _ => true
}


}
