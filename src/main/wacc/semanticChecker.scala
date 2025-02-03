package wacc
import scala.collection.mutable

case class SemanticError(message: String) extends Exception(message)


sealed trait SymbolEntry

case class VariableEntry(varType: Type, scopeLevel: Int) extends SymbolEntry
case class FunctionEntry(returnType: Type, paramTypes: List[String]) extends SymbolEntry


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
  
  def addVariable(name: String, varType: Type): Boolean = {
    if (table.contains(name)) return false 
    table(name) = VariableEntry(varType, scopeLevel)
    true
  }

  def addFunction(name: String, returnType: Type, paramTypes: List[String]): Boolean = {
    if (table.contains(name)) return false
    table(name) = FunctionEntry(returnType, paramTypes)
    true
  }

  def lookup(name: String): Option[SymbolEntry] = table.get(name)
}

object semanticChecker {

  val symbolTable: SymbolTable = new SymbolTable

  def checkSemantic(parsed: Any): Option[String] = parsed match {
    case program: Program => checkProgram(program)
    case stmt: Stmt => checkStatement(stmt)
    case expr: Expr => checkExprType(expr, symbolTable) match {
      case Left(error) => Some(error)
      case Right(_) => None
    }
    case func: Func => checkFunc(func)
    case _ => Some(s"Unknown parsed structure")
  }
  
  // we want to use Err for better error messages later
  def checkProgram(program: Program): Option[String] = {
    val funcErrors = program.funcs.flatMap(checkFunc)
    // val noFuncErrors = program.funcs.foreach(checkFunctionDeclaration)
    val stmtErrors = checkStatement(program.stmt).toList
    val errors = funcErrors /*++ noFuncErrors*/ ++ stmtErrors
    if (errors.isEmpty) None else Some(errors.mkString("\n"))
  }

  def checkFunc(func: Func) : Option[String] = None // TODO: undefined)
  // Check function declarations
  def checkFunctionDeclaration(func: Func): Unit = {
    println(s"Checking function: ${func.name}")
    // Example: Check function body (statements) for semantic errors
    checkStatement(func.stmt)
  }

  def checkStatement(stmt: Stmt): Option[String] = stmt match {
    case SkipStmt => None
    // <type> <ident> '=' <rValue>
    case DeclAssignStmt(t, name, value) => checkExprType(value, symbolTable) match {
      case Left(error) => Some(error)
      case Right(valueType) =>
        if (symbolTable.addVariable(name, t)) None
        else Some(s"Error: Variable $name already declared")
    }
    // <lValue> '=' <rValue> Check if lvalue is declared and types match
    // lvalue must be an identifier or array-element or pair-element
    // rvalue must be a <expr> | <arrayLiter> | 'newpair' '(' <expr> ',' <expr> ')' | <pairElem> | 'call' <ident> '(' <argList>? ')'
    case AssignStmt(lvalue, rvalue) => None // Fixing

    // 'read' <lValue>
    case ReadStmt(value) => checkExprType(value, symbolTable) match {
      case Left(error) => Some(error)
      case Right(_) => None
    }

    // 'free' <expr>
    case FreeStmt(expr) => checkExprType(expr, symbolTable) match {
      case Left(error) => Some(error)
      case Right(ArrayType(_)) | Right(PairType(_, _)) => None // The ‘free’ statement takes an argument that must be of type 𝜏[] or pair(𝜏, 𝜎).
      case Right(_) => Some("Semantic Error: `free` statement must be applied to an array or pair")
      case _ => None // Could not match statement type
    }

    // 'return' <expr>
    case ReturnStmt(expr) => checkExprType(expr, symbolTable) match {
      case Left(error) => Some(error)
      case Right(_) => None
    }

    // 'exit' <IntType>
    case ExitStmt(expr) => 
      checkExprType(expr, symbolTable) match {
        case Left(error) => Some(error) // Propagate any errors from checking the expression
        case Right(BaseType.IntType) => None // Valid type for exit (int)
        case Right(_) => Some("Semantic Error: Exit statement must have an integer argument") // Invalid type
    }

    // 'print' <expr>
    case PrintStmt(expr) => checkExprType(expr, symbolTable) match {
      case Left(error) => Some(error)
      case Right(_) => None
    }

    // 'println' <expr>
    case PrintlnStmt(expr) => checkExprType(expr, symbolTable) match {
      case Left(error) => Some(error)
      case Right(_) => None
    }

    // 'if' <expr> 'then' <stmt> 'else' <stmt> 'fi'
    case IfStmt(cond, thenStmt, elseStmt) => cond match {
      // hard code to check if it works
      case BinaryOp(_, BinaryOperator.Add, _) => Some(s"Error: If condition must be a boolean, found IntType in $cond")
        // checkExpr(cond) match {
        //   case Some(BoolType) => 
        //     checkStatement(thenStmt) ++ checkStatement(elseStmt)
        //   case Some(IntType) =>
        //     Some(s"Error: If condition must be a boolean, found IntType in $cond")
        //   case Some(other) =>
        //     Some(s"Error: If condition must be a boolean, found $other in $cond")
        //   case None => Some(s"Error: Could not determine type of condition $cond")
      case _ => None
    }

    // 'while' <expr> 'do' <stmt> 'done'
    case WhileStmt(cond, body) => cond match {
      case BinaryOp(_, BinaryOperator.Add, _) => Some(s"Error: While condition must be a boolean, found IntType in $cond")
      case _ => None
    }
    // 'begin' <stmt> 'end'
    case BodyStmt(body) => checkStatement(body)

    // <stmt> ';' <stmt>
    case SeqStmt(left, right) =>
      (checkStatement(left), checkStatement(right)) match {
        case (Some(errorL), Some(errorR)) => Some(s"$errorL, $errorR")  // Combine both errors
        case (Some(errorL), None) => Some(errorL)  // Only left statement has an error
        case (None, Some(errorR)) => Some(errorR)  // Only right statement has an error
        case (None, None) => None  // No errors in either statement
      }
  }

  def checkExprType(expr: Expr, env: SymbolTable): Either[String, Type] = expr match {
    
    case IntLiteral(_) => Right(BaseType.IntType)
    case BoolLiteral(_) => Right(BaseType.BoolType)
    case CharLiteral(_) => Right(BaseType.CharType)
    case StrLiteral(_) => Right(BaseType.StrType)
    case PairLiteral => Left(s"Pair not defined")
    case Identifier(name) => Left(s"Ident not defined")
      //env.lookup(name).toRight(s"Semantic Error: Undefined variable '$name'")
    case ArrayElem(name, indices) => ???
    // Arithmetic Binary Operations: +, -, *, /, %
    case BinaryOp(left, op, right) if Set(
      BinaryOperator.Add,
      BinaryOperator.Subtract,
      BinaryOperator.Multiply,
      BinaryOperator.Divide,
      BinaryOperator.Modulus
    ).contains(op) =>
      (checkExprType(left, env), checkExprType(right, env)) match {
        case (Right(BaseType.IntType), Right(BaseType.IntType)) => Right(BaseType.IntType) 
        case _ => Left("Semantic Error: Incompatible types for arithmetic operation") 
      }
    
    case BinaryOp(left, op, right) if Set(
      BinaryOperator.Greater,
      BinaryOperator.GreaterEqual,
      BinaryOperator.Less,
      BinaryOperator.LessEqual,
    ).contains(op) =>
      (checkExprType(left, env), checkExprType(right, env)) match {
        case (Right(BaseType.IntType), Right(BaseType.IntType)) => Right(BaseType.BoolType) 
        case (Right(BaseType.CharType), Right(BaseType.CharType)) => Right(BaseType.BoolType) 
        case _ => Left("Semantic Error: Incompatible types for comparison operation") 
      }
    
    case BinaryOp(left, op, right) if Set(
      BinaryOperator.Equal,
      BinaryOperator.NotEqual
    ).contains(op) =>
      (checkExprType(left, env), checkExprType(right, env)) match {
        case (Right(t1), Right(t2)) if areTypesCompatible(t1, t2) => Right(BaseType.BoolType) 
        case _ => Left("Semantic Error: Incompatible types for equality comparison") 
      }
    case BinaryOp(left, op, right) if Set(
      BinaryOperator.And,
      BinaryOperator.Or
    ).contains(op) =>
      (checkExprType(left, env), checkExprType(right, env)) match {
        case (Right(BaseType.BoolType), Right(BaseType.BoolType)) => Right(BaseType.BoolType) 
        case _ => Left("Semantic Error: Logical operators require boolean operands") 
    }

    // Unary Operators: !, -, len, ord, chr
    case UnaryOp(UnaryOperator.Not, expr) =>
      checkExprType(expr, env) match {
        case Right(BaseType.BoolType) => Right(BaseType.BoolType) 
        case _ => Left("Semantic Error: `!` operator requires a boolean operand") 
      }

    case UnaryOp(UnaryOperator.Negate, expr) =>
      checkExprType(expr, env) match {
        case Right(BaseType.IntType) => Right(BaseType.IntType) 
        case _ => Left("Semantic Error: `-` operator requires an integer operand") 
      }

    case UnaryOp(UnaryOperator.Length, expr) =>
      checkExprType(expr, env) match {
        case Right(ArrayType(_)) => Right(BaseType.IntType) 
        case _ => Left("Semantic Error: `len` operator requires an array")
      }

    case UnaryOp(UnaryOperator.Ord, expr) =>
      checkExprType(expr, env) match {
        case Right(BaseType.CharType) => Right(BaseType.IntType) 
        case _ => Left("Semantic Error: `ord` operator requires a character") 
      }

    case UnaryOp(UnaryOperator.Chr, expr) =>
      checkExprType(expr, env) match {
        case Right(BaseType.IntType) => Right(BaseType.CharType) 
        case _ => Left("Semantic Error: `chr` operator requires an integer") 
      }

    case _ => Left(s"Undefined Expression")
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


  


  // def checkArrayElementType(elementType: Type): Boolean = elementType match {
  //   case PairKeyword => false // Arrays cannot hold pairs
  //   case _ => true
  // }


}


