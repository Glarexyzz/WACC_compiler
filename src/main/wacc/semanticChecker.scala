package wacc
import scala.collection.mutable

case class SemanticError(message: String) extends Exception(message)


sealed trait SymbolEntry

case class VariableEntry(varType: Type) extends SymbolEntry
case class FunctionEntry(returnType: Type, params: Option[List[Param]]) extends SymbolEntry


class SymbolTable {
  //private val table: mutable.Map[String, mutable.Stack[SymbolEntry]] = mutable.Map()

  // Table to store symbol entries. 
  // Variables are stored in a mutable Stack (to handle scopes), while functions are stored directly.
  private val functionTable: mutable.Map[String, FunctionEntry] = mutable.Map()
  //private val variableScopes: mutable.Map[String, mutable.Stack[VariableEntry]] = mutable.Map()
  private val variableScopes: mutable.Stack[mutable.Map[String, VariableEntry]] = mutable.Stack()
  private var scopeLevel: Int = 0 // Tracks current scope depth
  private var functionStatus: Option[Type] = None // Tracks the return type of the current function

  
  def enterScope(): Unit = {
    scopeLevel += 1
    variableScopes.push(mutable.Map())  
  }

  /*
  def exitScope(): Unit = {
    // Remove all variables/functions declared at current scope level
    table.keys.foreach { key =>
      if (table(key).size > scopeLevel) {
        table(key).pop()
      }
      if (table(key).isEmpty) table.remove(key) // Clean up empty entries
    }
    scopeLevel -= 1
  }
  */

  def exitScope(): Unit = {
    if (scopeLevel > 0) {
      variableScopes.pop()
      scopeLevel -= 1
    }
  }
  
  def addVariable(name: String, varType: Type): Boolean = {
    // val entries = variableScopes.getOrElseUpdate(name, mutable.Stack())
    // // Add the variable to the current scope
    // entries.push(VariableEntry(varType))
    // true
    if (variableScopes.nonEmpty) {
      val currentScope = variableScopes.top // Get current scope
      if (currentScope.contains(name)) {
        return false // Variable already declared in this scope
      }
      currentScope(name) = VariableEntry(varType) // Add variable
      return true
    }
    false // No active scope
  }


  def addFunction(name: String, returnType: Type, params: Option[List[Param]]): Boolean = {
    // Check if the function already exists in the table
    if (functionTable.contains(name)) return false
    // Add the function entry
    val functionEntry = FunctionEntry(returnType, params)
    functionTable(name) = functionEntry
    true
  }
  def lookupVariable(name: String): Option[VariableEntry] = {
    variableScopes.reverseIterator.collectFirst {
      case scope if scope.contains(name) => scope(name)
    }
  }
  

  def lookupFunction(name: String): Option[FunctionEntry] = {
    functionTable.get(name)
  }

  def lookup(name: String): Option[SymbolEntry] = {
    lookupVariable(name).orElse(lookupFunction(name))
  }

  def setFunctionStatus(t: Option[Type]): Unit = {
    functionStatus = t
  }

  def checkFunctionStatus(): Option[Type] = functionStatus
  
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
  
  // 
  def checkProgram(program: Program): Option[String] = {
    val funcErrors = program.funcs.flatMap(checkFunc)
    // val noFuncErrors = program.funcs.foreach(checkFunctionDeclaration)
    val stmtErrors = checkStatement(program.stmt).toList
    val errors = funcErrors /*++ noFuncErrors*/ ++ stmtErrors
    if (errors.isEmpty) None else Some(errors.mkString("\n"))
  }

  def checkFunc(func: Func) : Option[String] =  {
    symbolTable.enterScope()
    symbolTable.addFunction(func.name, func.t, func.paramList)
    symbolTable.setFunctionStatus(Some(func.t))
    checkStatement(func.stmt)
    // Check function declarations
    // def checkFunctionDeclaration(func: Func): Unit = {
    //   println(s"Checking function: ${func.name}")
    symbolTable.setFunctionStatus(None) 
    symbolTable.exitScope()
    None
  }

  def checkStatement(stmt: Stmt): Option[String] = stmt match {
    case SkipStmt => None
    // <type> <ident> '=' <rValue>
    // No duplicate variable declarations in the same scope
    case DeclAssignStmt(t, name, value) => 
      checkRValue(value) match {
        case Left(error) => Some(error)
        case Right(rType) => 
          val can_add_if_no_duplicate = symbolTable.addVariable(name, rType)
          if (can_add_if_no_duplicate) None
          else Some(s"Semantic Error in Declaration: Variable $name is already declared")
      }

    case AssignStmt(lvalue, rvalue) => 
      (checkLValue(lvalue), checkRValue(rvalue)) match {
        case (Some(error), _) => Some(error)
        case (_, Left(error)) => Some(error)
        case (None, Right(rType)) => lvalue match {
          case LValue.LName(name) => symbolTable.lookup(name) match {
            case Some(VariableEntry(lType)) if areTypesCompatible(lType, rType) => None
            case Some(_) => Some(s"Semantic Error: Incompatible types in assignment to $name")
            case None => Some(s"Semantic Error: Variable $name not declared")
          }
          case LValue.LArray(ArrayElem(name, _)) => symbolTable.lookup(name) match {
            case Some(VariableEntry(ArrayType(lType))) if areTypesCompatible(lType, rType) => None
            case Some(_) => Some(s"Semantic Error: Incompatible types in assignment to $name")
            case None => Some(s"Semantic Error: Variable $name not declared")
          }

          case LValue.LPair(pairElem) => 
          // Extract types from both LHS and RHS pairs
            rvalue match {
              case RValue.RPair(pairElemR) => 
                (checkPairElem(pairElem), checkPairElem(pairElemR)) match {
                  case (Right(PairType(lLType, lRType)), Right(PairType(rLType, rRType))) =>
                    // Check if types match
                    if (areTypesCompatible(lLType, rLType) && areTypesCompatible(lRType, rRType)) {
                      None // Types are compatible
                    } else {
                      Some("Semantic Error: Incompatible types in pair assignment")
                    }
                  case (Left(error), _) => Some(error)  // Error in LHS pair element
                  case (_, Left(error)) => Some(error)  // Error in RHS pair element
                  case _ => Some("Semantic Error: Incompatible types between LHS and RHS pair elements")
                }
              case _ => Some("Semantic Error: Incompatible types in pair assignment")
            }
          }
      }

    // 'read' <lValue>
    // must be either type int or type char
    case ReadStmt(lvalue) => lvalue match {
      case LValue.LName(name) => symbolTable.lookup(name) match {
        case Some(VariableEntry(BaseType.IntType)) => None
        case Some(VariableEntry(BaseType.CharType)) => None
        case Some(_) => Some(s"Semantic Error: Variable $name must be of type int or char")
        case None => Some(s"Semantic Error: Variable $name not declared")
      }
      case _ => Some("Semantic Error: Invalid lvalue in read statement")
    }

    // 'free' <expr>
    case FreeStmt(expr) => checkExprType(expr, symbolTable) match {
      case Left(error) => Some(error)
      case Right(ArrayType(_)) | Right(PairType(_, _)) => None // The ‘free’ statement takes an argument that must be of type 𝜏[] or pair(𝜏, 𝜎).
      case Right(_) => Some("Semantic Error: `free` statement must be applied to an array or pair")
    }

    // 'return' <expr>
    // A return statement can only be present in the body of a function:
    // When executed, it will evaluate its argument, and pass teh resulting value back to the caller
    // of the function, exiting the current call immediately
    case ReturnStmt(expr) => 
      checkExprType(expr, symbolTable) match {
        case Left(error) => Some(error)
        case Right(retType) =>
          symbolTable.checkFunctionStatus() match {
            case Some(funcType) if areTypesCompatible(funcType, retType) => None
            case Some(funcType) => Some(s"Semantic Error: Incompatible return type, expected $funcType, found $retType")
            case None => Some("Semantic Error: Return statement must be inside a function")
          }
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
    case BodyStmt(body) => 
      symbolTable.enterScope()
      checkStatement(body)

    // <stmt> ';' <stmt>
    case SeqStmt(left, right) =>
      (checkStatement(left), checkStatement(right)) match {
        case (Some(errorL), Some(errorR)) => Some(s"$errorL, $errorR")  // Combine both errors
        case (Some(errorL), None) => Some(errorL)  // Only left statement has an error
        case (None, Some(errorR)) => Some(errorR)  // Only right statement has an error
        case (None, None) => None  // No errors in either statement
      }
  }

  def checkLValue(lvalue: LValue): Option[String] = lvalue match {
    case LValue.LName(name) => symbolTable.lookup(name) match {
      case Some(VariableEntry(_)) => None
      case Some(_) => Some(s"Error: $name is not a variable")
      case None => Some(s"Error: $name is not declared")
    }
    case LValue.LArray(ArrayElem(name, indices)) => symbolTable.lookup(name) match {
      case Some(VariableEntry(ArrayType(_))) => None
      case Some(_) => Some(s"Error: $name is not an array")
      case None => Some(s"Error: $name is not declared")
    }
    case LValue.LPair(pair) => checkPairElem(pair) match {
      case Right(_) => None
      case Left(error) => Some(error)
    }
  }

  def checkRValue(rvalue: RValue): Either[String, Type] = rvalue match {
    case RValue.RExpr(expr) => checkExprType(expr, symbolTable)
    //case class ArrayLiter(elements: Option[List[Expr]])
    case RValue.RArrayLiter(arrayLiter) =>
      val elements = arrayLiter.elements
      elements match {
        case None => Right(ArrayType(AnyType))
        case Some(list) => 
          list.foldLeft[Either[String, Type]](Right(ArrayType(AnyType))) {
            case (acc, expr) =>
              acc match {
                case Left(error) => Left(error) // If there's an error from previous elements, propagate it
                case Right(ArrayType(elementType)) =>
                  checkExprType(expr, symbolTable) match {
                    // Pattern match will fail on pattern case: 
                    // Right(IntType), Right(BoolType), Right(CharType), Right(StrType), Right(AnyType), Right(wacc.PairType(_, _))
                    case Right(exprType) => // AnyType is compatible with any type
                      if (areTypesCompatible(elementType, exprType)) Right(ArrayType(elementType)) // Accumulate the element type if they are compatible
                      else Left("Semantic Error: Array elements must be of the same type") // Return error if types mismatch
                    case Left(error) => Left(error) // Propagate any errors from checkExprType
                  }
                case _ => Left("Semantic Error: Elements must be ArrayType in Array")
              }
          }
        }
      
    case RValue.RNewPair(lExpr, rExpr) =>
      (checkExprType(lExpr, symbolTable), checkExprType(rExpr, symbolTable)) match {
        case (Right(lType), Right(rType)) => 
          (lType, rType) match {
            case (left: PairElemType, right: PairElemType) => Right(PairType(left, right))
            case _ => Left("Error: Pair elements must be of type pairElemType")
          }
        case (Left(lError), Left(rError)) => Left(s"$lError,\n $rError")
        case (Left(error), _) => Left(error)
        case (_, Left(error)) => Left(error)
      }
      // In a declaration, it would be on the right hand side, as RPair(FstElem(someLValue))
  // In other words, it should already have a value. 
  // So we should check the table to find LName(p), expecting us to find PairType(Type1, Type2)
  // then type of FstElem(LName(p)) should be Type1
  // Although it seems in the definitition, both sides are defined as PairElemType... which works since the rest extend it
  // and if someLValue is LArrayElem(name, IntLiteral(index)), then we should find ArrayType(Type)
  // and if someLValue is LPair(), then type would become 'pair' (erased type) PairKeyword
    case RValue.RPair(pairElem) => checkPairElem(pairElem)

    case RValue.RCall(name, argList) => Left("Error: Function calls not implemented")

  }

  def checkPairElem(pairElem: PairElem): Either[String, Type] = pairElem match {
      case PairElem.FstElem(LValue.LName(p)) => symbolTable.lookup(p) match {
        case Some(VariableEntry(PairType(leftType, _))) => Right(leftType)
        case Some(_) => Left(s"Error: $p is not a pair")
        case None => Left(s"Error: $p is not declared")
      }
      case PairElem.SndElem(LValue.LName(p)) => symbolTable.lookup(p) match {
        case Some(VariableEntry(PairType(_, rightType))) => Right(rightType)
        case Some(_) => Left(s"Error: $p is not a pair")
        case None => Left(s"Error: $p is not declared")
      }
      case PairElem.FstElem(LValue.LArray(ArrayElem(name, _))) => 
        symbolTable.lookup(name) match {
          case Some(VariableEntry(ArrayType(leftType))) => Right(leftType)
          case Some(_) => Left(s"Error: $name is not an array")
          case None => Left(s"Error: $name is not declared")
      }
      case PairElem.SndElem(LValue.LArray(ArrayElem(name, _))) => 
        symbolTable.lookup(name) match {
          case Some(VariableEntry(ArrayType(leftType))) => Right(leftType)
          case Some(_) => Left(s"Error: $name is not an array")
          case None => Left(s"Error: $name is not declared")
      }
      case PairElem.FstElem(LValue.LPair(_)) | PairElem.SndElem(LValue.LPair(_)) => Right(PairKeyword)
      case _ => Left("Error: Invalid pair element")
    }

  def checkExprType(expr: Expr, env: SymbolTable): Either[String, Type] = expr match {
    
    case IntLiteral(_) => Right(BaseType.IntType)
    case BoolLiteral(_) => Right(BaseType.BoolType)
    case CharLiteral(_) => Right(BaseType.CharType)
    case StrLiteral(_) => Right(BaseType.StrType)
    case PairLiteral => Left(s"Pair not defined")
    case Identifier(name) => env.lookup(name) match {
      case Some(VariableEntry(t)) => Right(t)
      case Some(FunctionEntry(t, _)) => Right(t)
      case None => Left(s"Semantic Error: $name is not declared")
    }
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

// override Option[String] addition method
extension (a: Option[String]) def ++(b: Option[String]): Option[String] = (a, b) match {
  case (Some(a), Some(b)) => Some(s"$a, $b")
  case (Some(a), None) => Some(a)
  case (None, Some(b)) => Some(b)
  case (None, None) => None
}
