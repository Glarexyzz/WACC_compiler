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
  var scopeLevel: Int = 0 // Tracks current scope depth
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
    val result = variableScopes.zipWithIndex.reverseIterator.collectFirst {
      case (scope, level) if level <= scopeLevel && scope.contains(name) => scope(name)
    }
    printf("Lookup for variable '%s' at scope level %d: %s\n", name, scopeLevel, result)
    result
  
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
  

  def checkProgram(program: Program): Option[String] = {
    symbolTable.enterScope()
    val funcErrors = program.funcs.flatMap(checkFunc)
    // val noFuncErrors = program.funcs.foreach(checkFunctionDeclaration)
    val stmtErrors = checkStatement(program.stmt).toList
    symbolTable.exitScope()
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
        // t name = (rType)
        // so t is the more 'broad case', rType is the more 'specific case'
        // Any is the broadest possible case?
        // can rType be weakened to t?
          if (areTypesCompatible(rType, t)) {
            printf("Checking declaration of variable '%s' of type %s\n", name, t)
            val can_add_if_no_duplicate = symbolTable.addVariable(name, t)
            if (can_add_if_no_duplicate) 
             println(s"Added variable $name of type $rType at scope level ${symbolTable.scopeLevel}")
             None
            else Some(s"Semantic Error in Declaration: Variable $name is already declared")
           }
          else Some(s"Semantic Error in Declaration: $rType is not compatible with $t for variable $name")

      }

    case AssignStmt(lvalue, rvalue) => 
      println("Checking assignment")
      (checkLValue(lvalue), checkRValue(rvalue)) match {
        case (Some(error), _) => Some(error)
        case (_, Left(error)) => Some(error)
        case (None, Right(rType)) => 
          println(s"Matching lvalue: $lvalue and rvalue $rType")
          // can rType be weakened to lType?
          lvalue match {
            case LValue.LName(name) => symbolTable.lookupVariable(name) match {
              case Some(VariableEntry(lType)) if areTypesCompatible(rType, lType) => 
                println(s"Assignment of $rType to $lType in $name")
                None
              case Some(_) => Some(s"Semantic Error in identifier: Incompatible types in assignment to $name")
              case None => Some(s"Semantic Errorin identifier: Variable $name not declared")
            }
            case LValue.LArray(ArrayElem(name, _)) => symbolTable.lookupVariable(name) match {
              case Some(VariableEntry(ArrayType(lType))) if areTypesCompatible(rType, lType) => None
              case Some(_) => Some(s"Semantic Error in array: Incompatible types in assignment to $name")
              case None => Some(s"Semantic Error in array: Variable $name not declared")
            }

            case LValue.LPair(pairElem) => 
            // Extract types from both LHS and RHS pairs
              rvalue match {
                case RValue.RPair(pairElemR) => 
                  (checkPairElem(pairElem), checkPairElem(pairElemR)) match {
                    case (Right(PairType(lLType, lRType)), Right(PairType(rLType, rRType))) =>
                      // Check if types match
                      if (areTypesCompatible(rLType, lLType) && areTypesCompatible(rRType, lRType)) {
                        None // Types are compatible
                      } else {
                        Some(s"Semantic Error: $rLType is not compatible with $lLType or $rRType is not compatible with $lRType")
                      }
                    case (Left(error), _) => Some(error)  // Error in LHS pair element
                    case (_, Left(error)) => Some(error)  // Error in RHS pair element
                    case _ => Some("Semantic Error: Incompatible types between LHS and RHS pair elements")
                  }
                case _ => Some(s"Semantic Error: $rvalue must be a Rpair")
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
    // the return should be more specific than the outside
    // so return can be weakened to function type
    case ReturnStmt(expr) => 
      checkExprType(expr, symbolTable) match {
        case Left(error) => Some(error)
        case Right(retType) =>
          symbolTable.checkFunctionStatus() match {
            case Some(funcType) if areTypesCompatible(retType, funcType) => None
            case Some(funcType) => Some(s"Semantic Error: $retType is incompatible to $funcType")
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
    case PrintlnStmt(expr) => 
      println(s"Checking println of $expr")
      checkExprType(expr, symbolTable) match {
        case Left(error) => Some(error)
        case Right(_) => None
      }

    // 'if' <expr> 'then' <stmt> 'else' <stmt> 'fi'
    case IfStmt(cond, thenStmt, elseStmt) => 
      checkExprType(cond, symbolTable) match {
        case Left(error) => Some(error)
        case Right(BaseType.BoolType) => 
          checkStatement(thenStmt) ++ checkStatement(elseStmt)
        case Right(_) => Some("Semantic Error: If condition must be a boolean")
      }

    // 'while' <expr> 'do' <stmt> 'done'
    case WhileStmt(cond, body) => 
      checkExprType(cond, symbolTable) match {
        case Left(error) => Some(error)
        case Right(BaseType.BoolType) => 
          checkStatement(body)
        case Right(_) => Some("Semantic Error: If condition must be a boolean")
      }

    // 'begin' <stmt> 'end'
    case BodyStmt(body) => 
      symbolTable.enterScope() // because some of our parsed output have a bodystmt wrapper
      checkStatement(body)

    // <stmt> ';' <stmt>
    case SeqStmt(left, right) =>
      checkStatement(left) match {
        case Some(errorL) => checkStatement(right) match {
          case Some(errorR) => Some(s"Left: $errorL, \nRight: $errorR")  // Combine errors from both left and right
          case None => Some(errorL)  // Only left has an error
        }
        case None => checkStatement(right)  // Check right if left has no error
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
          // Check the first element type and propagate it
          checkExprType(list.head, symbolTable) match {
            case Left(error) => Left(error)
            case Right(firstType) => 
              list.foldLeft[Either[String, Type]](Right(ArrayType(firstType))) {
                case (acc, expr) =>
                  acc match {
                    case Left(error) => Left(error) // If there's an error from previous elements, propagate it
                    case Right(ArrayType(elementType)) =>
                      checkExprType(expr, symbolTable) match {
                        case Right(exprType) => // any type (exprType) can be weakened to AnyType - the type of the array takes the weakest type (except Any)
                          if (areTypesCompatible(exprType, elementType)) Right(ArrayType(elementType)) // Accumulate the weakest type
                          else if (areTypesCompatible(elementType, exprType)) Right(ArrayType(exprType)) // Accumulate the weakest type
                          else Left(s"Semantic Error: Array elements $exprType must be compatible with $elementType") // Return error if types mismatch
                        case Left(error) => Left(error) // Propagate any errors from checkExprType
                      }
                    case _ => Left("Semantic Error: Elements must be ArrayType in Array")
                  }
              }
          }

        }
      
    case RValue.RNewPair(lExpr, rExpr) =>
      (checkExprType(lExpr, symbolTable), checkExprType(rExpr, symbolTable)) match {
        case (Right(lType), Right(rType)) => 
          Right(PairType(typeToPairElemType(lType), typeToPairElemType(rType)))
        case (Left(lError), Left(rError)) => Left(s"$lError,\n $rError")
        case (Left(error), _) => Left(error)
        case (_, Left(error)) => Left(error)
      }

    case RValue.RPair(pairElem) => checkPairElem(pairElem)

    // 'call' <ident> '(' <argList>? ')'
    case RValue.RCall(name, argList) => 
      symbolTable.lookupFunction(name) match {
        case Some(FunctionEntry(returnType, params)) => 
          params match {
            case None =>
              argList match {
                case None => Right(returnType)
                case Some(_) => Left("Semantic Error: Too many arguments in function call")
              }
            case Some(paramList) =>
              argList match {
                case None => Left("Semantic Error: Too few arguments in function call")
                case Some(args) =>
                  if (paramList.length != args.length) {
                    Left("Semantic Error: Number of arguments does not match function definition")
                    } 
                  else {
                    paramList.zip(args).foldLeft[Either[String, Type]](Right(returnType)) {
                      case (acc, (param, arg)) =>
                      acc match {
                        case Left(error) => Left(error) // Propagate any errors from previous arguments
                        case Right(_) =>
                          checkExprType(arg, symbolTable) match {
                            case Right(argType) =>
                            // can the parameter type be weakened to the argument type?
                            if (areTypesCompatible(param.t, argType)) Right(returnType) // Check if types are compatible
                            else Left(s"Semantic Error: Argument type mismatch for parameter ${param.name}")
                            case Left(error) => Left(error) // Propagate any errors from checking the argument
                          }
                      }
                    }
                  }
              }
          }
        case None => Left(s"Semantic Error: Function $name not declared")
      }

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
  
  def typeToPairElemType(t: Type): PairElemType = t match {
    case BaseType.IntType => BaseTElem(BaseType.IntType)
    case BaseType.BoolType => BaseTElem(BaseType.BoolType)
    case BaseType.CharType => BaseTElem(BaseType.CharType)
    case BaseType.StrType => BaseTElem(BaseType.StrType)
    case ArrayType(innerType) => ArrayTElem(ArrayType(innerType))
    case PairType(leftElem, rightElem) => PairKeyword
    case _ => NullType // a bit doubtful - does this cover all possible types?
  }

  def checkExprType(expr: Expr, env: SymbolTable): Either[String, Type] = expr match {
    
    case IntLiteral(_) => Right(BaseType.IntType)
    case BoolLiteral(_) => Right(BaseType.BoolType)
    case CharLiteral(_) => Right(BaseType.CharType)
    case StrLiteral(_) => Right(BaseType.StrType)

    case PairLiteral => Right(PairType(NullType, NullType))
    
    case Identifier(name) => env.lookup(name) match {
      case Some(VariableEntry(t)) => Right(t)
      case Some(FunctionEntry(t, _)) => Right(t)
      case None => Left(s"Semantic Error: $name is not declared")
    }

    // // <ident> ('[ <expr> ']')+
// case class ArrayElem(name: String, indices: List[Expr]) extends Expr
    case ArrayElem(name, indices) => 
      // all Expr in indices must be compatible with IntExpr
      indices.foldLeft[Either[String, Type]](Right(BaseType.IntType)) {
        case (acc, index) =>
          acc match {
          case Left(error) => Left(error) // Propagate any errors from previous indices
          case Right(_) =>
            checkExprType(index, env) match {
              // can int be weakened to the index type? (since int is the most specific type of element)
              case Right(t) => areTypesCompatible(BaseType.IntType, t) match {
                case true => Right(BaseType.IntType) // Continue if index is of type int
                case false => Left(s"Semantic Error: Array indices must be compatible with type int but are $t") // Error if index is not of type int
              }
              case Left(error) => Left(error) // Propagate any errors from checkExprType
            }
          }
      } match {
        case Right(_) =>
          env.lookup(name) match {
        case Some(VariableEntry(ArrayType(innerType))) => Right(ArrayType(innerType))
        case Some(_) => Left(s"Semantic Error: $name is not an array")
        case None => Left(s"Semantic Error: $name is not declared")
          }
        case Left(error) => Left(error)
      }


    case BinaryOp(left, op, right) if Set(
      BinaryOperator.Add,
      BinaryOperator.Subtract,
      BinaryOperator.Multiply,
      BinaryOperator.Divide,
      BinaryOperator.Modulus
    ).contains(op) =>
      (checkExprType(left, env), checkExprType(right, env)) match {
        // case (Right(BaseType.IntType), Right(BaseType.IntType)) => Right(BaseType.IntType) 
        case (Right(t1), Right(t2)) =>
          if (areTypesCompatible(BaseType.IntType, t1) && areTypesCompatible(BaseType.IntType, t2)) { 
            Right(BaseType.IntType) 
          } else { 
            Left(s"Semantic Error: $t1 and $t2 are incompatible for arithmetic operation") 
          }
        case (Left(error1), Left(error2)) => Left(s"$error1, $error2")
        case (Left(error), _) => Left(error)
        case (_, Left(error)) => Left(error)
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
        case (Right(t1), Right(t2)) => 
          if (areTypesCompatible(t1, t2) || areTypesCompatible(t2, t1)) 
            Right(BaseType.BoolType) 
          else Left(s"Semantic Error: Incompatible types $t1, $t2 for equality comparison")
        case (Left(error1), Left(error2)) => Left(s"$error1, $error2")
        case (Left(error), _) => Left(error)
        case (_, Left(error)) => Left(error)
      }

    case BinaryOp(left, op, right) if Set(
      BinaryOperator.And,
      BinaryOperator.Or
    ).contains(op) =>
      (checkExprType(left, env), checkExprType(right, env)) match {
        case (Right(BaseType.BoolType), Right(BaseType.BoolType)) => Right(BaseType.BoolType) 
        case (Right(t1), Right(t2)) => Left(s"Semantic Error: $t1 and $t2 are incompatible for logical operation")
        case (Left(error1), Left(error2)) => Left(s"$error1, $error2")
        case (Left(error), _) => Left(error)
        case (_, Left(error)) => Left(error)
      }

    // Unary Operators: !, -, len, ord, chr
    case UnaryOp(UnaryOperator.Not, expr) =>
      checkExprType(expr, env) match {
        case Right(BaseType.BoolType) => Right(BaseType.BoolType) 
        case Right(t) => Left(s"Semantic Error: `!` operator requires a boolean operand but found $t") 
        case Left(error) => Left(error)
      }

    case UnaryOp(UnaryOperator.Negate, expr) =>
      checkExprType(expr, env) match {
        case Right(t) => 
          if (areTypesCompatible(BaseType.IntType, t)) 
            Right(BaseType.IntType)
          else Left(s"Semantic Error: `-` operator requires an integer operand but found $t") 
        case Left(error) => Left(error)
      }

    case UnaryOp(UnaryOperator.Length, expr) =>
      checkExprType(expr, env) match {
        case Right(ArrayType(_)) => Right(BaseType.IntType) 
        case Right(t) => Left(s"Semantic Error: `len` operator requires an array but found $t")
        case Left(error) => Left(error)
      }

    case UnaryOp(UnaryOperator.Ord, expr) =>
      checkExprType(expr, env) match {
        case Right(BaseType.CharType) => Right(BaseType.IntType) 
        case Right(t) => Left(s"Semantic Error: `ord` operator requires a character but found $t") 
        case Left(error) => Left(error)
      }

    case UnaryOp(UnaryOperator.Chr, expr) =>
      checkExprType(expr, env) match {
        case Right(t) => 
          if (areTypesCompatible(BaseType.IntType, t)) 
            Right(BaseType.IntType)
          else Left(s"Semantic Error: `-` operator requires an integer operand but found $t") 
        case Left(error) => Left(error)
      }
  }



  // t1 compatible to t2 = t1 can be weakened to t2
  def areTypesCompatible(t1: Type, t2: Type): Boolean = (t1, t2) match {
    // char[] can be treated as string
    case (ArrayType(BaseType.CharType), BaseType.StrType) => true 
    // string cannot be treated as char[]
    case (BaseType.StrType, ArrayType(BaseType.CharType)) => false // string to char[] is not allowed
    
    // arrays with compatible inner types
    case (ArrayType(innerType1), ArrayType(innerType2)) =>
      areTypesCompatible(innerType1, innerType2) // recursively check if inner types are compatible
    // things in an array are compatible with things of the same type outside the array
    // case (ArrayType(innerType), innerType2) =>
    //   areTypesCompatible(innerType, innerType2) // check if inner type is compatible with the other type
    // case (innerType1, ArrayType(innerType2)) =>
    //   areTypesCompatible(innerType1, innerType2)

    case (BaseType.IntType, BaseType.IntType) => true
    case (BaseType.BoolType, BaseType.BoolType) => true
    case (BaseType.CharType, BaseType.CharType) => true
    case (BaseType.StrType, BaseType.StrType) => true

    case (PairType(leftElem1, rightElem1), PairType(leftElem2, rightElem2)) =>
      areTypesCompatible(leftElem1, leftElem2) && areTypesCompatible(rightElem1, rightElem2)
    case(BaseTElem(elem1), BaseTElem(elem2)) => 
      areTypesCompatible(elem1, elem2)
    case(ArrayTElem(elem1), ArrayTElem(elem2)) => 
      areTypesCompatible(elem1, elem2)
    case(PairKeyword, PairKeyword) => true
    // any type can be weaken to AnyType
    case (_, AnyType) => true
    // I may regret this, but AnyType can be weakened to any type. So ArrayType(AnyType) is compatible with ArrayType(IntType) in declaration
    case (AnyType, _) => true
    

    // any PairElemType can be weakened to a Null Type
    case (BaseTElem(_), NullType) => true
    case (ArrayTElem(_), NullType) => true
    case (PairKeyword, NullType) => true
    // any PairElemType can be weakened to a PairKeyword and vice versa
    case (PairType(_, _), PairKeyword) => true
    case (PairKeyword, PairType(_, _)) => true

    // We can treat the wrapper and non-wrapper types as the same (can we?)
    case(BaseTElem(elem1), elem2) =>
      areTypesCompatible(elem1, elem2)
    case(elem1, BaseTElem(elem2)) =>
      areTypesCompatible(elem1, elem2)
    case(ArrayTElem(elem1), elem2) =>
      areTypesCompatible(elem1, elem2)
    case(elem1, ArrayTElem(elem2)) =>
      areTypesCompatible(elem1, elem2)

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
