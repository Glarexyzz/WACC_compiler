package wacc
import scala.collection.mutable
import wacc.Constants._

sealed trait SymbolEntry

case class VariableEntry(varType: Type) extends SymbolEntry
case class FunctionEntry(returnType: Type, params: List[Param]) extends SymbolEntry


class SymbolTable {
  // Table to store symbol entries. 
  // Variables are stored in a mutable Stack (to handle scopes), while functions are stored directly.
  private val functionTable: mutable.Map[String, FunctionEntry] = mutable.Map()
  private val variableScopes: mutable.Stack[mutable.Map[String, VariableEntry]] = mutable.Stack()
  private var functionStatus: Option[(String, Type)] = None // Tracks the return type of the current function

  var scopeLevel: Int = 0 // Tracks current scope depth
  var nVariablesInScope = 0        // Number of variables in the **current scope**.
  var maxConcurrentVariables = 0   // Global "high-water mark" for variable count.
  private var nVariableRegs = 0            // Total number of variables across all scopes (for function params etc.)

  def getFunctionTable: Map[String, FunctionEntry] = functionTable.toMap
  def getVariableScopes: List[Map[String, VariableEntry]] = variableScopes.toList.map(_.toMap)
  def getMaxConcurrentVariables: Int = maxConcurrentVariables
  def getCurrentFunctionParamsNum(funcName: String): Int = functionTable(funcName).params.length
  
  def enterScope(): Unit = {
    //println(s"Entering scope: ${getVariableScopes}")
    nVariableRegs += nVariablesInScope  // Carry over total variables seen so far.
    scopeLevel += 1
    val newScope = if (functionStatus.isDefined && variableScopes.nonEmpty) {
      // If we're inside a function, copy the previous scope
      variableScopes.top.clone()
    } else {
      mutable.Map[String, VariableEntry]()
    }
    variableScopes.push(newScope)
    nVariablesInScope = 0
  }

  def exitScope(): Unit = {
    if (scopeLevel > 0) {
      //println(s"Exiting scope: ${getVariableScopes}")
      val exitingScope = variableScopes.pop()
      val parentScope = variableScopes.top
      var nExitVars = exitingScope.size - parentScope.size
      //println(s"exiting scope: $exitingScope")
      //println(s"exiting scope size: ${exitingScope.size}")
      nVariableRegs += nVariablesInScope  // Carry over total variables seen so far.
      //println(s"nVarRegs before exiting: $nVariableRegs")
      nVariablesInScope = 0  // The parent scope remove child scope variables
      if (functionStatus.isDefined) {
        functionStatus match {
          case Some(name, t) =>
            val nParams = getCurrentFunctionParamsNum(name)
            nVariableRegs += nParams
            //println(s"nVarRegs going through parameters: $nVariableRegs")
            if (parentScope.size != 0) {
              nExitVars += nParams
            }
          case None => 
        }
      } 

      // When we pop a scope, all its variables "die"
      nVariableRegs -= nExitVars
      //println(s"nVarRegs after exiting: $nVariableRegs")
      maxConcurrentVariables = Math.max(maxConcurrentVariables, nVariableRegs) // Update max
      scopeLevel -= 1
    }
  }
  
  def addVariable(name: String, varType: Type): Boolean = {
    if (variableScopes.nonEmpty) {
      //println(s"variableName: $name, varType: $varType")
      val currentScope = variableScopes.top // Get current scope
      if (currentScope.contains(name) && !functionStatus.isDefined) {
        return false // Variable already declared in this scope
      }
      currentScope(name) = VariableEntry(varType)
      nVariablesInScope += 1

      val totalNowAlive = nVariableRegs + nVariablesInScope
      //println(s"nVarRegs: $nVariableRegs")
      //println(s"nVarScope: $nVariablesInScope")
      maxConcurrentVariables = Math.max(maxConcurrentVariables, totalNowAlive)
      //println(s"maxVarNum: $maxConcurrentVariables")
      
      return true
    }
    false // No active scope
  }


  def addFunction(name: String, returnType: Type, params: Option[List[Param]]): Boolean = {
    // Check if the function already exists in the table
    if (functionTable.contains(name)) return false
    // Add the function entry
    val paramsList: List[Param] = params match {
      case Some(parameters) => parameters
      case None => List()
    }
    val functionEntry = FunctionEntry(returnType, paramsList)
    functionTable(name) = functionEntry
    true
  }

  def lookupVariable(name: String): Option[VariableEntry] = {

    val result = if (functionStatus.isDefined) {
    // Only check the most recent (innermost) scope
      variableScopes.headOption.flatMap(scope => scope.get(name))
    } else {
      variableScopes.zipWithIndex.reverseIterator.toList.reverse.collectFirst {
        case (scope, index) if index >= (variableScopes.size - scopeLevel) && scope.contains(name) => 
          scope(name)
      }
    }
    result
  }

  def lookupFunction(name: String): Option[FunctionEntry] = {
    functionTable.get(name)
  }

  def lookup(name: String): Option[SymbolEntry] = {
    lookupVariable(name).orElse(lookupFunction(name))
  }

  def setFunctionStatus(name: Option[String] = None, returnType: Option[Type]): Unit = {
    returnType match {
      case Some(t) => 
        val funcName = name match {
          case Some(n) => n
          case None => ""
        }
        functionStatus = Some(funcName, t)
      case None => functionStatus = None
    }
  }

  def checkFunctionStatus(): Option[Type] = functionStatus match {
    case Some(n, t) => Some (t)
    case None => None
  }
  
}

object semanticChecker {

  private val constants: mutable.Map[String, (Type, Any)] = mutable.Map()
  private val unusedVars: mutable.Set[String] = mutable.Set.empty[String]
  val symbolTable: SymbolTable = new SymbolTable

  def addConstant(name: String, value: (Type, Any)): Unit = {
    constants(name) = value
  }

  def removeConstant(name: String) = {
    constants -= name
  }

  def checkSemantic(parsed: Any): Either[String, (SymbolTable, mutable.Map[String, (Type, Any)], mutable.Set[String])] = parsed match {
    case program: Program => checkProgram(program).toLeft(symbolTable, constants, unusedVars)
    case _ => Left(s"Unknown parsed structure")
  }
  

  def checkProgram(program: Program): Option[String] = {
    symbolTable.enterScope()
    
    // Adds all valid function declarations
    val funcDeclarationErrors = program.funcs.flatMap(addFuncDeclaration)
    
    // Checks function bodies
    val funcBodyErrors = program.funcs.flatMap(checkFunc)
    
    // Checks main program
    val stmtErrors = checkStatement(program.stmt).toList
    
    val errors = funcDeclarationErrors ++ funcBodyErrors ++ stmtErrors
    if (errors.isEmpty) None else Some(errors.mkString("\n"))
  }

  // Helper function to add function declarations to the symbol table
  def addFuncDeclaration(func: Func): Option[String] = {
    val paramNames: List[String] = func.paramList.getOrElse(Nil).map(_.name)
    val name: String = func.name
    
    // Check for duplicate parameter names
    if (paramNames.length != paramNames.toSet.size) {
      return Some(s"Invalid parameters in function $name. Duplicate names for parameters is not allowed.")
    }
    
    // Add the function to the symbol table
    val added = symbolTable.addFunction(func.name, func.t, func.paramList)
    if (!added) {
      return Some(s"Invalid redeclaration of function $name.")
    }
    
    None
  }

  def checkFunc(func: Func): Option[String] = {
    symbolTable.enterScope()

    symbolTable.setFunctionStatus(Some(func.name), Some(func.t))
    
    // Add function parameters to the symbol table
    func.paramList.foreach { params =>
      params.foreach { param =>
        symbolTable.addVariable(param.name, param.t)
        //println("is function's parameter\n")
        symbolTable.nVariablesInScope -= 1
        symbolTable.maxConcurrentVariables -= 1
      }
    }
    
    // Checks the function's body
    val bodyCheckResult = checkStatement(func.stmt) match {
      case None => None
      case Some(error) => Some(s"In function ${func.name},\n$error")
    }
    symbolTable.exitScope()
    symbolTable.setFunctionStatus(returnType = None)
    bodyCheckResult
  }

  def evaluateExpr(value: Expr): Option[Int] = value match {
    case IntLiteral(n) => Some(n.toInt)
    case BoolLiteral(b) => if (b) Some(trueValue) else Some(falseValue)
    case CharLiteral(c) => Some(c.toInt)
    case Identifier(name) => constants.get(name) match {
      case Some((_, n: Int)) => Some(n)
      case _ => None
    }
    case UnaryOp(op, expr) => evaluateExpr(expr) match {
      case Some(result) => op match {
        case UnaryOperator.Negate => Some((-result))
        case UnaryOperator.Not => Some(((result - 1).abs))
        case UnaryOperator.Ord => Some(result)
        case UnaryOperator.Chr => Some(result)
        case _ => None
      }
      case _ => None
    }
    case BinaryOp(expr1, op, expr2) => (evaluateExpr(expr1), evaluateExpr(expr2)) match {
      case (Some(result1), Some(result2)) => op match {
          case BinaryOperator.Add          => Some(result1 + result2)
          case BinaryOperator.Subtract     => Some(result1 - result2)
          case BinaryOperator.Multiply     => Some(result1 * result2)
          case BinaryOperator.Divide       => if (result2 != 0) Some(result1 / result2) else None
          case BinaryOperator.Modulus      => if (result2 != 0) Some(result1 % result2) else None
          case BinaryOperator.And          => if (result1 == trueValue && result2 == trueValue) Some(trueValue) else Some(falseValue)
          case BinaryOperator.Or           => if (result1 == trueValue || result2 == trueValue) Some(trueValue) else Some(falseValue)
          case BinaryOperator.Greater      => if (result1 >  result2) Some(trueValue) else Some(falseValue)
          case BinaryOperator.GreaterEqual => if (result1 >= result2) Some(trueValue) else Some(falseValue)
          case BinaryOperator.Less         => if (result1 <  result2) Some(trueValue) else Some(falseValue)
          case BinaryOperator.LessEqual    => if (result1 <= result2) Some(trueValue) else Some(falseValue)
          case BinaryOperator.Equal        => if (result1 == result2) Some(trueValue) else Some(falseValue)
          case BinaryOperator.NotEqual     => if (result1 != result2) Some(trueValue) else Some(falseValue)
        }
      case _ => None
    }
    case _ => None
  }

  def evaluateArray(array: ArrayLiter): Option[List[Any]] = array.elements match {
    case Some(exprList) =>
      val evaluated = exprList.map {
        case expr: Expr => expr match {
          case Identifier(name) => constants.get(name) match {
            case Some((_, arr: List[Any])) => Some(arr)
            case _ => evaluateExpr(expr)
          }
          case arr: ArrayLiter => evaluateArray(arr)
          case _ => evaluateExpr(expr)
        }
      }
      if (evaluated.contains(None)) None else Some(evaluated.map(_.get))
    case None => Some(List())
  }

  // Returns true if the array type can be added to the constants map
  def allowedArrayType(t: Type): Boolean = t match {
    case ArrayType(innertype) => allowedArrayType(innertype)
    case BaseType.IntType => true
    case BaseType.BoolType => true
    case _ => false
  }

  // Extracts the needed RValue if it is a valid value to be evaluated
  def extract(value: RValue): Option[Any] = value match {
    case RValue.RExpr(expr) => Some(expr)
    case RValue.RArrayLiter(array) => Some(array)
    case _ => None
  }

  // Checks constant validity, and adds it if it is valid
  def checkAndAddConstant(t: Type, name: String, value: RValue) = {
    extract(value) match {
      case Some(expr: Expr) => evaluateExpr(expr) match {
        case Some(n) => t match {
          case BaseType.IntType => 
            if (n.abs <= max16BitUnsigned || n <= min32BitSigned){
              addConstant(name, (BaseType.IntType, n))
            }
          case BaseType.BoolType =>
            if (n == trueValue || n == falseValue) addConstant(name, (BaseType.BoolType, n))
          case BaseType.CharType =>
            if (n >= 0 && n <= 127) addConstant(name, (BaseType.CharType, n))
          case _ =>
        }
        case _ =>
      }
      case Some(array: ArrayLiter) => evaluateArray(array) match {
        case Some(list) => 
          if (allowedArrayType(t)) addConstant(name, (t, list))
        case None =>
      }
      case _ =>
    }
  }

  // Checks unused variable validity, and adds it if it is valid
  def checkAndAddUnusedVariable(t: Type, name: String, value: RValue) = {
    extract(value) match {
      case Some(expr: Expr) => evaluateExpr(expr) match {
        case Some(n) => t match {
          case BaseType.IntType => 
            if (n.abs <= max16BitUnsigned || n <= min32BitSigned){
              unusedVars.add(name)
            }
          case BaseType.BoolType =>
            if (n == trueValue || n == falseValue) addConstant(name, (BaseType.BoolType, n))
          case BaseType.CharType =>
            if (n >= 0 && n <= 127) unusedVars.add(name)
          case _ =>
        }
        case _ =>
      }
      case _ =>
    }
  }

  def checkStatement(stmt: Stmt): Option[String] = stmt match {
    case SkipStmt => None

    case DeclAssignStmt(t, name, value) => 
      checkRValue(value) match {
        case Left(error) => Some(error)
        case Right(rType) => 

          if (isCompatibleTo(rType, t)) {
            //println(s"\nadding $name to the table")
            val can_add_if_no_duplicate = symbolTable.addVariable(name, t)
            if (can_add_if_no_duplicate) {
              checkAndAddUnusedVariable(t, name, value)
              checkAndAddConstant(t, name, value)
              None
            }
            else Some(s"Semantic Error in Declaration: Variable $name is already declared")
           }
          else Some(s"Semantic Error in Declaration: $rType is not compatible with $t for variable $name")

      }

    case AssignStmt(lvalue, rvalue) => 
      (checkLValue(lvalue), checkRValue(rvalue)) match {
        case (Some(error), _) => Some(error)
        case (_, Left(error)) => Some(error)
        case (None, Right(rType)) => 
          lvalue match {
            case LValue.LName(name) => symbolTable.lookupVariable(name) match {
              case Some(VariableEntry(lType)) => 
                if (isCompatibleTo(rType, lType)) {
                  removeConstant(name)
                  unusedVars.remove(name)
                  None
                } else {
                  Some(s"Semantic Error in Assignment of identifier $name: $rType is not compatible to $lType identifier $name")
                }
              case None => Some(s"Semantic Error in Assignment: Identifer $name not declared")
            }

            case LValue.LArray(ArrayElem(name, _)) => symbolTable.lookupVariable(name) match {
              case Some(VariableEntry(ArrayType(lType))) => 
                if (isCompatibleTo(rType, lType)) {
                  removeConstant(name)
                  unusedVars.remove(name)
                  None
                } else {
                  Some(s"Semantic Error in Assignment of array $name: $rType is not compatible to $lType")
                }
              case Some(t) => Some(s"Semantic Error in Assignment of array $name: $rType is not compatible to $t")
              case None => Some(s"Semantic Error in Assignment: Array $name not declared")
            }

            case LValue.LPair(pairElem) => 
              (checkPairElem(pairElem), checkRValue(rvalue)) match {
                case (Right(AnyType), Right(AnyType)) => 
                  Some(s"Semantic Error in Assignment of pair $pairElem: Types of both pairs $pairElem and $rvalue cannot be unknown")

                // LHS is a known type and RHS is a known type
                case (Right(lType), Right(rType)) =>
                  if (isCompatibleTo(rType, lType)) None 
                  else Some(s"Semantic Error in Assignment of pair $pairElem: $rType is not compatible to $lType")

                // Error cases
                case (Left(error1), Left(error2)) => Some(s"$error1,\n$error2")
                case (Left(error), _) => Some(error)
                case (_, Left(error)) => Some(error)
              }
        }
      }

    // 'read' <lValue>
    // must be either type int or type char
    case ReadStmt(lvalue) => lvalue match {
      case LValue.LName(name) => symbolTable.lookup(name) match {
        case Some(VariableEntry(BaseType.IntType)) => 
          unusedVars.remove(name)
          removeConstant(name)
          None
        case Some(VariableEntry(BaseType.CharType)) =>
          unusedVars.remove(name)
          removeConstant(name)
          None
        case Some(t) => Some(s"Semantic Error in Read: Identifier $name must be of type int or char, but got $t instead")
        case None => Some(s"Semantic Error in Read: Identifier $name not declared")
      }
      case LValue.LPair(pairElem) => 
        checkPairElem(pairElem) match {
          case Right(BaseTElem(BaseType.IntType)) => None
          case Right(BaseTElem(BaseType.CharType)) => None
          case Right(t) => Some(s"Semantic Error in Read: Type of pair $pairElem must be type int or char, but got $t instead")
          case Left(error) => Some(error)
        }
      case _ => Some(s"Semantic Error in Read: Invalid lvalue in read statement")
    }

    // 'free' <expr>
    case FreeStmt(expr) => checkExprType(expr, symbolTable) match {
      case Left(error) => Some(error)
      case Right(ArrayType(_)) | Right(PairType(_, _)) => None // The ‘free’ statement takes an argument that must be of type 𝜏[] or pair(𝜏, 𝜎).
      case Right(t) => Some(s"Semantic Error in Free: `free` statement must be applied to an array or pair, but got $t instead")
    }

    // 'return' <expr>
    case ReturnStmt(expr) => 
      checkExprType(expr, symbolTable) match {
        case Left(error) => Some(error)
        case Right(retType) =>
          symbolTable.checkFunctionStatus() match {
            case Some(funcType) if isCompatibleTo(retType, funcType) => None
            case Some(funcType) => Some(s"Semantic Error in Return: Return type $retType of $expr is incompatible to function type $funcType")
            case None => Some("Semantic Error in Return: Return statement must be inside a function")
          }
      }


    // 'exit' <IntType>
    case ExitStmt(expr) => 
      checkExprType(expr, symbolTable) match {
        case Left(error) => Some(error) // Propagate any errors from checking the expression
        case Right(BaseType.IntType) => None // Valid type for exit (int)
        case Right(t) => Some(s"Semantic Error: Exit statement must have an integer argument, but got $t instead") // Invalid type
      }

    // 'print' <expr>
    case PrintStmt(expr) => 
      checkExprType(expr, symbolTable) match {
        case Left(error) => Some(error)
        case Right(_) => None
    }

    // 'println' <expr>
    case PrintlnStmt(expr) => 
      checkExprType(expr, symbolTable) match {
        case Left(error) => Some(error)
        case Right(_) => None
      }

    // 'if' <expr> 'then' <stmt> 'else' <stmt> 'fi'
    case IfStmt(cond, thenStmt, elseStmt) => 
      checkExprType(cond, symbolTable, true) match {
        case Left(error) => Some(error)

        case Right(BaseType.BoolType) => 
          symbolTable.enterScope()
          val resultThenStmt: Option[String] = checkStatement(thenStmt)
          symbolTable.exitScope()
          symbolTable.enterScope()
          val resultElseStmt: Option[String] = checkStatement(elseStmt)
          symbolTable.exitScope()
          resultThenStmt ++ resultElseStmt
        
        case Right(t) => Some(s"Semantic Error in If: If condition must be a boolean, but got $t instead")
      }

    // 'while' <expr> 'do' <stmt> 'done'
    case WhileStmt(cond, body) => 
      checkExprType(cond, symbolTable, true) match {
        case Left(error) => Some(error)
        case Right(BaseType.BoolType) => 
          symbolTable.enterScope()
          val result: Option[String] = checkStatement(body)
          symbolTable.exitScope()
          result
        case Right(t) => Some(s"Semantic Error in While: If condition must be a boolean, but got $t instead")
      }

    // 'begin' <stmt> 'end'
    case BodyStmt(body) => 
      symbolTable.enterScope() // because some of our parsed output have a bodystmt wrapper
      val result: Option[String] = checkStatement(body)
      symbolTable.exitScope()
      result

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
      case Some(_) => Some(s"Error in checking left value of $name: expected variable, but got function instead")
      case None => Some(s"Error in checking left value: $name is not declared")
    }
    case LValue.LArray(ArrayElem(name, indices)) => symbolTable.lookupVariable(name) match {
      case Some(VariableEntry(ArrayType(_))) => 
        unusedVars.remove(name)
        removeConstant(name)
        None
      case Some(VariableEntry(t)) => Some(s"Error in checking left value of $name: expected array, but got $t instead")
      case None => Some(s"Error in checking left value: $name is not declared")
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
                          if (isCompatibleTo(exprType, elementType)) Right(ArrayType(elementType)) // Accumulate the weakest type
                          else if (isCompatibleTo(elementType, exprType)) Right(ArrayType(exprType)) // Accumulate the weakest type
                          else Left(s"Semantic Error: Array elements $exprType must be compatible with $elementType") // Return error if types mismatch
                        case Left(error) => Left(error) // Propagate any errors from checkExprType
                      }
                    case Right(t) => Left(s"Semantic Error: Elements must be ArrayType in Array, but got $t instead")
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
            case List() =>
              argList match {
                case None => Right(returnType)
                case Some(_) => Left(s"Semantic Error: Too many arguments in function call of $name")
              }
            case paramList =>
              argList match {
                case None => Left(s"Semantic Error: Too few arguments in function call of $name")
                case Some(args) =>
                  if (paramList.length != args.length) {
                    Left(s"Semantic Error: Number of arguments does not match function definition of $name")
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
                            if (isCompatibleTo(param.t, argType)) Right(returnType) // Check if types are compatible
                            else Left(s"Semantic Error: Argument type mismatch for parameter ${param.name} in $name")
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
      case PairElem.FstElem(LValue.LName(p)) => symbolTable.lookupVariable(p) match {
        case Some(VariableEntry(PairType(leftType, _))) => Right(leftType)
        case Some(VariableEntry(t)) => Left(s"Error: $p should be a pair, got $t instead")
        case None => Left(s"Error: $p is not declared")
      }
      case PairElem.SndElem(LValue.LName(p)) => symbolTable.lookupVariable(p) match {
        case Some(VariableEntry(PairType(_, rightType))) => Right(rightType)
        case Some(VariableEntry(t)) => Left(s"Error: $p should be a pair, got $t instead")
        case None => Left(s"Error: $p is not declared")
      }
      case PairElem.FstElem(LValue.LArray(ArrayElem(name, _))) => 
        symbolTable.lookupVariable(name) match {
          case Some(VariableEntry(ArrayType(PairType(leftType, _)))) => Right(leftType)
          case Some(VariableEntry(t)) => Left(s"Error: $name should be an array, got $t instead")
          case None => Left(s"Error: $name is not declared")
      }
      case PairElem.SndElem(LValue.LArray(ArrayElem(name, _))) => 
        symbolTable.lookupVariable(name) match {
          case Some(VariableEntry(ArrayType(PairType(_, rightType)))) => Right(rightType)
          case Some(VariableEntry(t)) => Left(s"Error: $name should be an array, got $t instead")
          case None => Left(s"Error: $name is not declared")
      }
      case PairElem.FstElem(LValue.LPair(pairElem)) => 
        checkPairElem(pairElem) match {
          case Right(PairKeyword) => Right(AnyType)
          case Right(t) => Right(PairKeyword)
          case Left(err) => Left(err)
      }
      case PairElem.SndElem(LValue.LPair(pairElem)) => 
        checkPairElem(pairElem) match {
          case Right(PairKeyword) => Right(AnyType)
          case Right(t) => Right(PairKeyword)
          case Left(err) => Left(err)
      }

      case _ => Left("Error: Invalid pair element")
    }
  
  def typeToPairElemType(t: Type): PairElemType = t match {
    case BaseType.IntType => BaseTElem(BaseType.IntType)
    case BaseType.BoolType => BaseTElem(BaseType.BoolType)
    case BaseType.CharType => BaseTElem(BaseType.CharType)
    case BaseType.StrType => BaseTElem(BaseType.StrType)
    case ArrayType(innerType) => ArrayTElem(ArrayType(innerType))
    case PairType(leftElem, rightElem) => PairKeyword
    case _ => NullType 
  }

  // This function gets an item from an array, if the indices are valid and any variables are constants
  def getArrayItem(arr: Any, indices: List[Expr]): Option[Any] = indices match {
    case Nil => Some(arr)
    case i :: rest => i match {
      case Identifier(name) => constants.get(name) match {
        case Some((BaseType.IntType, index)) => (arr, index) match {
          case (list: List[Any], n: Int) if indexInRange(list, n) => getArrayItem(list(n), rest)
          case _ => None
        }
        case Some(_) => None
        case _ => None
      }
      case IntLiteral(n) => arr match {
        case list: List[Any] if indexInRange(list, n.toInt) => getArrayItem(list(n.toInt), rest)
        case _ => None
      }
      case ArrayElem(name, is) => constants.get(name) match {
        case Some((_, outerlist: List[Any])) => getArrayItem(outerlist, is) match {
          case Some(n) => (arr, n) match {
            case (list: List[Any], n: Int) if indexInRange(list, n) => getArrayItem(list(n.toInt), rest)
            case _ => None
          }
          case _ => None
        }
        case _ => None
      }
      case _ => None
    }
  }

  // Checks range of an index in a list
  def indexInRange(list: List[Any], n: Int): Boolean = n < list.length && n >= 0

  def checkExprType(expr: Expr, env: SymbolTable, isIfOrWhile: Boolean = false): Either[String, Type] = expr match {
    
    case IntLiteral(_) => Right(BaseType.IntType)
    case BoolLiteral(_) => Right(BaseType.BoolType)
    case CharLiteral(_) => Right(BaseType.CharType)
    case StrLiteral(_) => Right(BaseType.StrType)

    case PairLiteral => Right(PairType(NullType, NullType))
    
    case Identifier(name) => 
      unusedVars.remove(name)
      if (isIfOrWhile) {
        removeConstant(name)
      }
      env.lookup(name) match {
        case Some(VariableEntry(t)) => Right(t)
        case Some(FunctionEntry(t, _)) => Right(t)
        case None => Left(s"Semantic Error in Expression: Identifier $name is not declared")
      }

    // // <ident> ('[ <expr> ']')+
// case class ArrayElem(name: String, indices: List[Expr]) extends Expr
    case ArrayElem(name, indices) => constants.get(name) match {
      case Some((_, arr)) => if (getArrayItem(arr, indices) == None || isIfOrWhile) removeConstant(name)
      case _ =>
    }
      // all Expr in indices must be compatible with IntExpr
      indices.foldLeft[Either[String, Type]](Right(BaseType.IntType)) {
        case (acc, index) =>
          acc match {
          case Left(error) => Left(error) // Propagate any errors from previous indices
          case Right(_) =>
            checkExprType(index, env) match {
              // can int be weakened to the index type? (since int is the most specific type of element)
              case Right(t) => isCompatibleTo(BaseType.IntType, t) match {
                case true => Right(BaseType.IntType) // Continue if index is of type int
                case false => Left(s"Semantic Error in Expression: Array indices must be compatible with type int but are $t") // Error if index is not of type int
              }
              case Left(error) => Left(error) // Propagate any errors from checkExprType
            }
          }
      } match {
        case Right(_) =>
          env.lookupVariable(name) match {
        case Some(VariableEntry(ArrayType(innerType))) => checkValidArrayIndexing(ArrayType(innerType), indices.length)
          //Right(ArrayType(innerType))
        case Some(VariableEntry(t)) => Left(s"Semantic Error in Expression: $name should be an array, but got $t instead")
        case None => Left(s"Semantic Error in Expression: array $name is not declared")
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
      (checkExprType(left, env, isIfOrWhile), checkExprType(right, env, isIfOrWhile)) match {
        // case (Right(BaseType.IntType), Right(BaseType.IntType)) => Right(BaseType.IntType) 
        case (Right(t1), Right(t2)) =>
          if (isCompatibleTo(BaseType.IntType, t1) && isCompatibleTo(BaseType.IntType, t2)) { 
            Right(BaseType.IntType) 
          } else { 
            Left(s"Semantic Error: $t1 and $t2 are incompatible for arithmetic operation $op") 
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
      (checkExprType(left, env, isIfOrWhile), checkExprType(right, env, isIfOrWhile)) match {
        case (Right(BaseType.IntType), Right(BaseType.IntType)) => Right(BaseType.BoolType) 
        case (Right(BaseType.CharType), Right(BaseType.CharType)) => Right(BaseType.BoolType) 
        case (Right(t1), Right(t2)) => Left(s"Semantic Error: Types should be int or char, but got $t1 and $t2 instead in operation $op") 
        case (Left(error1), Left(error2)) => Left(s"$error1, $error2")
        case (Left(error), _) => Left(error)
        case (_, Left(error)) => Left(error)
      }
    
    case BinaryOp(left, op, right) if Set(
      BinaryOperator.Equal,
      BinaryOperator.NotEqual
    ).contains(op) =>
      (checkExprType(left, env, isIfOrWhile), checkExprType(right, env, isIfOrWhile)) match {
        case (Right(t1), Right(t2)) => 
          if (isCompatibleTo(t1, t2) || isCompatibleTo(t2, t1)) 
            Right(BaseType.BoolType) 
          else Left(s"Semantic Error: Incompatible types $t1, $t2 for equality comparison of operation $op")
        case (Left(error1), Left(error2)) => Left(s"$error1, $error2")
        case (Left(error), _) => Left(error)
        case (_, Left(error)) => Left(error)
      }

    case BinaryOp(left, op, right) if Set(
      BinaryOperator.And,
      BinaryOperator.Or
    ).contains(op) =>
      (checkExprType(left, env, isIfOrWhile), checkExprType(right, env, isIfOrWhile)) match {
        case (Right(BaseType.BoolType), Right(BaseType.BoolType)) => Right(BaseType.BoolType) 
        case (Right(t1), Right(t2)) => Left(s"Semantic Error: $t1 and $t2 are incompatible for logical operation of $op")
        case (Left(error1), Left(error2)) => Left(s"$error1, $error2")
        case (Left(error), _) => Left(error)
        case (_, Left(error)) => Left(error)
      }

    // Unary Operators: !, -, len, ord, chr
    case UnaryOp(UnaryOperator.Not, expr) =>
      checkExprType(expr, env, isIfOrWhile) match {
        case Right(BaseType.BoolType) => Right(BaseType.BoolType) 
        case Right(t) => Left(s"Semantic Error: `!` operator requires a boolean operand but found $t") 
        case Left(error) => Left(error)
      }

    case UnaryOp(UnaryOperator.Negate, expr) =>
      checkExprType(expr, env, isIfOrWhile) match {
        case Right(t) => 
          if (isCompatibleTo(BaseType.IntType, t)) 
            Right(BaseType.IntType)
          else Left(s"Semantic Error: `-` operator requires an integer operand but found $t") 
        case Left(error) => Left(error)
      }

    case UnaryOp(UnaryOperator.Length, expr) =>
      checkExprType(expr, env, isIfOrWhile) match {
        case Right(ArrayType(_)) => Right(BaseType.IntType) 
        case Right(t) => Left(s"Semantic Error: `len` operator requires an array but found $t")
        case Left(error) => Left(error)
      }

    case UnaryOp(UnaryOperator.Ord, expr) =>
      checkExprType(expr, env, isIfOrWhile) match {
        case Right(BaseType.CharType) => Right(BaseType.IntType) 
        case Right(t) => Left(s"Semantic Error: `ord` operator requires a character but found $t") 
        case Left(error) => Left(error)
      }

    case UnaryOp(UnaryOperator.Chr, expr) =>
      checkExprType(expr, env, isIfOrWhile) match {
        case Right(t) => 
          if (isCompatibleTo(BaseType.IntType, t)) 
            Right(BaseType.CharType)
          else Left(s"Semantic Error: `-` operator requires an integer operand but found $t") 
        case Left(error) => Left(error)
      }
  }



  // t1 compatible to t2 = t1 can be weakened to t2
  def isCompatibleTo(t1: Type, t2: Type): Boolean = (t1, t2) match {
    // char[] can be treated as string
    case (ArrayType(BaseType.CharType), BaseType.StrType) => true 
    // string cannot be treated as char[]
    case (BaseType.StrType, ArrayType(BaseType.CharType)) => false // string to char[] is not allowed
    
    // arrays with compatible inner types
    case (ArrayType(innerType1), ArrayType(innerType2)) =>
      nestedCompatibility(innerType1, innerType2) // recursively check if inner types are compatible
    

    case _ => nestedCompatibility(t1, t2)
  }

  def nestedCompatibility(t1: Type, t2: Type): Boolean = (t1, t2) match {
    case (BaseType.IntType, BaseType.IntType) => true
    case (BaseType.BoolType, BaseType.BoolType) => true
    case (BaseType.CharType, BaseType.CharType) => true
    case (BaseType.StrType, BaseType.StrType) => true
    case (ArrayType(innerType1), ArrayType(innerType2)) =>
      nestedCompatibility(innerType1, innerType2)
    
    // pairs should not be covariant
    case (PairType(leftElem1, rightElem1), PairType(leftElem2, rightElem2)) =>
      nestedCompatibility(leftElem1, leftElem2) && nestedCompatibility(rightElem1, rightElem2)
    case(BaseTElem(elem1), BaseTElem(elem2)) => 
      nestedCompatibility(elem1, elem2)
    case(ArrayTElem(elem1), ArrayTElem(elem2)) => 
      nestedCompatibility(elem1, elem2)
    case(PairKeyword, PairKeyword) => true
  
    // any PairElemType can be weakened to a Null Type
    case (NullType, BaseTElem(_)) => true
    case (NullType, ArrayTElem(_)) => true
    case (NullType, PairKeyword) => true
    // any PairElemType can be weakened to a PairKeyword and vice versa
    // this includes nulltype
    case (PairType(_, _), PairKeyword) => true
    case (PairKeyword, PairType(_, _)) => true
    case (PairKeyword, NullType) => true

    case(BaseTElem(elem1), elem2) =>
      nestedCompatibility(elem1, elem2)
    case(elem1, BaseTElem(elem2)) =>
      nestedCompatibility(elem1, elem2)
    case(ArrayTElem(elem1), elem2) =>
      nestedCompatibility(elem1, elem2)
    case(elem1, ArrayTElem(elem2)) =>
      nestedCompatibility(elem1, elem2)
    // any other type can be weaken to AnyType
    case (_, AnyType) => true
    case (AnyType, _) => true
    case _ => false
  }
  def checkValidArrayIndexing(arrayType: Type, numIndices: Int): Either[String, Type] = {
    arrayType match {
      case ArrayType(innerType) =>
        if (numIndices == 1) Right(innerType) 
        else checkValidArrayIndexing(innerType, numIndices - 1) // ✅ Now properly returns Either
      case _ => Left("Semantic Error: Too many indices for array")
    }
  } 
}

// override Option[String] addition method
extension (a: Option[String]) def ++(b: Option[String]): Option[String] = (a, b) match {
  case (Some(a), Some(b)) => Some(s"$a, $b")
  case (Some(a), None) => Some(a)
  case (None, Some(b)) => Some(b)
  case (None, None) => None
}
