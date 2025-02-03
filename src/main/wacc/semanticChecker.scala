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
    // No duplicate variable declarations in the same scope
    case DeclAssignStmt(t, name, value) => 
      checkRValue(value) match {
        case Left(error) => Some(error)
        case Right(rType) => 
          if (symbolTable.addVariable(name, rType)) None
          else Some(s"Semantic Error: Variable $name already declared")
      }
      
    // <lValue> '=' <rValue> Check if lvalue is declared and types match
    // lvalue must be an identifier or array-element or pair-element
    // rvalue must be a <expr> | <arrayLiter> | 'newpair' '(' <expr> ',' <expr> ')' | <pairElem> | 'call' <ident> '(' <argList>? ')'
    case AssignStmt(lvalue, rvalue) => 
      (checkLValue(lvalue), checkRValue(rvalue)) match {
        case (Some(error), _) => Some(error)
        case (_, Left(error)) => Some(error)
        case (None, Right(rType)) => lvalue match {
          case LValue.LName(name) => symbolTable.lookup(name) match {
            case Some(VariableEntry(lType, _)) if areTypesCompatible(lType, rType) => None
            case Some(_) => Some(s"Semantic Error: Incompatible types in assignment to $name")
            case None => Some(s"Semantic Error: Variable $name not declared")
          }
          case LValue.LArray(ArrayElem(name, _)) => symbolTable.lookup(name) match {
            case Some(VariableEntry(ArrayType(lType), _)) if areTypesCompatible(lType, rType) => None
            case Some(_) => Some(s"Semantic Error: Incompatible types in assignment to $name")
            case None => Some(s"Semantic Error: Variable $name not declared")
          }
          // but LPair takes in a pair of type PairElem... 
          case LValue.LPair(pair) => checkPairElem(pair) match {
            // rType is also a PairType, check to see if leftPairType and rightPairType are compatible
            case Right(PairType(lLType, lRType)) => 
              rType = PairType(lRType, rRType)
              if (areTypesCompatible(lLType, rLType) && areTypesCompatible(lRType, rRType)) None
              else Some("Semantic Error: Incompatible types in assignment inside pair") 
            case Right(_) => Some("Semantic Error: Incompatible types in assignment to pair")
            case Left(error) => Some(error)
          }
        }
        //case (None, _) => None
      }

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

  def checkLValue(lvalue: LValue): Option[String] = lvalue match {
    case LValue.LName(name) => symbolTable.lookup(name) match {
      case Some(VariableEntry(_, _)) => None
      case Some(_) => Some(s"Error: $name is not a variable")
      case None => Some(s"Error: $name is not declared")
    }
    case LValue.LArrayLiter(name, indices) => symbolTable.lookup(name) match {
      case Some(VariableEntry(ArrayType(_), _)) => None
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
    case RValue.RArrayLiter(arrayLiter) =>
      val exprTypes = arrayLiter.elements.map(checkExprType(_, symbolTable))
      if (exprTypes.forall(_.isRight)) {
        val elementType = exprTypes.head.right.get // Get the first element type (which may be None)
        if (exprTypes.forall(_ == elementType)) Right(ArrayType(elementType))
        else Left("Semantic Error: Array elements must be of the same type")
      }
      else {
        // Error evaluating expression
        Left(exprTypes.collect { case Left(error) => error }.mkString(", "))
      }
    case RValue.RNewPair(lExpr, rExpr) =>
      (checkExprType(lExpr, symbolTable), checkExprType(rExpr, symbolTable)) match {
        case (Right(lType), Right(rType)) => Right(PairType(lType, rType))
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

    case RCall(name, argList) => Left("Error: Function calls not implemented")

  }

  def checkPairElem(pairElem: PairElem): Either[String, Type] = pairElem match {
      case PairElem.FstElem(LName(p)) => symbolTable.lookup(p) match {
        case Some(VariableEntry(PairType(leftType, _), _)) => Right(leftType)
        case Some(_) => Left(s"Error: $p is not a pair")
        case None => Left(s"Error: $p is not declared")
      }
      case PairElem.SndElem(LName(p)) => symbolTable.lookup(p) match {
        case Some(VariableEntry(PairType(_, rightType), _)) => Right(rightType)
        case Some(_) => Left(s"Error: $p is not a pair")
        case None => Left(s"Error: $p is not declared")
      }
      case PairElem.FstElem(LArrayElem(name, _)) | PairElem.SndElem(LArrayElem(name, _)) => symbolTable.lookup(name) match {
        case Some(VariableEntry(ArrayType(leftType), _)) => Right(leftType)
        case Some(_) => Left(s"Error: $name is not an array")
        case None => Left(s"Error: $name is not declared")
      }
      case PairElem.FstElem(LPairElem(name)) | SndElem(LPairElem(name)) => Right(PairKeyword)
      case _ => Left("Error: Invalid pair element")
    }
  

  

  /* To quote the spec:
     Erasure: When either type within a pair are themselves a pair, they MUST have
     an erased type 'pair'. For instance, 'pair(int, pair(int, int))' is not allowed, 
     but pair(int,pair) is legal. pair(T1, T2) can be weakened to pair and vice versa.
     If nested pairs were not subject to erasure, this would involve infinite types.
     Since the type 'pair' is not itself a legal type of a variable, nested pairs
     will cease to be erased once they are extracted; at this point, the full type of the
     pair MUST be known.
     ^ if this makes any sense to any of y'all, since I don't fully get it.
  */
  // pair (int, char) p = newpair(1, 'a');
  // int x = fst p;
  // print x
  // DeclAssignStmt(IntType, x, RPair(FstElem(LName(p))))
  // Assignment is legal when assigning array (even of unknown type) in nested pair extraction
  // probably cos of the pair info loss
  // pair(int, int) p = newpair(1, 2);
  // pair(pair, int) q = newpair(p, 3);
  // fst fst q = []
  // AssignStmt(LPair(FstElem(LPair(FstElem(LName(q))))), RArrayLiter(ArrayLiter(None)))

  // pair(int. int) p = newpair(1, 2);
  // pair(int, int)[] a = [p, p];
  // fst a[0] = 3;
  // int x = fst a[1];
  // AssignStmt(LPair(FstElem(LArrayElem(a, IntLiteral(0)))), RExpr(IntLiteral(3)))
  // DeclAssignStmt(IntType, x, RPair(FstElem(LArrayElem(a, IntLiteral(1))))
  // I think checkPairElem has different behaviours depending on whether it is called in an
  // Assign or Declaration.
  // In a declaration, it would be on the right hand side, as RPair(FstElem(someLValue))
  // In other words, it should already have a value. 
  // So we should check the table to find LName(p), expecting us to find PairType(Type1, Type2)
  // then type of FstElem(LName(p)) should be Type1
  // Although it seems in the definitition, both sides are defined as PairElemType... which works since the rest extend it
  // and if someLValue is LArrayElem(name, IntLiteral(index)), then we should find ArrayType(Type)
  // and if someLValue is LPair(), then type would become 'pair' (erased type) PairKeyword

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

// override Option[String] addition method
extension (a: Option[String]) def ++(b: Option[String]): Option[String] = (a, b) match {
  case (Some(a), Some(b)) => Some(s"$a, $b")
  case (Some(a), None) => Some(a)
  case (None, Some(b)) => Some(b)
  case (None, None) => None
}