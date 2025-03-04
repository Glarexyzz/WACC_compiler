package wacc

import java.io.{File, PrintWriter}
import scala.collection.mutable
import wacc.Helpers._



/*
1. Define IR representations for AST
2. Implement code generation for expressions and statements
3. Write an AArch64 assembly backend (to plan in detail more later)
*/
object CodeGen {

  private val stringLiterals: mutable.Map[String, String] = mutable.Map() // Store unique string labels
  def nextLabel(): String = s".L.str${stringLiterals.size}"

  private var symbolTable = new SymbolTable()

  // Register allocation
  // We need to account for spill over registers
  private val tempRegisters: List[Register] = List(X8, X9, X10, X11, X12, X13, X14, X15)
  private val availableTempRegisters = mutable.Stack[Register](X8, X9, X10, X11, X12, X13, X14, X15)
  private val activeTempRegisters = mutable.Set[Register]()
  private val availableRegisters = mutable.Stack[Register](X19, X20, X21, X22, X23, X24, X25, X26, X27, X28) // Pool of free registers
  private val activeRegisters = mutable.Set[Register]()  // Set of registers currently in use
  private val registerStack = mutable.Stack[Register]()  // Stack for spilled registers
  private val instrBuffer = mutable.ListBuffer[IRInstr]()
  private val variableRegisters = mutable.Map[String, (Register, Type)]()

  private def getRegister(): Register = {
    if (availableRegisters.nonEmpty) {
      val reg = availableRegisters.pop()
      activeRegisters += reg
      return reg
    }

    // No registers available - Spill an active register
    if (activeRegisters.nonEmpty) {
      val regToSpill = activeRegisters.head // Choose an active register to spill
      instrBuffer += IRStr(regToSpill, SP)  // Spill to stack
      registerStack.push(regToSpill)        // Track it
      activeRegisters -= regToSpill         // Remove from active
      return regToSpill
    }

    throw new Exception("No available registers and no registers to spill!")
  }

    private def getTempRegister(): Register = {
    if (availableTempRegisters.nonEmpty) {
      val reg = availableTempRegisters.pop()
      activeTempRegisters += reg
      return reg
    }
    // If no temp registers available, get a normal one
    getRegister()
  }


  def freeRegister(reg: Register): Unit = {
    if (registerStack.nonEmpty && registerStack.top == reg) {
      instrBuffer += IRLdr(reg, SP)  // Restore spilled register
      registerStack.pop()
    } else if (tempRegisters.contains(reg)) {
      availableTempRegisters.push(reg)
    } else {
      availableRegisters.push(reg)
    }
    activeRegisters -= reg
  } 
    

  // Helper functions generated 
  private val helpers = mutable.Map[IRLabel, List[IRInstr]]()

  // Main function
  def compile(prog: Program, filepath: String, newSymbolTable: SymbolTable): Unit = {
    println("Compiling...")
    // initialise symbol table
    symbolTable = newSymbolTable
    val ir = generateIR(prog)

    // AArch64 assembly conversion
    // val assembly = AArch64Gen.generateAssembly(ir, stringLiterals)
    val assembly = ir.map(_.toString).mkString("\n") + "\n"
    val asmFileName = new File(filepath).getName.replaceAll("\\.wacc$", ".s")
    val asmFile = new File(asmFileName).getAbsolutePath 

    writeToFile(asmFile, assembly)

  }

    // Utility function to write assembly code to file
  def writeToFile(filename: String, content: String): Unit = {
      val writer = new PrintWriter(new File(filename))
      writer.write(content)
      writer.close()
  }

  // need to create generateMainIR, generateHeadIR, generateHelperIRs
  def generateIR(prog: Program): List[IRInstr] = {
    // evaluate main and func first
    val mainIR = generateMainIR(prog.stmt) // Handles main function
    val funcIRs = prog.funcs.flatMap(generateFunc) // Handles any wacc functions
    val headIR = generateHeadIR()      // Handles .data and .text sections
    val helperIRs = generateHelperIRs() // Handles any _helper functions

    headIR ++ mainIR ++ funcIRs ++ helperIRs
  }

  def generateMainIR(stmt: Stmt): List[IRInstr] = generateFunctionIR(stmt)

  def generateFunc(func: Func): List[IRInstr] = {
    generateFunctionIR(func.stmt, Some(func.name), func.paramList)
  }

  // for storing parameters of functions
  private var paramsMap = Map[String, (Register, Type)]()

  def assignFuncParams(params: List[Param]):Map[String, (Register, Type)] = {
    val paramRegisters = List(X0, X1, X2, X3, X4, X5, X6, X7)
    params.zip(paramRegisters).map {
      case (param, reg) =>
        (param.name, (reg, param.t))
    }.toMap
  }
  
  // Branches for main function
  private var nBranch = 0 // Track number of branching sections
  private var branches = mutable.ListBuffer[IRInstr]() // Store branches as IRFuncLabels
  private var currentBranch = mutable.ListBuffer[IRInstr]() // The working branch
  // default is currentBranch
  private def branchLabel(n: Int = 0, funcLabel: Option[String] = None): String = {
    val branchNo = nBranch + n
    funcLabel match {
      case Some(name) => s"wacc_$name"
      case None if branchNo == 0 => "main" 
      case None => s".L${branchNo-1}"
    }
  }

  private def addBranch(funcLabel: Option[String] = None): Unit = {
    branches += IRFuncLabel(IRLabel(branchLabel(funcLabel = funcLabel)), currentBranch.toList)
    currentBranch = mutable.ListBuffer[IRInstr]() // Reset for next branch
    nBranch += 1
  }

  def generateFunctionIR(
    stmt: Stmt, 
    funcLabel: Option[String] = None, 
    paramRegs: Option[List[wacc.Param]] = None
  ): List[IRInstr] = {
    branches = mutable.ListBuffer[IRInstr]() // cleanup the branches

    
    paramRegs match {
      case Some(params) =>
        paramsMap = assignFuncParams(params)
        variableRegisters ++= paramsMap  // add parameters
      case None =>
        None  // unchanged
    }

    val allocatedRegs = initialiseVariables(symbolTable)

    // function: Save parameters onto the stack to allow modification
    val paramPushInstrs = paramsMap.values.map { case (reg, _) => 
      List(
        IRCmt(s"push {$reg}"),
        pushReg(reg, XZR),
        IRMovReg(X16, SP)
      )
    }

    // Generate prologue (Add to first branch)
    val prologue = funcLabel match {
      case Some(_) =>
        List(
          IRCmt("Function prologue"),
          pushReg(FP, LR),
          IRMovReg(FP, SP)
        ) ++ paramPushInstrs.flatten
      case None =>
        List(
          IRCmt("Main/Branch prologue"),
          pushReg(FP, LR)
        ) ++ pushRegs(allocatedRegs) ++ List(
          IRMovReg(FP, SP)
        )
    }

    currentBranch ++= prologue

    generateStmt(stmt) // Generate IR for main function body

    // Add function epilogue (add to last branch)
    val epilogue = funcLabel match {
      case Some(_) =>
        None
      case None =>
        List(
          IRCmt("Main/Branch epilogue"),
          IRMov(X0, 0) // Default return code
        ) ++ popRegs(allocatedRegs) ++ List(
          popReg(FP, LR),
          IRRet()
        )
    }

    currentBranch ++= epilogue

    // ❌ Ensure function label appears first, followed by branch labels inside the function
    addBranch(funcLabel)

    branches.toList
  }

  def initialiseVariables(symTab: SymbolTable): List[Register] = {
    val allocatedRegs = mutable.ListBuffer[Register]()

    // Iterate over variables in the current scope and allocate registers for them
    symTab.getVariableScopes.headOption.foreach { currentScope =>
      currentScope.foreach { case (varName,  VariableEntry(t)) =>
        val reg = getRegister()
        variableRegisters(varName) = (reg, t)  // map variable names to allocated registers and type
        allocatedRegs += reg              // track allocated register
      }
    }
    allocatedRegs.toList
  }

  def generateHeadIR(): List[IRInstr] = {
    val dataSection = stringLiterals.map { case (label, value) =>
      wordLabel(value.length, label, value)
    }.flatten.toList

    List(IRLabel(".data")) ++ dataSection ++ List(IRAlign(4), IRLabel(".text"), IRGlobal("main"))
  }

  def generateHelperIRs(): List[IRInstr] = {
    helpers.values.toList.flatten
  }

  def generateStmt(stmt: Stmt): Unit = stmt match {
      case SkipStmt =>

      // All declared variables are initialised at the start from the symbol table
      case DeclAssignStmt(t, name, value) =>
        val (reg, t) = variableRegisters(name)
        generateRValue(value, reg, Some(t))
            
      case AssignStmt(lvalue, rvalue) => 
        lvalue match {
          case LValue.LName(name) => 
            variableRegisters.get(name) match {
              case Some((reg, t)) =>
                generateRValue(rvalue, reg, Some(t))
                // function parameter update push to stack
                if (paramsMap.contains(name)) {
                  currentBranch ++= List(
                    IRCmt(s"push {$reg}"),
                    pushReg(reg, XZR),
                    IRMovReg(X16, SP)
                  )
                }
              case None =>
                // should never reach here
                throw new Exception(s"Variable $name used before declaration")
            }
          case LValue.LArray(ArrayElem(name, indices)) =>
            val (baseReg, arrType) = variableRegisters(name) // Base address
            generateExpr(indices.head, W17) // Get index value
            val varReg = getTempRegister()
            val elemType = variableRegisters(name)._2
            generateRValue(rvalue, varReg, Some(elemType))
            currentBranch += IRMovReg(X7, baseReg)
            if (elemType == BaseType.CharType) {
              currentBranch += IRBl("_arrStore1")
              helpers.getOrElseUpdate(IRLabel("_arrStore1"), arrStore1(varReg.asW))
            } else {
              currentBranch += IRBl("_arrStore4")
              helpers.getOrElseUpdate(IRLabel("_arrStore4"), arrStore4(varReg.asW))
              
            }
            freeRegister(varReg)
            helpers.getOrElseUpdate(IRLabel("_errOutOfBounds"), errOutOfBounds())

          case LValue.LPair(pairElem) =>
            val temp = getTempRegister()
            generateRValue(rvalue, temp, Some(PairType(NullType, NullType))) 
            generateLPair(pairElem, temp, true)
            freeRegister(temp)
        }

      case ReadStmt(lvalue) => 
        lvalue match {
          case LValue.LName(name) => 
            val (regX, t) = variableRegisters(name)
            val reg = regX.asW
            t match {
              case BaseType.IntType => 
                helpers.getOrElseUpdate(IRLabel("_readi"), readi())
                currentBranch ++= List(IRMovReg(W0, reg), IRBl("_readi"), IRMovReg(reg, W0))
              case BaseType.CharType => 
                helpers.getOrElseUpdate(IRLabel("_readc"), readc())
                currentBranch ++= List(IRMovReg(W0, reg), IRBl("_readc"), IRMovReg(reg, W0))
              case _ => List()
            }

          case LValue.LPair(PairElem.FstElem(LValue.LName(name))) =>
            val (reg, t) = variableRegisters(name)
            nullErrorCheck(reg)
            currentBranch += IRLdr(X0, reg)
            checkPairType(t, true) match {
              case BaseType.IntType =>
                helpers.getOrElseUpdate(IRLabel("_readi"), readi())
                currentBranch += IRBl("_readi")
              case BaseType.CharType =>
                helpers.getOrElseUpdate(IRLabel("_readc"), readc())
                currentBranch += IRBl("_readi")
              case _ => List()
            }
            currentBranch ++= List(
              IRMovReg(W16, W0),
              IRStr(W16, reg)
            )
          case LValue.LPair(PairElem.SndElem(LValue.LName(name))) =>
            val (reg, t) = variableRegisters(name)
            nullErrorCheck(reg)
            currentBranch += IRLdr(X0, reg, Some(8))
            checkPairType(t, false) match {
              case BaseType.IntType =>
                helpers.getOrElseUpdate(IRLabel("_readi"), readi())
                currentBranch += IRBl("_readi")
              case BaseType.CharType =>
                helpers.getOrElseUpdate(IRLabel("_readc"), readc())
                currentBranch += IRBl("_readi")
              case _ => List()
            }
            currentBranch ++= List(
              IRMovReg(W16, W0),
              IRStr(W16, reg, Some(8))
            )
            
          case _ => List()
        }

      case FreeStmt(expr) => 
        expr match {
          case (Identifier(name)) =>
            val (reg, t) = variableRegisters(name)
            t match {
              case ArrayType(_) => currentBranch += IRSubImm(X0, reg, 4) += IRBl("free")
              case PairType(_, _)=> 
                currentBranch += IRMovReg(X0, reg) += IRBl("_freepair")
                helpers.getOrElseUpdate(IRLabel("_freepair"), freepair())
                helpers.getOrElseUpdate(IRLabel("_errNull"), errNull())
                helpers.getOrElseUpdate(IRLabel("_prints"), prints())
              case _ => throw new Exception(s"Unsupported type for freeing: $t")
            
            }
        }

      case PrintStmt(expr) =>
        expr match {
          case Identifier(name) if paramsMap.contains(name) =>
            val (reg, t) = paramsMap(name)
            // function parameter push
            currentBranch ++= List(
              IRCmt(s"pop/peek {$reg}"),
              IRLdur(reg, SP, 0),
              IRMovReg(X16, SP)
            )
          case _ =>
            None
        }
        val exprType = generateExpr(expr)
        
        if (exprType == BaseType.IntType) {
          helpers.getOrElseUpdate(IRLabel("_printi"), printi())
          currentBranch +=  IRBl("_printi")
        } else if (exprType == BaseType.CharType) {
          helpers.getOrElseUpdate(IRLabel("_printc"), printc())
          currentBranch +=  IRBl("_printc")
        } else if (exprType == BaseType.StrType || exprType == ArrayType(BaseType.CharType)) {
          helpers.getOrElseUpdate(IRLabel("_prints"), prints())
          currentBranch +=  IRBl("_prints")
        } else if (exprType == BaseType.BoolType) {
          helpers.getOrElseUpdate(IRLabel("_printb"), printb())
          currentBranch +=  IRBl("_printb")
        } else if (exprType == ArrayType(BaseType.IntType)) {
          helpers.getOrElseUpdate(IRLabel("_printp"), printp())
          currentBranch +=  IRBl("_printp")
        } else if (exprType.isInstanceOf[PairType]) {
          helpers.getOrElseUpdate(IRLabel("_printp"), printp())
          currentBranch +=  IRBl("_printp")
        }

      case PrintlnStmt(expr) => 
        helpers.getOrElseUpdate(IRLabel("_println"), printlnFunc())
        generateStmt(PrintStmt(expr))
        currentBranch += IRBl("_println")

        expr match {
          case Identifier(name) if paramsMap.contains(name) =>
            val (reg, t) = paramsMap(name)
            // function parameter pop
            currentBranch ++= List(
              IRCmt(s"pop {$reg}"),
              popReg(reg, XZR)
            )
          case _ =>
            None
        }


      case ReturnStmt(expr) => 
        // function: Restore parameters from the stack before returning
        val paramPopInstrs = paramsMap.values.map { case (reg, _) =>
          List(
            IRCmt(s"pop {$reg}"),
            popReg(reg, XZR)
          )
        }
        generateExpr(expr, W0)
        currentBranch ++= (
          List(
            IRCmt("Function epilogue: reset stack pointer")
          ) ++ paramPopInstrs.flatten ++ List(
            IRMovReg(SP, FP), // Reset stack pointer before returning
            popReg(FP, LR),
            IRRet()
          )
        )

      case ExitStmt(expr) =>
        generateExpr(expr)
        currentBranch += IRBl("exit")


      case IfStmt(cond, thenStmt, elseStmt) =>
        val temp = getTempRegister()
        generateExpr(cond, temp) // load result in temp register
        currentBranch += IRCmpImm(temp, 1) += IRJumpCond(EQ, branchLabel(1)) // if true, jump to next branch
        freeRegister(temp)
        generateStmt(elseStmt) // else, continue
        currentBranch += IRJump(branchLabel(2)) 
        addBranch()
        generateStmt(thenStmt)
        addBranch()

      case WhileStmt(cond, body) =>
        val bodyBranch = branchLabel(1)
        val condBranch = branchLabel(2)
        currentBranch += IRJump(condBranch) // jump to condition check
        addBranch()
        generateStmt(body)
        addBranch()
        val temp = getTempRegister()
        generateExpr(cond, temp) // if condition true, jump to body
        currentBranch += IRCmpImm(temp, 1) += IRJumpCond(EQ, bodyBranch)
        freeRegister(temp)

      case BodyStmt(body) => generateStmt(body)

      case SeqStmt(left, right) => generateStmt(left)
                                   generateStmt(right)
  }

  def genOverflow() = 
    helpers.getOrElseUpdate(IRLabel("_prints"), prints())
    helpers.getOrElseUpdate(IRLabel("_errOverflow"), errOverflow())

  def genDivZero() =
    helpers.getOrElseUpdate(IRLabel("_prints"), prints())
    helpers.getOrElseUpdate(IRLabel("_errDivZero"), errDivZero())

  def extractInt(expr1: Expr, expr2: Expr, isAdd: Boolean): Option[(Expr, Int)] = 
    def valid(value: Int): Boolean = (value >= -4096 && value <= 4095 && isAdd) || (value >= 0 && value <= 4095 && !isAdd)

    (expr1, expr2) match {
      case (_, IntLiteral(value)) if valid(value.toInt) => Some((expr1, value.toInt))
      case (IntLiteral(value), _) if (valid(value.toInt) && isAdd) => Some((expr2, value.toInt))
      case _ => None
    }

  def generateExpr(expr: Expr, dest: Register = X0, temp: Register = X0): Type = 
    val destX = dest.asX
    val destW = dest.asW
    expr match {
      case IntLiteral(value) =>
        if (value.abs <= 65535 || value <= -2147483647) {
          currentBranch += IRMov(destW, value.toInt)
          BaseType.IntType
        } else {
          val lower16 = (value & 0xFFFF).toInt          // Extract lower 16 bits
          val upper16 = ((value >> 16) & 0xFFFF).toInt  // Extract upper 16 bits should be fine to use toInt since should be 16 bits anyway?
          currentBranch +=
            IRMov(destW, lower16) +=        // MOV dest, #lower16
            IRMovk(destW, upper16, 16)   // MOVK dest, #upper16, LSL #16
          BaseType.IntType
        }

      case BoolLiteral(value) =>
        currentBranch += IRMov(destW, if (value) 1 else 0)
        BaseType.BoolType

      case CharLiteral(value) =>
        currentBranch += IRMov(destW, value.toInt)
        BaseType.CharType

      case StrLiteral(value) =>
        val label = nextLabel()
        val formatVal = escapeInnerQuotes(value)
        stringLiterals.getOrElseUpdate(label, formatVal) // Store string in .data
        currentBranch += IRAdrp(destX, label) += IRAddImm(destX, destX, s":lo12:$label")
        BaseType.StrType

      // move the identifier into the destination register
      case Identifier(name) =>
        val (reg, t) = variableRegisters(name)
        // compare if the dest and src are the same value or not to reduce redundancy
        if (destW != reg.asW) {
          t match {
            //case ArrayType(BaseType.CharType) => currentBranch += IRStr(reg, X16)
            case ArrayType(_) => currentBranch += IRMovReg(destX, reg.asX)
            case BaseType.StrType => currentBranch += IRMovReg(destX, reg.asX)
            case _ => currentBranch += IRMovReg(destW, reg.asW)
          }  
        }
        t

      case PairLiteral =>
        currentBranch += IRMov(destX, 0) // 0 is the null value
        PairType(NullType, NullType)
      
      case UnaryOp(op, expr) => 
        val srcRegX = getTempRegister()
        val srcRegW = srcRegX.asW
        generateExpr(expr, srcRegX)
        val unaryType = op match {
          case UnaryOperator.Negate =>
            currentBranch += IRNeg(destW, srcRegW) += IRJumpCond(VS, "_errOverflow")
            genOverflow()
            BaseType.IntType 
          case UnaryOperator.Not =>
            currentBranch += IRCmpImm(srcRegW, 1) += IRCset(destW, NE)
            BaseType.BoolType
          case UnaryOperator.Length =>
            expr match {
                case Identifier(name) =>
                  val varSrcRegW = variableRegisters(name)._1
                  currentBranch += IRLdur(destW, varSrcRegW, -4)
                case _ =>
            }
            BaseType.IntType

            

          case UnaryOperator.Ord => 
            expr match {
              case Identifier(name) => 
                val r = variableRegisters(name)._1
                currentBranch += IRMovReg(destW, r.asW)
            }
            BaseType.IntType

          case UnaryOperator.Chr =>
            helpers.getOrElseUpdate(IRLabel("_errBadChar"), errBadChar())
            currentBranch += IRTst(srcRegW, 0xffffff80)     // Test if value is within ASCII range (0-127)
                    += IRCsel(X1, X0, X1, NE) // Conditional move if out of range
                    += IRJumpCond(NE , "_errBadChar") // Branch if invalid
                    += IRMovReg(destW, srcRegW)          // Move the value into W0 (truncate to char)
            BaseType.CharType
              
        }
        freeRegister(srcRegX)
        unaryType

      // DO REGISTERS AS PARAMETER  
      
      case BinaryOp(expr1, op, expr2) =>
        if (op == BinaryOperator.Add || op == BinaryOperator.Subtract || op == BinaryOperator.Multiply) then {
          genOverflow()
        } else {
          genDivZero()
        }
        // 📌 Helpers for comparisons:
        def compareFunc(cond:Condition): Type = {
          val (wreg1, wreg2) = genExprs(expr1, expr2, false)
          val temp = W8 // Use X8 as temporary register
          currentBranch += IRCmp(wreg1, wreg2) += IRCset(temp, cond) += IRMovReg(destW, temp)
          freeRegister(wreg1.asX)
          freeRegister(wreg2.asX)
          BaseType.BoolType
        }
        // Helper to generate both sides of a binary operator
        def genExprs(expr1: Expr, expr2: Expr, useTemp: Boolean): (Register, Register) = {
          val xreg1 = if (useTemp) then getTempRegister() else getRegister()
          val wreg1 = xreg1.asW        
          generateExpr(expr1, xreg1)
          val xreg2 = if (useTemp) then getTempRegister() else getRegister()
          val wreg2 = xreg2.asW 
          generateExpr(expr2, xreg2)
          (wreg1, wreg2)
        }
        val binaryInstrs = op match {
          case BinaryOperator.Add => 
            extractInt(expr1, expr2, true) match {
              case Some((expr, value)) =>
                generateExpr(expr, dest)
                currentBranch += IRAddsImm(destW, destW, value) += IRJumpCond(VS, "_errOverflow")
              case _ =>
                val (wreg1, wreg2) = genExprs(expr1, expr2, true)
                currentBranch += IRAdds(destW, wreg1, wreg2) += IRJumpCond(VS, "_errOverflow")
                freeRegister(wreg1.asX)
                freeRegister(wreg2.asX)
            }
            BaseType.IntType
          
          case BinaryOperator.Subtract =>
            extractInt(expr1, expr2, false) match {
              case Some((expr, value)) => 
                generateExpr(expr, dest)
                currentBranch += IRSubImm(destW, destW, value) += IRJumpCond(VS, "_errOverflow")
              case _ =>
                val (wreg1, wreg2) = genExprs(expr1, expr2, true)
                currentBranch += IRSub(destW, wreg1, wreg2) += IRJumpCond(VS, "_errOverflow")
                freeRegister(wreg1.asX)
                freeRegister(wreg2.asX)
            }
            BaseType.IntType

          case BinaryOperator.Multiply =>
            val (wreg1, wreg2) = genExprs(expr1, expr2, true)
            currentBranch += IRSMull(destX, wreg1, wreg2) += IRCmpExt(destX, destW) += IRJumpCond(NE, "_errOverflow")
            freeRegister(wreg1.asX)
            freeRegister(wreg2.asX)
            BaseType.IntType


          case BinaryOperator.Divide => 
            val (wreg1, wreg2) = genExprs(expr1, expr2, true)
            currentBranch += IRCmpImm(wreg2, 0) += IRJumpCond(EQ, "_errDivZero") += IRSDiv(destW, wreg1, wreg2)
            freeRegister(wreg1.asX)
            freeRegister(wreg2.asX)
            BaseType.IntType

          case BinaryOperator.Modulus => 
            val (wreg1, wreg2) = genExprs(expr1, expr2, true)
            currentBranch += IRCmpImm(wreg2, 0) += IRJumpCond(EQ, "_errDivZero") += IRSDiv(W1, wreg1, wreg2) += IRMSub(destW, W1, wreg2, wreg1)
            freeRegister(wreg1.asX)
            freeRegister(wreg2.asX)
            BaseType.IntType

          case BinaryOperator.Greater =>
            compareFunc(GT)
          case BinaryOperator.GreaterEqual =>
            compareFunc(GE)
          case BinaryOperator.Less =>
            compareFunc(LT)
          case BinaryOperator.LessEqual =>
            compareFunc(LE)
          case BinaryOperator.Equal =>
            compareFunc(EQ)
          case BinaryOperator.NotEqual =>
            compareFunc(NE)

          case BinaryOperator.Or =>
            val (wreg1, wreg2) = genExprs(expr1, expr2, false)
            currentBranch += IRCmpImm(wreg1, 1) += IRCset(wreg1, EQ) += IRCmpImm(wreg2, 1) += IRCset(wreg2, EQ) += IROr(destW, wreg1, wreg2)
            freeRegister(wreg1.asX)
            freeRegister(wreg2.asX)
            BaseType.BoolType
          
          case BinaryOperator.And =>
            val (wreg1, wreg2) = genExprs(expr1, expr2, false)
            currentBranch += IRCmpImm(wreg1, 1) += IRJumpCond(NE, branchLabel(1)) += IRCmpImm(wreg2, 1)
            addBranch()
            currentBranch += IRCset(destW, EQ)
            freeRegister(wreg1.asX)
            freeRegister(wreg2.asX)
            BaseType.BoolType
        }
        binaryInstrs  

      case ArrayElem(name, indices) => 
      
        val (baseReg, arrType) = variableRegisters(name) // Base address
        generateExpr(indices.head, W17) // Get index value
        arrType match {
          case ArrayType(ArrayType(_)) => 
            helpers.getOrElseUpdate(IRLabel("_arrLoad8"), arrLoad8())
            helpers.getOrElseUpdate(IRLabel("_errOutOfBounds"), errOutOfBounds())
            currentBranch += IRMovReg(X7, baseReg) 
            currentBranch += IRBl("_arrLoad8") += IRMovReg(X8, X7) += IRLdur(W0, X8, -4)
          case _ =>
            helpers.getOrElseUpdate(IRLabel("_arrLoad4"), arrLoad4())
            helpers.getOrElseUpdate(IRLabel("_errOutOfBounds"), errOutOfBounds())
            currentBranch += IRMovReg(X7, baseReg) 
            currentBranch += IRBl("_arrLoad4") += IRMovReg(destW, W7)
          
        
        }
        
        arrType match {
          case ArrayType(inner) => inner
          case _ => throw new Exception()
        }
        

      case _ => BaseType.IntType
        
    }
  
  def generateLValue (name: String, lvalue: LValue, reg: Register): Type =
    lvalue match {
        case LValue.LName(name) => 
        case _ =>
    } 
    BaseType.IntType

  def generateLPair(pair: PairElem, dest: Register, isStr: Boolean, isFirst: Boolean = true): Type =
    pair match {
      case PairElem.FstElem(LValue.LName(p)) =>
        val (reg, t) = variableRegisters(p)
        nullErrorCheck(reg)
        if (isFirst && isStr) then {
          currentBranch += IRStr(dest, reg)
        } else {
          currentBranch += IRLdr(dest, reg)
        }
        t
      case PairElem.SndElem(LValue.LName(p)) => 
        val (reg, t) = variableRegisters(p)
        nullErrorCheck(reg)
        if (isFirst && isStr) then {
          currentBranch += IRStr(dest, reg, Some(8))
        } else {
          currentBranch += IRLdr(dest, reg, Some(8))
        }
        t
      case PairElem.FstElem(LValue.LPair(innerPair)) => 
        val temp = getTempRegister()
        val t = generateLPair(innerPair, temp, isStr, false)
        nullErrorCheck(temp)
        if (isFirst && isStr) then {
          currentBranch += IRStr(dest, temp)
        } else {
          currentBranch += IRLdr(dest, temp)
        }
        freeRegister(temp)
        t
      case PairElem.SndElem(LValue.LPair(innerPair)) => 
        val temp = getTempRegister()
        val t = generateLPair(innerPair, temp, isStr, false)
        nullErrorCheck(temp)
        if (isFirst && isStr) then {
          currentBranch += IRStr(dest, temp, Some(8))
        } else {
          currentBranch += IRLdr(dest, temp, Some(8))
        }
        freeRegister(temp)
        t
      case _ => BaseType.IntType // Nothing yet - add arrays later
    }
  

  def generateRValue(rvalue: RValue, reg: Register, exprType: Option[Type] = None): Unit = {
    rvalue match {
      case RValue.RExpr(expr) => generateExpr(expr, reg)
      // unimplemented
      case RValue.RArrayLiter(arrayLiter) => 
        val elementsIR = arrayLiter.elements.getOrElse(List()) // list of elements
        val size = elementsIR.size // number of elements 
        val arrayMemory = arrayMemorySize(size, exprType.get)

        currentBranch += IRMov(W0, arrayMemory) += IRBl("_malloc") += IRMovReg(X16, X0) 
        += IRAddImmInt(X16, X16, 4) += IRMov(W8, size) += IRStur(W8, X16, -4)
        // val registers = 
        for ((element, i) <- elementsIR.zipWithIndex) { // iterate over each expr 
          val elType = generateExpr(element, W8)
          elType match {
            case BaseType.IntType => 
              if (i == 0) { // separate case for first element
                currentBranch += IRStr(W8, X16)
              } else {
              currentBranch += IRStr(W8, X16, Some(i * 4)) // Store element
              }
            case BaseType.CharType => 
              //if (expType == BaseType.StrType)
              if (i == 0) { // separate case for first element
                currentBranch += IRStrb(W8, X16)
              } else {
              currentBranch += IRStrb(W8, X16, Some(i)) // Should be strb
              }
            case BaseType.BoolType => 
              if (i == 0) { // separate case for first element
                currentBranch += IRStrb(W8, X16)
              } else {
                currentBranch += IRStrb(W8, X16, Some(i)) // Store element
              }
            case BaseType.StrType => 
              if (i == 0) { // separate case for first element
                currentBranch += IRStrb(W8, X16)
              } else {
                currentBranch += IRStrb(W8, X16, Some(i)) // should be str but if char[] then should be strb
              }
            case PairType(_,_) =>
              currentBranch.remove(currentBranch.length - 1)
              element match {
                case Identifier(name) =>
                  val elemReg = variableRegisters(name)._1
                  if (i == 0) { // separate case for first element
                    currentBranch += IRStr(elemReg, X16)
                  } else {
                    currentBranch += IRStr(elemReg, X16, Some(i * 8)) // should be str but if char[] then should be strb
                  }
              }
            case ArrayType(inner) =>
              currentBranch.remove(currentBranch.length - 1)
              element match {
                case Identifier(name) =>
                  val elemReg = variableRegisters(name)._1
                  if (i == 0) { // separate case for first element
                    currentBranch += IRStr(elemReg, X16)
                  } else {
                    currentBranch += IRStr(elemReg, X16, Some(i * 8)) // should be str but if char[] then should be strb
                  }
              }
              
            case _ =>
          }
        
          
        }
        currentBranch += IRMovReg(reg, X16) 
        helpers.getOrElseUpdate(IRLabel("_prints"), prints())
        helpers.getOrElseUpdate(IRLabel("_malloc"), malloc())
        helpers.getOrElseUpdate(IRLabel("_errOutOfMemory"), errOutOfMemory())
        

      case RValue.RNewPair(fst, snd) => 
        def addPairElem(expr: Expr, offset: Option[Int]) = {
          expr match {
            case Identifier(name) => 
              val (reg, t) = variableRegisters(name)
              currentBranch += IRStr(reg, X16, offset)
            case _ => 
              val temp = getTempRegister()
              generateExpr(expr, temp)
              currentBranch += IRStr(temp, X16, offset)
              freeRegister(temp)
          }
        }
        val pairMemorySize = 16 // 8 bytes for each element

        currentBranch += IRMov(W0, pairMemorySize) += IRBl("_malloc") += IRMovReg(X16, X0)
        addPairElem(fst, None)
        addPairElem(snd, Some(8))
        currentBranch += IRMovReg(reg, X16) // move pair memory to destination register

        helpers.getOrElseUpdate(IRLabel("_prints"), prints())
        helpers.getOrElseUpdate(IRLabel("_malloc"), malloc())
        helpers.getOrElseUpdate(IRLabel("_errOutOfMemory"), errOutOfMemory())

      case RValue.RPair(pairElem) => 
        generateLPair(pairElem, reg, false)

      case RValue.RCall(name, Some(args)) => 
        val paramRegs = List(X0, X1, X2, X3, X4, X5, X6, X7)
        args.zip(paramRegs).foreach { case (arg, reg) =>
          generateExpr(arg, reg) // Move argument values into x0-x7
        }
        currentBranch ++= List(IRBl(s"wacc_$name"), IRMovReg(reg.asW, W0))

      case RValue.RCall(name, None) =>
        currentBranch ++= List(IRBl(s"wacc_$name"), IRMovReg(reg.asW, W0))    }

    
  }

  def elementSize(expType: Type): Int = {
    expType match {
      case BaseType.IntType => 4 // Integers are 4 bytes
      case BaseType.CharType => 1 // Chars are 1 byte
      case BaseType.BoolType => 1 // Bools are 1 byte
      case BaseType.StrType => 8 // Strings are pointers (8 bytes)
      case ArrayType(t) => elementSize(t)
      case BaseTElem(t) => elementSize(t)
      case ArrayTElem(t) => elementSize(t) // does the pair contain an array or an element of the array?
      case PairType(_,_) => 16
      case _ => throw new IllegalArgumentException(s"Unsupported element type: $expType")
    }
  }

  def arrayMemorySize(size: Int, expType: Type): Int = {
    expType match {
      case PairType(_, _) => 4 + size * 8 // treat pairs as pointer so 8 bytes
      case ArrayType(t)  => 4 + (size * elementSize(t))  // Integers are 4 bytes
      case BaseType.StrType  => 4 + size 
      case _ => throw new IllegalArgumentException(s"Unsupported array type: $expType")
    }
}

  def checkPairType(pairType: Type, isFst: Boolean): Type = {
    def toBaseType(t: Type): Type = t match {
      case BaseTElem(t) => t
      case ArrayTElem(t) => t
      case _ => t
    }
    pairType match {
      case PairType(fst, snd) => if (isFst) then toBaseType(fst) else toBaseType(snd)
      case _ => pairType
    }
  }



  def generateArrayElem(arrayElem: ArrayElem): List[IRInstr] = List()
  

  def generatePairElem(pairElem: PairElem): List[IRInstr] = List()
  
  def escapeInnerQuotes(str: String): String = {
    if (str.startsWith("\"") && str.endsWith("\"")) {
      "\"" + str.drop(1).dropRight(1).replace("\"", "\\\"") + "\""
    } else {
      str.replace("\"", "\\\"") // Just escape quotes if no surrounding quotes
    }
  }

  def nullErrorCheck(reg: Register): Unit = {
    helpers.getOrElseUpdate(IRLabel("_prints"), prints())
    helpers.getOrElseUpdate(IRLabel("_errNull"), errNull())
    currentBranch += IRCmpImm(reg, 0) += IRJumpCond(EQ, "_errNull")
  }

}
