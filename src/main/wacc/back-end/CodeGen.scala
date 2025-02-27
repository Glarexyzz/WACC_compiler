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
  private val availableRegisters = mutable.Stack[Register](X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20, X21, X22, X23, X24, X25, X26, X27, X28) // Pool of free registers
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

  def freeRegister(reg: Register): Unit = {
    if (registerStack.nonEmpty && registerStack.top == reg) {
      instrBuffer += IRLdr(reg, SP)  // Restore spilled register
      registerStack.pop()
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
    val mainIR = List(IRFuncLabel(IRLabel("main"), generateMainIR(prog.stmt))) // Handles main function
    val funcIRs = prog.funcs.flatMap(generateFunc) // Handles any wacc functions
    val headIR = generateHeadIR()      // Handles .data and .text sections
    val helperIRs = generateHelperIRs() // Handles any _helper functions

    headIR ++ mainIR ++ funcIRs ++ helperIRs
  }


  def generateMainIR(stmt: Stmt): List[IRInstr] = {
    val allocatedRegs = initialiseVariables(symbolTable) // Get allocated registers

    val prologue = 
      List(
        IRCmt("Function prologue"),
        pushReg(FP, LR)) ++
      pushRegs(allocatedRegs) ++
      List(
      IRMovReg(FP, SP)
    )

    val bodyIR = generateStmt(stmt)

    uninitialiseVariables() // Free allocated registers

    val epilogue = List(
      IRMov(X0, 0), // Default return code
      IRCmt("Function epilogue")) ++
      popRegs(allocatedRegs) ++ List( // Pop all allocated registers
      popReg(FP, LR),
      IRRet()
    )

    prologue ++ bodyIR ++ epilogue
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

  def uninitialiseVariables(): List[Register] = {
    val allocatedRegs = mutable.ListBuffer[Register]()
    // Iterate over variables in the variableRegisters and free registers for them
    variableRegisters.foreach { case (varName, (reg, _)) =>
      freeRegister(reg)
      allocatedRegs += reg
      }
    allocatedRegs.toList
  }

  def generateFunc(func: Func): List[IRInstr] = {
    List()
    // left blank for now, initialise later - because this looks really tricky....
    // if anyone has any ideas for this, please go ahead
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

  def generateStmt(stmt: Stmt): List[IRInstr] = stmt match {
      case SkipStmt => List() 

      // All declared variables are initialised at the start from the symbol table
      case DeclAssignStmt(t, name, value) =>
        val (reg, t) = variableRegisters(name)
        val (valueIR, _) = generateRValue(value, reg.asW)
        valueIR
      
      case AssignStmt(LValue.LName(name), rvalue) => 
        variableRegisters.get(name) match {
          case Some((reg, _)) =>
            val (valueIR, _) = generateRValue(rvalue, reg.asW)
            valueIR
          case None =>
            throw new Exception(s"Variable $name used before declaration")
        }
        val (reg, _) = variableRegisters(name)
        val (valueIR, _) = generateRValue(rvalue, reg.asW)
        valueIR
      
      

      case AssignStmt(lvalue, rvalue) => List()
        // val rvalueIR = generateRValue(rvalue)
        // val destReg = getDestRegister(rvalueIR)
        // val storeIR = lvalue match {
        //   case LValue.LName(name) => List(IRStore(name, destReg))
        //   case LValue.LArray(arrayElem) =>
        //     val arrayIR = generateArrayElem(arrayElem)
        //     arrayIR ++ List(IRArrayStore(arrayElem.name, getDestRegister(arrayIR), destReg))
        //   case LValue.LPair(pairElem) =>
        //     val pairIR = generatePairElem(pairElem)
        //     pairIR ++ List(IRStore(getDestRegister(pairIR), destReg))
        // }
        // freeRegister(destReg)
        // rvalueIR ++ storeIR
//// load the current value in the destination of the read so it supports defaults
      case ReadStmt(lvalue) => 
        lvalue match {
          case LValue.LName(name) => 
            val (regX, t) = variableRegisters(name)
            val reg = regX.asW
            t match {
              case BaseType.IntType => 
                helpers.getOrElseUpdate(IRLabel("_readi"), readi())
                List(IRMovReg(W0, reg), IRBl("_readi"), IRMovReg(reg, W0))
              case BaseType.CharType => 
                helpers.getOrElseUpdate(IRLabel("_readc"), readc())
                List(IRMovReg(W0, reg), IRBl("_readc"), IRMovReg(reg, W0))
              case _ => List()
            }
          case _ => List()
        }
        // val reg = getRegister()
        // val readIR = lvalue match {
        //   case LValue.LName(name) => List(IRRead(name))
        //   case LValue.LArray(arrayElem) =>
        //     val arrayIR = generateArrayElem(arrayElem)
        //     arrayIR :+ IRRead(getDestRegister(arrayIR))
        //   case LValue.LPair(pairElem) =>
        //     val pairIR = generatePairElem(pairElem)
        //     pairIR :+ IRRead(getDestRegister(pairIR))
        // }
        // freeRegister(reg)
        // readIR

      case FreeStmt(expr) => List()
        // val exprIR = generateExpr(expr)
        // val reg = getDestRegister(exprIR)
        // freeRegister(reg)
        // exprIR :+ IRFree(reg)

      case PrintStmt(expr) =>
        val (exprIR, exprType) = generateExpr(expr)
        if (exprType == BaseType.IntType) {
          helpers.getOrElseUpdate(IRLabel("_printi"), printi())
          exprIR :+ IRBl("_printi")
        } else if (exprType == BaseType.CharType) {
          helpers.getOrElseUpdate(IRLabel("_printc"), printc())
          exprIR :+ IRBl("_printc")
        } else if (exprType == BaseType.StrType) {
          helpers.getOrElseUpdate(IRLabel("_prints"), prints())
          exprIR :+ IRBl("_prints")
        } else if (exprType == BaseType.BoolType) {
          helpers.getOrElseUpdate(IRLabel("_printb"), printb())
          exprIR :+ IRBl("_printb")
        } else {
          // we have not handled arrays and pairs yet
          exprIR
        }


      case PrintlnStmt(expr) => 
        helpers.getOrElseUpdate(IRLabel("_println"), printlnFunc())
        generateStmt(PrintStmt(expr)) :+ IRBl("_println")


      case ReturnStmt(expr) => List()
        // val exprIR = generateExpr(expr)
        // val reg = getDestRegister(exprIR)
        // freeRegister(reg)
        // exprIR :+ IRReturn(Some(reg))

      // Focus on Exit Stmt first
      // Adds the following lines:
      /*
      	mov w0, #-1
	      // statement primitives do not return results (but will clobber r0/rax)
	      bl exit
    */
      case ExitStmt(expr) =>
        val (exprIR, exprType) = generateExpr(expr)
        exprIR :+ IRBl("exit")

        // val exprIR = generateExpr(expr)
        // val reg = getDestRegister(exprIR)
        // freeRegister(reg)
        // exprIR :+ IRReturn(Some(reg))

      case IfStmt(cond, thenStmt, elseStmt) => List()
          // val condIR = generateExpr(cond)  // Generate IR for condition evaluation
          // val condMov = IRMovReg(W19, W0) // boolean is stored in W0 and then moved in W19?
          // val thenIR = generateStmt(thenStmt)  
          // val elseIR = generateStmt(elseStmt)  

          // val thenLabel = IRLabel("then")
          // val elseLabel = IRLabel("else")
          // val endLabel  = IRLabel("endif")

          // condIR ++
          // List(condMov, IRCmp(W19, WZR), IRJumpCond(EQ, thenLabel.name)) ++
          // elseIR ++
          // List(IRJump(endLabel.name)) ++
          // List(thenLabel) ++
          // thenIR ++
          // List(endLabel)

// would this compile? I'm going to try compiling everything first to test my generateIR
// .... could you help me comment out the stuff that doesn't compile? Especially in the generateExpr part. I'll fix the other issues
          // condIR ++ List(
          //     IRJumpCond(cond.toString, thenLabel.name),
          //     IRJump(elseLabel.name)
          // ) ++ List(thenLabel) ++ thenIR ++ List(IRJump(endLabel.name)) ++
          // List(elseLabel) ++ elseIR ++ List(endLabel)

      case WhileStmt(cond, body) => List()
          // val loopLabel = IRLabel("while_loop")
          // val bodyIR = generateStmt(body)
          // val endLabel = IRLabel("end_while")
          // val condIR = generateExpr(cond)

          // List(loopLabel) ++ condIR ++ List(
          //     IRJumpCond(cond.toString, bodyIR.headOption.map(_.toString).getOrElse(endLabel.name)),
          //     IRJump(endLabel.name)
          // ) ++ bodyIR ++ List(IRJump(loopLabel.name), endLabel)

      case BodyStmt(body) => generateStmt(body)

      case SeqStmt(left, right) => generateStmt(left) ++ generateStmt(right)
  }

  def generateExpr(expr: Expr, dest: Register = W0): (List[IRInstr], Type) = expr match {
    case IntLiteral(value) =>
      if (value.abs <= 65535) {
        (List(IRMov(dest, value.toInt)), BaseType.IntType) 
      } else {
        val lower16 = (value & 0xFFFF).toInt          // Extract lower 16 bits
        val upper16 = ((value >> 16) & 0xFFFF).toInt  // Extract upper 16 bits should be fine to use toInt since should be 16 bits anyway?

        (List(
          IRMov(dest, lower16),               // MOV dest, #lower16
          IRMovk(dest, upper16, 16)   // MOVK dest, #upper16, LSL #16
        ), BaseType.IntType)
      }


    case BoolLiteral(value) =>
      (List(IRMov(dest, if (value) 1 else 0)), BaseType.BoolType)

    case CharLiteral(value) =>
      (List(IRMov(dest, value.toInt)), BaseType.CharType)

    case StrLiteral(value) =>
      val label = nextLabel()
      stringLiterals.getOrElseUpdate(label, value) // Store string in .data
      (List(IRAdrp(X0, label), IRAddImm(X0, X0, s":lo12:$label")), BaseType.StrType)

    // move the identifier into the destination register
    case Identifier(name) =>
      val (reg, t) = variableRegisters(name)
      (List(IRMovReg(dest, reg.asW)), t)
    
    case PairLiteral =>
      (List(), BaseType.IntType) // Assume IntType for simplicity
    
    case UnaryOp(op, expr) => 
      val srcReg = getRegister().asW
      val (exprIR, exprType) = generateExpr(expr, srcReg)
      val instrs = exprIR
      op match {
        case UnaryOperator.Negate =>
          (instrs :+ IRNeg(dest, srcReg), BaseType.IntType) 
        case UnaryOperator.Not =>
          (instrs :+ IRCmpImm(srcReg, 1) :+ IRCset(dest, NE), BaseType.BoolType)
        case UnaryOperator.Length =>
          (instrs :+ IRLdur(dest, srcReg, -4), BaseType.IntType) 


        case UnaryOperator.Ord =>
          (instrs, BaseType.IntType) // Char to Int (no instruction needed)

        case UnaryOperator.Chr =>
          (instrs :+ IRTst(srcReg, 0xffffff80)     // Test if value is within ASCII range (0-127)
                  :+ IRCsel(X1, X0, X1, NE) // Conditional move if out of range
                  :+ IRJumpCond(NE , "_errBadChar") // Branch if invalid
                  :+ IRMovReg(dest, srcReg),           // Move the value into W0 (truncate to char)
            BaseType.CharType)

            
      }

    // DO REGISTERS AS PARAMETER  
    
    case BinaryOp(expr1, op, expr2) =>
      val reg1 = getRegister().asW
      val reg2 = getRegister().asW
      val (instrs1, _) = generateExpr(expr1)  // Generate IR for expr1
      val (instrs2, _) = generateExpr(expr2)  // Generate IR for expr2

      val instrs = instrs1 ++ instrs2  // Combine IR instructions for both expressions
      op match {
        case BinaryOperator.Add =>
          // for numbers greater than 65537 movk is used to store value in reg
          helpers.getOrElseUpdate(IRLabel("_prints"), prints())
          helpers.getOrElseUpdate(IRLabel("_errOverflow"), errOverflow())
          (instrs :+ IRAdds(dest, reg1, reg2) :+ IRJumpCond(VS, "_errOverflow"), BaseType.IntType) // ADD W0, reg1, reg2
        
        case BinaryOperator.Subtract =>
          helpers.getOrElseUpdate(IRLabel("_prints"), prints())
          helpers.getOrElseUpdate(IRLabel("_errOverflow"), errOverflow())
          (instrs :+ IRSub(dest, reg1, reg2) :+ IRJumpCond(VS, "_errOverflow"), BaseType.IntType) // SUB W0, reg1, reg2

        case BinaryOperator.Multiply =>
          helpers.getOrElseUpdate(IRLabel("_prints"), prints())
          helpers.getOrElseUpdate(IRLabel("_errOverflow"), errOverflow())
          (instrs :+ IRSMull(dest, reg1, reg2) :+ IRCmpExt(dest, reg1) :+ IRJumpCond(NE, "_errOverflow"), BaseType.IntType) // MUL W0, reg1, reg2

        case BinaryOperator.Divide => 
          helpers.getOrElseUpdate(IRLabel("_prints"), prints())
          helpers.getOrElseUpdate(IRLabel("_errDivZero"), errDivZero())
          (instrs :+ IRCmpImm(reg2, 0) :+ IRJumpCond(EQ, "_errDivZero") :+ IRSDiv(dest, reg1, reg2), BaseType.IntType) // SDIV W0, reg1, reg2

        case BinaryOperator.Modulus => 
          helpers.getOrElseUpdate(IRLabel("_prints"), prints())
          helpers.getOrElseUpdate(IRLabel("_errDivZero"), errDivZero())
          (instrs :+ IRCmpImm(reg2, 0) :+ IRJumpCond(EQ, "_errDivZero") :+ IRSDiv(W1, reg1, reg2) :+ IRMSub(dest, W1, reg2, reg1), BaseType.IntType)

        case BinaryOperator.Greater =>
          (instrs :+ IRCmp(reg1, reg2) :+ IRCset(dest, GT), BaseType.BoolType)
          // CMP reg1, reg2
          // CSEL W0, 1, 0, GT (if reg1 > reg2, W0 = 1 else W0 = 0)
        
        case BinaryOperator.GreaterEqual =>
          (instrs :+ IRCmp(reg1, reg2) :+ IRCset(dest, GE), BaseType.BoolType)
        case BinaryOperator.Less =>
          (instrs :+ IRCmp(reg1, reg2) :+ IRCset(dest, LT), BaseType.BoolType)

        case BinaryOperator.LessEqual =>
          (instrs :+ IRCmp(reg1, reg2) :+ IRCset(dest, LE), BaseType.BoolType)
          

        case BinaryOperator.Equal =>
          (instrs :+ IRCmp(reg1, reg2) :+ IRCset(dest, EQ), BaseType.BoolType)
          

        case BinaryOperator.NotEqual =>
          (instrs :+ IRCmp(reg1, reg2) :+ IRCset(dest, NE), BaseType.BoolType)
          

        case BinaryOperator.Or =>
          (instrs1 ++ List(IRCmpImm(reg1, 1), IRJumpCond(EQ, ".L0")) ++ instrs2 ++ List(IRCmpImm(reg2, 1), 
            IRCset(dest, EQ)), BaseType.BoolType) // AND W0, reg1, reg2

        case BinaryOperator.And =>
          (instrs1 ++ List(IRCmpImm(reg1, 1), IRJumpCond(NE, ".L0")) ++ instrs2 ++ List(IRCmpImm(reg2, 1), 
            IRCset(dest, EQ)), BaseType.BoolType)

      }         
    

      
    case _ => (List(), BaseType.IntType)


      // case PairLiteral => List()
      //   // val reg = getRegister()
      //   // List(IRLoadImmediate(reg, 0))

      
      //     // val exprIR = generateExpr(expr)
      //     // val destReg = getRegister()
      //     // exprIR :+ IRUnaryOp(op, destReg, getDestRegister(exprIR))

      // case BinaryOp(left, op, right) => List()
      //     // val leftIR = generateExpr(left)
      //     // val rightIR = generateExpr(right)
      //     // val destReg = getRegister()
      //     // leftIR ++ rightIR :+ IRBinaryOp(op, destReg, getDestRegister(leftIR), getDestRegister(rightIR))

      // case ArrayElem(name, indices) => List()
      //     // val indexIRs = indices.flatMap(generateExpr)
      //     // indexIRs :+ IRArrayLoad("tmp", name, indexIRs.last.asInstanceOf[IRLoad].dest)
  }

  def generateRValue(rvalue: RValue, reg: Register): (List[IRInstr], Type) = {
    rvalue match {
      case RValue.RExpr(expr) => generateExpr(expr, reg)
      // unimplemented
      case RValue.RArrayLiter(arrayLiter) => (List(), BaseType.IntType)
      case RValue.RNewPair(left, right) => (List(), BaseType.IntType)
      case RValue.RPair(pairElem) => (List(), BaseType.IntType)
      case RValue.RCall(name, args) => (List(), BaseType.IntType)
    }
  }
  // rvalue match {
  //     case RValue.RExpr(expr) => generateExpr(expr)
  //     case RValue.RArrayLiter(arrayLiter) =>
  //         val elementsIR = arrayLiter.elements.getOrElse(List()).flatMap(generateExpr)
  //         val arrayReg = getRegister()
  //         elementsIR :+ IRAlloc(arrayReg, elementsIR.length * 8)
  //     case RValue.RNewPair(left, right) =>
  //       val leftIR = generateExpr(left)
  //       val rightIR = generateExpr(right)
  //       val leftReg = getDestRegister(leftIR)
  //       val rightReg = getDestRegister(rightIR)
  //       val pairReg = getRegister()
  //       freeRegister(leftReg)
  //       freeRegister(rightReg)
  //       leftIR ++ rightIR :+ IRNewPair(pairReg, leftReg, rightReg)
  //     case RValue.RPair(pairElem) => generatePairElem(pairElem)
  //     case RValue.RCall(name, args) =>
  //       val argIRs = args.getOrElse(List()).map(generateExpr)
  //       val argRegs = argIRs.map(getDestRegister) // Extract registers
  //       argRegs.foreach(freeRegister) // Free registers correctly
  //       argIRs.flatten :+ IRCall(name, argRegs) // Flatten instructions list before appending IRCall
  // }

  def generateArrayElem(arrayElem: ArrayElem): List[IRInstr] = List()
  // {
  //   val indexIRs = arrayElem.indices.flatMap(generateExpr)
  //   val indexReg = getDestRegister(indexIRs)
  //   val arrayReg = getRegister()
  //   freeRegister(indexReg)
  //   indexIRs :+ IRArrayLoad(arrayReg, arrayElem.name, indexReg)  
  // }

  def generatePairElem(pairElem: PairElem): List[IRInstr] = List()
  // pairElem match {
  //     case PairElem.FstElem(value) =>
  //       val valueIR = generateExpr(value)
  //       val valueReg = getDestRegister(valueIR)
  //       val pairReg = getRegister()
  //       freeRegister(valueReg)
  //       valueIR :+ IRPairElem(pairReg, valueReg, isFirst = true)
  //     case PairElem.SndElem(value) =>
  //       val valueIR = generateExpr(value)
  //       val valueReg = getDestRegister(valueIR)
  //       val pairReg = getRegister()
  //       freeRegister(valueReg)
  //       valueIR :+ IRPairElem(pairReg, valueReg, isFirst = false)
  // }
}
