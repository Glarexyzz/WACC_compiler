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
    val mainIR = generateMainIR(prog.stmt) // Handles main function
    val funcIRs = prog.funcs.flatMap(generateFunc) // Handles any wacc functions
    val headIR = generateHeadIR()      // Handles .data and .text sections
    val helperIRs = generateHelperIRs() // Handles any _helper functions

    headIR ++ mainIR ++ funcIRs ++ helperIRs
  }

  // Branches for main function
  private var nBranch = 0 // Track number of branching sections
  private val branches = mutable.ListBuffer[IRInstr]() // Store branches as IRFuncLabels
  private var currentBranch = mutable.ListBuffer[IRInstr]() // The working branch
  private def currentBranchLabel(): String = if nBranch == 0 then "main" else s".L${nBranch-1}"
  private def nextBranchLabel(): String = s".L$nBranch"
  private def addBranch(): Unit = {
    branches += IRFuncLabel(IRLabel(currentBranchLabel()), currentBranch.toList)
    currentBranch = mutable.ListBuffer[IRInstr]() // Reset for next branch
    nBranch += 1
  }

  def generateMainIR(stmt: Stmt): List[IRInstr] = {
    val allocatedRegs = initialiseVariables(symbolTable)
    // Generate prologue (Add to first branch)
    val prologue = List(
      IRCmt("Function prologue"),
      pushReg(FP, LR)
    ) ++ pushRegs(allocatedRegs) ++ List(
      IRMovReg(FP, SP)
    )

    currentBranch ++= prologue

    generateStmt(stmt) // Generate IR for main function body

    // Add function epilogue (add to last branch)
    val epilogue = List(
      IRMov(X0, 0), // Default return code
      IRCmt("Function epilogue")
    ) ++ popRegs(allocatedRegs) ++ List(
      popReg(FP, LR),
      IRRet()
    )

    currentBranch ++= epilogue

    addBranch() // Save last branch

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

  def generateStmt(stmt: Stmt): Unit = stmt match {
      case SkipStmt =>

      // All declared variables are initialised at the start from the symbol table
      case DeclAssignStmt(t, name, value) =>
        val (reg, t) = variableRegisters(name)
        generateRValue(value, reg)
      
      case AssignStmt(LValue.LName(name), rvalue) => 
        variableRegisters.get(name) match {
          case Some((reg, _)) =>
            generateRValue(rvalue, reg)
          case None =>
            // should never reach here
            throw new Exception(s"Variable $name used before declaration")
        }
      
      

      case AssignStmt(lvalue, rvalue) => 

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
          case _ => List()
        }

      case FreeStmt(expr) => List()

      case PrintStmt(expr) =>
        val exprType = generateExpr(expr)
        if (exprType == BaseType.IntType) {
          helpers.getOrElseUpdate(IRLabel("_printi"), printi())
          currentBranch +=  IRBl("_printi")
        } else if (exprType == BaseType.CharType) {
          helpers.getOrElseUpdate(IRLabel("_printc"), printc())
          currentBranch +=  IRBl("_printc")
        } else if (exprType == BaseType.StrType) {
          helpers.getOrElseUpdate(IRLabel("_prints"), prints())
          currentBranch +=  IRBl("_prints")
        } else if (exprType == BaseType.BoolType) {
          helpers.getOrElseUpdate(IRLabel("_printb"), printb())
          currentBranch +=  IRBl("_printb")
        } 

      case PrintlnStmt(expr) => 
        helpers.getOrElseUpdate(IRLabel("_println"), printlnFunc())
        generateStmt(PrintStmt(expr))
        currentBranch += IRBl("_println")


      case ReturnStmt(expr) => List()

      case ExitStmt(expr) =>
        generateExpr(expr)
        currentBranch += IRBl("exit")


      case IfStmt(cond, thenStmt, elseStmt) => List()
// main:
// 	// push {fp, lr}
// 	stp fp, lr, [sp, #-16]!
// 	mov fp, sp
// 	mov w8, #1
// 	cmp w8, #1
// 	b.eq .L0
// 	b .L1
// .L0:
// .L1:
// 	mov x0, #0
// 	// pop {fp, lr}
// 	ldp fp, lr, [sp], #16
// 	ret

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

      case SeqStmt(left, right) => generateStmt(left)
                                   generateStmt(right)
  }

  def generateExpr(expr: Expr, destX: Register = X0): Type = 
    val destW = destX.asW
    expr match {
      case IntLiteral(value) =>
        if (value.abs <= 65535) {
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
        stringLiterals.getOrElseUpdate(label, value) // Store string in .data
        currentBranch += IRAdrp(X0, label) += IRAddImm(X0, X0, s":lo12:$label")
        BaseType.StrType

      // move the identifier into the destination register
      case Identifier(name) =>
        val (reg, t) = variableRegisters(name)
        currentBranch += IRMovReg(destW, reg.asW)
        t
      
      case PairLiteral =>
        BaseType.IntType // Assume IntType for simplicity
      
      case UnaryOp(op, expr) => 
        val srcRegX = getRegister()
        val srcRegW = srcRegX.asW
        generateExpr(expr, srcRegX)
        val unaryType = op match {
          case UnaryOperator.Negate =>
            currentBranch += IRNeg(destW, srcRegW) 
            BaseType.IntType 
          case UnaryOperator.Not =>
            currentBranch += IRCmpImm(srcRegW, 1) += IRCset(destW, NE)
            BaseType.BoolType
          case UnaryOperator.Length =>
            currentBranch += IRLdur(destW, srcRegW, -4) 
            BaseType.IntType

          case UnaryOperator.Ord =>
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
        val xreg1 = getRegister() // alternatively, generateExpr produces either a register or a value
        val wreg1 = xreg1.asW // move one to a temporary register
        val xreg2 = getRegister() 
        val wreg2 = xreg2.asW
        generateExpr(expr1, xreg1)  // Generate IR for expr1
        generateExpr(expr2, xreg2)  // Generate IR for expr2
        // 📌 Helpers for comparisons:
        def compareFunc(cond:Condition): Type = {
          val temp = W8 // Use X8 as temporary register
          currentBranch += IRCmp(wreg1, wreg2) += IRCset(temp, cond) += IRMovReg(destW, temp)
          BaseType.BoolType
        }
        val binaryInstrs = op match {
          case BinaryOperator.Add =>
            // for numbers greater than 65537 movk is used to store value in reg
            helpers.getOrElseUpdate(IRLabel("_prints"), prints())
            helpers.getOrElseUpdate(IRLabel("_errOverflow"), errOverflow())
            currentBranch += IRAdds(destW, wreg1, wreg2) += IRJumpCond(VS, "_errOverflow")
            BaseType.IntType // ADD W0, reg1, reg2
          
          case BinaryOperator.Subtract =>
            helpers.getOrElseUpdate(IRLabel("_prints"), prints())
            helpers.getOrElseUpdate(IRLabel("_errOverflow"), errOverflow())
            currentBranch += IRSub(destW, wreg1, wreg2) += IRJumpCond(VS, "_errOverflow")
            BaseType.IntType // SUB W0, reg1, reg2

          case BinaryOperator.Multiply =>
            helpers.getOrElseUpdate(IRLabel("_prints"), prints())
            helpers.getOrElseUpdate(IRLabel("_errOverflow"), errOverflow())
            val xreg = getRegister()
            currentBranch += IRSMull(xreg, wreg1, wreg2) += IRCmpExt(xreg, xreg.asW) += 
                              IRJumpCond(NE, "_errOverflow") += IRMovReg(destW, xreg.asW) // MUL W0, reg1, reg2
            freeRegister(xreg)
            BaseType.IntType


          case BinaryOperator.Divide => 
            helpers.getOrElseUpdate(IRLabel("_prints"), prints())
            helpers.getOrElseUpdate(IRLabel("_errDivZero"), errDivZero())
            currentBranch += IRCmpImm(wreg2, 0) += IRJumpCond(EQ, "_errDivZero") += IRSDiv(destW, wreg1, wreg2)
            BaseType.IntType // SDIV W0, reg1, reg2

          case BinaryOperator.Modulus => 
            helpers.getOrElseUpdate(IRLabel("_prints"), prints())
            helpers.getOrElseUpdate(IRLabel("_errDivZero"), errDivZero())
            currentBranch += IRCmpImm(wreg2, 0) += IRJumpCond(EQ, "_errDivZero") += IRSDiv(W1, wreg1, wreg2) += IRMSub(destW, W1, wreg2, wreg1)
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
            // 
//  mov w8, #1
// 	cmp w8, #1
// 	b.eq .L0
// 	mov w8, #0
// 	cmp w8, #1
// .L0:
// 	cset w8, eq
// 	mov w19, w8
          case BinaryOperator.Or =>
            currentBranch += IRCmpImm(wreg1, 1) += IRJumpCond(EQ, nextBranchLabel()) += IRCmpImm(wreg2, 1)
            addBranch()
            currentBranch += IRCset(destW, EQ)
            BaseType.BoolType // AND W0, reg1, reg2

          case BinaryOperator.And =>
            currentBranch += IRCmpImm(wreg1, 1) += IRJumpCond(NE, nextBranchLabel()) += IRCmpImm(wreg2, 1)
            addBranch()
            currentBranch += IRCset(destW, EQ)
            BaseType.BoolType
        }
        freeRegister(xreg1)
        freeRegister(xreg2)
        binaryInstrs  
      

        
      case _ => BaseType.IntType


        // case PairLiteral => List()
        //   // val reg = getRegister()
        //   // List(IRLoadImmediate(reg, 0))

        
      
        

        // case ArrayElem(name, indices) => List()
        //     // val indexIRs = indices.flatMap(generateExpr)
        //     // indexIRs :+ IRArrayLoad("tmp", name, indexIRs.last.asInstanceOf[IRLoad].dest)
    }


  def generateRValue(rvalue: RValue, reg: Register): Type = {
    rvalue match {
      case RValue.RExpr(expr) => generateExpr(expr, reg)
      // unimplemented
      case RValue.RArrayLiter(arrayLiter) => 
        val elementsIR = arrayLiter.elements.getOrElse(List()) // list of elements
        val size = elementsIR.size // number of elements 
        val arrayMemory = 4 + (size * 4) // memory needed for array
        currentBranch += IRMov(W0, arrayMemory) += IRBl("_malloc") += IRMovReg(X16, X0) 
        += IRAddsImm(X16, X16, 4) += IRMov(W8, size) += IRStur(W8, X16, -4)
        // val registers = 
        for ((element, i) <- elementsIR.zipWithIndex) { // iterate over each expr 
          val expType = generateExpr(element, W8)
          expType match {
            case BaseType.IntType => 
              if (i == 0) { // separate case for first element
                currentBranch += IRStr(W8, X16)
              } else {
              currentBranch += IRStr(W8, X16, Some(i * 4)) // Store element
              }
            case BaseType.CharType => 
              if (i == 0) { // separate case for first element
                currentBranch += IRStrb(W8, X16)
              } else {
              currentBranch += IRStrb(W8, X16, Some(i)) // Store element
              }
            case BaseType.BoolType => 
              if (i == 0) { // separate case for first element
                currentBranch += IRStrb(W8, X16)
              } else {
                currentBranch += IRStrb(W8, X16, Some(i)) // Store element
              }
            case BaseType.StrType => List()
          //     if (i == 0) { // separate case for first element
          //       currentBranch += IRStrb(W8, X16)
          //     } else {
          //       currentBranch += IRStrb(W8, X16, Some(i)) // Store element
          //     }
          // }
          
        }
        currentBranch += IRMovReg(reg, X16) 
        helpers.getOrElseUpdate(IRLabel("_prints"), prints())
        helpers.getOrElseUpdate(IRLabel("_malloc"), malloc())
        helpers.getOrElseUpdate(IRLabel("_errOutOfMemor"), errOutOfMemory())
        BaseType.IntType


      case RValue.RNewPair(left, right) => BaseType.IntType
      case RValue.RPair(pairElem) => BaseType.IntType
      case RValue.RCall(name, args) => BaseType.IntType
    }
  }
  
  // rvalue match {
  //     case RValue.RExpr(expr) => generateExpr(expr)
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
