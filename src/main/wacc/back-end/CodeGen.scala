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
  // Global Variables stored in .data
  // val globalVariables: mutable.Map[Label, String] = mutable.Map() // this doesn't seem accurate, so I'll comment it out first
  private val stringLiterals: mutable.Map[String, String] = mutable.Map() // Store unique string labels

  def nextLabel(): String = s".L.str${stringLiterals.size}"

  // Register allocation
  // We need to account for spill over registers
  // private val availableRegisters = mutable.Stack[Register](X9, X10, X11, X12, X13) // Register pool
  // private def getRegister(): Register = {
  //   if (availableRegisters.nonEmpty) {
  //     return availableRegisters.pop()
  //   }
    
  //   // No registers available → Spill an active register
  //   // val regToSpill = activeRegisters.head // Choose a register to spill (simplified strategy)
  //   // registerStack.push(regToSpill)        // Save spilled register
  //   // activeRegisters -= regToSpill          // Remove from active set
    
  //   // instrBuffer += IRStr(regToSpill, SP)  // Spill register to stack
  //   // regToSpill
  // }
  // private def freeRegister(reg: Register): Unit = {
  //   if (registerStack.nonEmpty && registerStack.top == reg) {
  //     instrBuffer += IRLdr(reg, SP)  // ✅ Restore spilled register
  //     registerStack.pop()             // Remove from spill stack
  //   } else {
  //     availableRegisters.push(reg)
  //   }
  // }
  // Helper functions generated 
  private val helpers = mutable.Map[IRLabel, List[IRInstr]]()

  def compile(prog: Program, filepath: String): Unit = {
    println("Compiling...")
    // generating IR
    val ir = generateIR(prog)

    // AArch64 assembly conversion
    // val assembly = AArch64Gen.generateAssembly(ir, stringLiterals)
    val assembly = ir.map(_.toString).mkString("\n")
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

  // private def getDestRegister(instrs: List[IRInstr]): Option[Register] = {
  //   instrs.collectFirst {
  //     case IRMov(dest, _) => dest
  //     case IRMvn(dest, _) => dest
  //     case IRAdr(dest, _) => dest
  //     case IRLdr(dest, _) => dest
  //     case IRAdd(dest, _, _) => dest
  //     case IRSub(dest, _, _) => dest
  //     case IRMul(dest, _, _) => dest
  //     case IRDiv(dest, _, _) => dest
  //     case IRAnd(dest, _, _) => dest
  //     case IROr(dest, _, _) => dest
  //     case IRXor(dest, _, _) => dest
  //     case IRNeg(dest, _) => dest
  //     case IRCset(dest, _) => dest
  //   }
  // }

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
    val prologue = List(
      IRCmt("Function prologue"),
      pushReg(FP, LR),
      IRMovReg(FP, SP)
    )

    val bodyIR = generateStmt(stmt)

    val epilogue = List(
      IRMov(X0, 0), // Default return code
      IRCmt("Function epilogue"),
      popReg(FP, LR),
      IRRet()
    )

    prologue ++ bodyIR ++ epilogue
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

      case DeclAssignStmt(t, name, value) => List()
        // val rvalueIR = generateRValue(value)
        // val destReg = getDestRegister(rvalueIR)
        // freeRegister(destReg)
        // rvalueIR :+ IRStore(name, destReg)

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

      case ReadStmt(lvalue) => List()
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
        helpers.getOrElseUpdate(IRLabel("println"), printlnFunc())
        generateStmt(PrintStmt(expr)) :+ IRBl("println")


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
          // val condIR = generateExpr(cond)
          // val condReg = getDestRegister(condIR)
          // val thenIR = generateStmt(thenStmt)
          // val elseIR = generateStmt(elseStmt)
          // val thenLabel = IRLabel("then_block")
          // val elseLabel = IRLabel("else_block")
          // val endLabel = IRLabel("end_if")
          
          // val condJump = IRCmp(condReg, 0) // Compare condition register to 0 (false)
          // val branchIfTrue = IRJumpCond("ne", thenLabel.name) // Jump to then block if condition is not equal to 0 (true)
          // val jumpToElse = IRJump(elseLabel.name) // Jump to else block
  
  
          // val thenIR = generateStmt(thenStmt)
          // val thenJump = IRJump(endLabel.name) // Jump to end after the 'then' block
          // condIR ++ 
          // List(condJump, branchIfTrue, jumpToElse) ++ 
          // List(thenLabel) ++ thenIR :+ thenJump ++ 
          // List(elseLabel) ++ elseIR :+ IRLabel(endLabel.name)
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

  def generateExpr(expr: Expr): (List[IRInstr], Type) = expr match {
    case IntLiteral(value) =>
      (List(IRMov(W0, value.toInt)), BaseType.IntType)

    case BoolLiteral(value) =>
      (List(IRMov(W0, if (value) 1 else 0)), BaseType.BoolType)

    case CharLiteral(value) =>
      (List(IRMov(W0, value.toInt)), BaseType.CharType)

    // not complete
    // 	adrp x0, .L.str0
	// add x0, x0, :lo12:.L.str0
    case StrLiteral(value) =>
      val label = nextLabel()
      stringLiterals.getOrElseUpdate(label, value) // Store string in .data
      (List(IRAdrp(X0, label), IRAddImm(X0, X0, s":lo12:$label")), BaseType.StrType)
      // val reg = getRegister()

    // incomplete
    case Identifier(name) =>
      (List(), BaseType.IntType) // Assume IntType for simplicity
    
    case PairLiteral =>
      (List(), BaseType.IntType) // Assume IntType for simplicity
    
    case _ => (List(), BaseType.IntType)

      // case PairLiteral => List()
      //   // val reg = getRegister()
      //   // List(IRLoadImmediate(reg, 0))

      // case UnaryOp(op, expr) => List()
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

  def generateRValue(rvalue: RValue): List[IRInstr] = List()
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
