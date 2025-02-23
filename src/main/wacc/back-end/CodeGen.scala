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

  // Register allocation
  // We need to account for spill over registers
  private val availableRegisters = mutable.Stack[Register](X9, X10, X11, X12, X13) // Pool of free registers
  private val activeRegisters = mutable.Set[Register]()  // Set of registers currently in use
  private val registerStack = mutable.Stack[Register]()  // Stack for spilled registers
  private val instrBuffer = mutable.ListBuffer[IRInstr]()

  private def getRegister(): Register = {
    if (availableRegisters.nonEmpty) {
      val reg = availableRegisters.pop()
      activeRegisters += reg
      reg
    }

    // No registers available - Spill an active register
    if (activeRegisters.nonEmpty) {
      val regToSpill = activeRegisters.head // Choose an active register to spill
      instrBuffer += IRStr(regToSpill, SP)  // Spill to stack
      registerStack.push(regToSpill)        // Track it
      activeRegisters -= regToSpill         // Remove from active
      regToSpill
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
  private val helpers = mutable.Map[IRLabel, IRFuncLabel]()

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
      List(
        IRCmt(s"// length of $label"),
        IRWord(value.length + 1), // Store string length (+1 for null terminator)
        IRFuncLabel(
          IRLabel(label),
          List(IRAsciz(value))
        )
      )
    }.flatten.toList

    List(IRLabel(".data")) ++ dataSection ++ List(IRAlign(4), IRLabel(".text"), IRGlobal("main"))
  }

  def generateHelperIRs(): List[IRInstr] = {
    helpers.values.toList
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

      case PrintStmt(expr) => List()
        // behaviour differs depending on type expr
        // we have various print helper function (prints for string, printi for int)

        // val exprIR = generateExpr(expr)
        // val reg = getDestRegister(exprIR)
        // freeRegister(reg)
        // exprIR :+ IRPrint(reg)

      case PrintlnStmt(expr) => List()
        // really annoying.
        // logic works similarly to PrintStmt
        // but we have an added helper function _println that helps to print a line
        // helper functions can be found (soon) in helpers.scala

        // val exprIR = generateExpr(expr)
        // val reg = getDestRegister(exprIR)
        // freeRegister(reg)
        // exprIR :+ IRPrintln(reg)

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
      // val label = s"str_${value.hashCode.abs}"
      // stringLiterals.getOrElseUpdate(label, value) // Store string in .data
      // val reg = getRegister()
      (List(), BaseType.StrType)

    // incomplete
    case Identifier(name) =>
      (List(), BaseType.IntType) // Assume IntType for simplicity
    
    case PairLiteral =>
      (List(), BaseType.IntType) // Assume IntType for simplicity
    
    case UnaryOp(op, expr) => 
      val (exprIR, exprType) = generateExpr(expr)
      val instrs = exprIR
      op match {
      case UnaryOperator.Negate =>
        (instrs :+ IRNeg(W0, W0), BaseType.IntType) // Arithmetic negation (NEG W0, W0)

      case UnaryOperator.Not =>
        case UnaryOperator.Not =>
        (instrs :+ IRCmp(W0, 1) :+ IRCset("w0", Condition.NE), BaseType.BoolType)


      case UnaryOperator.Length =>
        (instrs :+ IRLdr(W0, W0), BaseType.IntType) // Load array length

      case UnaryOperator.Ord =>
        (instrs, BaseType.IntType) // Char to Int (no instruction needed)

      case UnaryOperator.Chr =>
        (instrs, BaseType.CharType) // Int to Char (no instruction needed)

      case _ =>
        throw new RuntimeException(s"Unsupported unary operator: $op")
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
