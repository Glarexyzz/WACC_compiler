package wacc

import java.io.{File, PrintWriter}
import scala.collection.mutable
import scala.sys.process._

/*
1. Define IR representations for AST
2. Implement code generation for expressions and statements
3. Write an AArch64 assembly backend (to plan in detail more later)
*/
object CodeGen {
  val stringLiterals: mutable.Map[String, String] = mutable.Map() // 🛠 Store unique string labels
  private val availableRegisters = mutable.Stack("x9", "x10", "x11", "x12", "x13") // Register pool
  private def getRegister(): String = availableRegisters.pop() // Allocate
  private def freeRegister(reg: String): Unit = availableRegisters.push(reg) // Free register



  def compile(prog: Program, filepath: String): Unit = {
    println("Compiling...")
    // generating IR
    val ir = generateIR(prog)

    // AArch64 assembly conversion
    val assembly = AArch64Gen.generateAssembly(ir, stringLiterals)

    val asmFile = filepath.replaceAll("\\.wacc$", "") + ".s"
    writeToFile(asmFile, assembly)

    // compile into binary and emulates it
    val outputFile = filepath.replaceAll("\\.wacc$", "")
    val compileCmd = s"aarch64-linux-gnu-gcc -o $outputFile $asmFile"
    if (compileCmd.! == 0) {
      val runCmd = s"qemu-aarch64 ./$outputFile"
      runCmd.!
    } else {
      println(s"❌ Error: Compilation failed for $asmFile")
    }

  }

    // Utility function to write assembly code to file
  def writeToFile(filename: String, content: String): Unit = {
      val writer = new PrintWriter(new File(filename))
      writer.write(content)
      writer.close()
  }

  private def getDestRegister(instrs: List[IRInstr]): String = {
    instrs.collectFirst {
      case IRLoad(dest, _) => dest
      case IRLoadImmediate(dest, _) => dest
      case IRLoadLabel(dest, _) => dest
      case IRBinaryOp(_, dest, _, _) => dest
      case IRUnaryOp(_, dest, _) => dest
      case IRCall(_, args) if args.nonEmpty => args.head // Function return value
    }.getOrElse(throw new RuntimeException("No destination register found in IR instructions"))
  }

  def generateIR(prog: Program): List[IRInstr] = {
      val funcIRs = prog.funcs.flatMap(generateFunc)
      val stmtIRs = generateStmt(prog.stmt)
      funcIRs ++ stmtIRs
  }

  def generateFunc(func: Func): List[IRInstr] = {
      val funcLabel = IRLabel(func.name)
      val paramIRs = func.paramList.getOrElse(List()).map(p => IRAlloc(p.name, 8)) // Assuming 64-bit registers
      val bodyIRs = generateStmt(func.stmt)
      val returnIR = if (!bodyIRs.exists(_.isInstanceOf[IRReturn])) Some(IRReturn(None)) else None
      List(funcLabel) ++ paramIRs ++ bodyIRs ++ returnIR.toList
  }

  def generateStmt(stmt: Stmt): List[IRInstr] = stmt match {
      case SkipStmt => List() 

      case DeclAssignStmt(t, name, value) =>
        val rvalueIR = generateRValue(value)
        val destReg = getDestRegister(rvalueIR)
        freeRegister(destReg)
        rvalueIR :+ IRStore(name, destReg)

      case AssignStmt(lvalue, rvalue) =>
        val rvalueIR = generateRValue(rvalue)
        val destReg = getDestRegister(rvalueIR)
        val storeIR = lvalue match {
          case LValue.LName(name) => List(IRStore(name, destReg))
          case LValue.LArray(arrayElem) =>
            val arrayIR = generateArrayElem(arrayElem)
            arrayIR ++ List(IRArrayStore(arrayElem.name, getDestRegister(arrayIR), destReg))
          case LValue.LPair(pairElem) =>
            val pairIR = generatePairElem(pairElem)
            pairIR ++ List(IRStore(getDestRegister(pairIR), destReg))
        }
        freeRegister(destReg)
        rvalueIR ++ storeIR

      case ReadStmt(lvalue) =>
        val reg = getRegister()
        val readIR = lvalue match {
          case LValue.LName(name) => List(IRRead(name))
          case LValue.LArray(arrayElem) =>
            val arrayIR = generateArrayElem(arrayElem)
            arrayIR :+ IRRead(getDestRegister(arrayIR))
          case LValue.LPair(pairElem) =>
            val pairIR = generatePairElem(pairElem)
            pairIR :+ IRRead(getDestRegister(pairIR))
        }
        freeRegister(reg)
        readIR

      case FreeStmt(expr) => 
        val exprIR = generateExpr(expr)
        val reg = getDestRegister(exprIR)
        freeRegister(reg)
        exprIR :+ IRFree(reg)
      case PrintStmt(expr) => 
        val exprIR = generateExpr(expr)
        val reg = getDestRegister(exprIR)
        freeRegister(reg)
        exprIR :+ IRPrint(reg)
      case PrintlnStmt(expr) => 
        val exprIR = generateExpr(expr)
        val reg = getDestRegister(exprIR)
        freeRegister(reg)
        exprIR :+ IRPrintln(reg)
      case ReturnStmt(expr) => 
        val exprIR = generateExpr(expr)
        val reg = getDestRegister(exprIR)
        freeRegister(reg)
        exprIR :+ IRReturn(Some(reg))
      case ExitStmt(expr) =>
        val exprIR = generateExpr(expr)
        val reg = getDestRegister(exprIR)
        freeRegister(reg)
        exprIR :+ IRReturn(Some(reg))

      case IfStmt(cond, thenStmt, elseStmt) =>
          val condIR = generateExpr(cond)
          val thenIR = generateStmt(thenStmt)
          val elseIR = generateStmt(elseStmt)
          val thenLabel = IRLabel("then_block")
          val elseLabel = IRLabel("else_block")
          val endLabel = IRLabel("end_if")

          condIR ++ List(
              IRJumpCond(cond.toString, thenLabel.name),
              IRJump(elseLabel.name)
          ) ++ List(thenLabel) ++ thenIR ++ List(IRJump(endLabel.name)) ++
          List(elseLabel) ++ elseIR ++ List(endLabel)

      case WhileStmt(cond, body) =>
          val loopLabel = IRLabel("while_loop")
          val bodyIR = generateStmt(body)
          val endLabel = IRLabel("end_while")
          val condIR = generateExpr(cond)

          List(loopLabel) ++ condIR ++ List(
              IRJumpCond(cond.toString, bodyIR.headOption.map(_.toString).getOrElse(endLabel.name)),
              IRJump(endLabel.name)
          ) ++ bodyIR ++ List(IRJump(loopLabel.name), endLabel)

      case BodyStmt(body) => generateStmt(body)

      case SeqStmt(left, right) => generateStmt(left) ++ generateStmt(right)
  }

  def generateExpr(expr: Expr): List[IRInstr] = expr match {
      case IntLiteral(value) => 
        val reg = getRegister()
        List(IRLoadImmediate(reg, value))
      case BoolLiteral(value) => 
        val reg = getRegister()
        List(IRLoadImmediate(reg, if (value) 1 else 0))
      case CharLiteral(value) => 
        val reg = getRegister()
        List(IRLoadImmediate(reg, value.toInt))
      case StrLiteral(value) =>
        val label = s"str_${value.hashCode.abs}"
        stringLiterals.getOrElseUpdate(label, value) // Store string in data section
        val reg = getRegister()
        List(IRLoadLabel(reg, label)) // Load address of string into register
      case Identifier(name) => 
        val reg = getRegister()
        List(IRLoad(reg, name))
      case PairLiteral => 
        val reg = getRegister()
        List(IRLoadImmediate(reg, 0))
      case UnaryOp(op, expr) =>
          val exprIR = generateExpr(expr)
          val destReg = getRegister()
          exprIR :+ IRUnaryOp(op, destReg, getDestRegister(exprIR))
      case BinaryOp(left, op, right) =>
          val leftIR = generateExpr(left)
          val rightIR = generateExpr(right)
          val destReg = getRegister()
          leftIR ++ rightIR :+ IRBinaryOp(op, destReg, getDestRegister(leftIR), getDestRegister(rightIR))
      case ArrayElem(name, indices) =>
          val indexIRs = indices.flatMap(generateExpr)
          indexIRs :+ IRArrayLoad("tmp", name, indexIRs.last.asInstanceOf[IRLoad].dest)
  }

  def generateRValue(rvalue: RValue): List[IRInstr] = rvalue match {
      case RValue.RExpr(expr) => generateExpr(expr)
      case RValue.RArrayLiter(arrayLiter) =>
          val elementsIR = arrayLiter.elements.getOrElse(List()).flatMap(generateExpr)
          val arrayReg = getRegister()
          elementsIR :+ IRAlloc(arrayReg, elementsIR.length * 8)
      case RValue.RNewPair(left, right) =>
        val leftIR = generateExpr(left)
        val rightIR = generateExpr(right)
        val leftReg = getDestRegister(leftIR)
        val rightReg = getDestRegister(rightIR)
        val pairReg = getRegister()
        freeRegister(leftReg)
        freeRegister(rightReg)
        leftIR ++ rightIR :+ IRNewPair(pairReg, leftReg, rightReg)
      case RValue.RPair(pairElem) => generatePairElem(pairElem)
      case RValue.RCall(name, args) =>
        val argIRs = args.getOrElse(List()).map(generateExpr)
        val argRegs = argIRs.map(getDestRegister) // Extract registers
        argRegs.foreach(freeRegister) // Free registers correctly
        argIRs.flatten :+ IRCall(name, argRegs) // Flatten instructions list before appending IRCall
  }

  def generateArrayElem(arrayElem: ArrayElem): List[IRInstr] = {
    val indexIRs = arrayElem.indices.flatMap(generateExpr)
    val indexReg = getDestRegister(indexIRs)
    val arrayReg = getRegister()
    freeRegister(indexReg)
    indexIRs :+ IRArrayLoad(arrayReg, arrayElem.name, indexReg)  
  }

  def generatePairElem(pairElem: PairElem): List[IRInstr] = pairElem match {
      case PairElem.FstElem(value) =>
        val valueIR = generateExpr(value)
        val valueReg = getDestRegister(valueIR)
        val pairReg = getRegister()
        freeRegister(valueReg)
        valueIR :+ IRPairElem(pairReg, valueReg, isFirst = true)
      case PairElem.SndElem(value) =>
        val valueIR = generateExpr(value)
        val valueReg = getDestRegister(valueIR)
        val pairReg = getRegister()
        freeRegister(valueReg)
        valueIR :+ IRPairElem(pairReg, valueReg, isFirst = false)
  }
}
