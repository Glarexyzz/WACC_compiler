package wacc

import java.io.{File, PrintWriter}
import scala.sys.process._
import AArch64Gen._
/*
1. Define IR representations for AST
2. Implement code generation for expressions and statements
3. Write an AArch64 assembly backend (to plan in detail more later)
*/
object CodeGen {
  def compile(prog: Program, filepath: String): Unit = {
    println("Compiling...")
    // generating IR
    val ir = generateIR(prog)

    // AArch64 assembly conversion
    val assembly = AArch64Gen.generateAssembly(ir)

    val asmFile = filepath.replace(".wacc", ".s")
    writeToFile(asmFile, assembly)

    // compile into binary and emulates it
    val outputFile = filepath.replace(".wacc", "")
    val compileCmd = s"aarch64-linux-gnu-gcc -o $outputFile $asmFile"
    if (compileCmd.! == 0) {
      val runCmd = s"qemu-aarch64 ./$outputFile"
      runCmd.!
    } else {
      compileCmd.!
    }

  }

    // Utility function to write assembly code to file
  def writeToFile(filename: String, content: String): Unit = {
      val writer = new PrintWriter(new File(filename))
      writer.write(content)
      writer.close()
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
          rvalueIR :+ IRStore(name, rvalueIR.last.asInstanceOf[IRLoad].dest)

      case AssignStmt(lvalue, rvalue) =>
          val rvalueIR = generateRValue(rvalue)
          lvalue match {
              case LValue.LName(name) => rvalueIR :+ IRStore(name, rvalueIR.last.asInstanceOf[IRLoad].dest)
              case LValue.LArray(arrayElem) =>
                  val arrayIR = generateArrayElem(arrayElem)
                  rvalueIR ++ arrayIR :+ IRArrayStore(arrayElem.name, arrayIR.last.asInstanceOf[IRLoad].dest, rvalueIR.last.asInstanceOf[IRLoad].dest)
              case LValue.LPair(pairElem) =>
                  val pairIR = generatePairElem(pairElem)
                  rvalueIR ++ pairIR :+ IRStore(pairIR.last.asInstanceOf[IRLoad].dest, rvalueIR.last.asInstanceOf[IRLoad].dest)
          }

      case ReadStmt(lvalue) =>
          lvalue match {
              case LValue.LName(name) => List(IRRead(name))
              case LValue.LArray(arrayElem) =>
                  val arrayIR = generateArrayElem(arrayElem)
                  arrayIR :+ IRRead(arrayIR.last.asInstanceOf[IRLoad].dest)
              case LValue.LPair(pairElem) =>
                  val pairIR = generatePairElem(pairElem)
                  pairIR :+ IRRead(pairIR.last.asInstanceOf[IRLoad].dest)
          }

      case FreeStmt(expr) => generateExpr(expr) :+ IRFree(expr.toString)
      case PrintStmt(expr) => generateExpr(expr) :+ IRPrint(expr.toString)
      case PrintlnStmt(expr) => generateExpr(expr) :+ IRPrintln(expr.toString)
      case ReturnStmt(expr) => generateExpr(expr) :+ IRReturn(Some(expr.toString))
      case ExitStmt(expr) => generateExpr(expr) :+ IRReturn(Some(expr.toString))

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
      case IntLiteral(value) => List(IRLoadImmediate("tmp", value))
      case BoolLiteral(value) => List(IRLoadImmediate("tmp", if (value) 1 else 0))
      case CharLiteral(value) => List(IRLoadImmediate("tmp", value.toInt))
      case StrLiteral(value) => List(IRLoadImmediate("tmp", value))
      case Identifier(name) => List(IRLoad("tmp", name))
      case PairLiteral => List(IRLoadImmediate("tmp", 0))
      case UnaryOp(op, expr) =>
          val exprIR = generateExpr(expr)
          exprIR :+ IRUnaryOp(op, "tmp", exprIR.last.asInstanceOf[IRLoad].dest)
      case BinaryOp(left, op, right) =>
          val leftIR = generateExpr(left)
          val rightIR = generateExpr(right)
          leftIR ++ rightIR :+ IRBinaryOp(op, "tmp", leftIR.last.asInstanceOf[IRLoad].dest, rightIR.last.asInstanceOf[IRLoad].dest)
      case ArrayElem(name, indices) =>
          val indexIRs = indices.flatMap(generateExpr)
          indexIRs :+ IRArrayLoad("tmp", name, indexIRs.last.asInstanceOf[IRLoad].dest)
  }

  def generateRValue(rvalue: RValue): List[IRInstr] = rvalue match {
      case RValue.RExpr(expr) => generateExpr(expr)
      case RValue.RArrayLiter(arrayLiter) =>
          val elementsIR = arrayLiter.elements.getOrElse(List()).flatMap(generateExpr)
          elementsIR :+ IRAlloc("array", elementsIR.length * 8)
      case RValue.RNewPair(left, right) =>
          val leftIR = generateExpr(left)
          val rightIR = generateExpr(right)
          leftIR ++ rightIR :+ IRNewPair("tmp", leftIR.last.asInstanceOf[IRLoad].dest, rightIR.last.asInstanceOf[IRLoad].dest)
      case RValue.RPair(pairElem) => generatePairElem(pairElem)
      case RValue.RCall(name, args) =>
          val argIRs = args.getOrElse(List()).flatMap(generateExpr)
          argIRs :+ IRCall(name, argIRs.map(_.toString))
  }

  def generateArrayElem(arrayElem: ArrayElem): List[IRInstr] = {
      val indexIRs = arrayElem.indices.flatMap(generateExpr)
      indexIRs :+ IRArrayLoad("tmp", arrayElem.name, indexIRs.last.asInstanceOf[IRLoad].dest)
  }

  def generatePairElem(pairElem: PairElem): List[IRInstr] = pairElem match {
      case PairElem.FstElem(value) =>
          val valueIR = generateExpr(value)
          valueIR :+ IRPairElem("tmp", valueIR.last.asInstanceOf[IRLoad].dest, isFirst = true)
      case PairElem.SndElem(value) =>
          val valueIR = generateExpr(value)
          valueIR :+ IRPairElem("tmp", valueIR.last.asInstanceOf[IRLoad].dest, isFirst = false)
  }
}
