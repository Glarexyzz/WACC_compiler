package wacc
/*
1. Define IR representations for AST
2. Implement code generation for expressions and statements
3. Write an AArch64 assembly backend (to plan in detail more later)
*/
object CodeGen {
  def compile(prog: Any, filepath: String): Unit = {
    println("Compiling...")
  }

  def generateIR(stmt: Stmt): List[IRInstr] = stmt match {
    case DeclAssignStmt(varType, name, RValue.RExpr(expr)) =>
      generateIR(expr) ++ List(IRStore(name, "R0"))  // Store result in variable
    
    case AssignStmt(LValue.LName(name), RValue.RExpr(expr)) =>
      generateIR(expr) ++ List(IRStore(name, "R0"))  // Assign to variable
    
    case PrintStmt(expr) =>
      generateIR(expr) ++ List(IRPrint("R0"))  // Print result
    
    case IfStmt(cond, thenStmt, elseStmt) =>
      val thenIR = generateIR(thenStmt)
      val elseIR = generateIR(elseStmt)
      val endLabel = freshLabel("end_if")

      generateIR(cond) ++
        List(IRJumpCond("EQ", endLabel)) ++
        thenIR ++
        List(IRJump(endLabel)) ++
        elseIR ++
        List(IRLabel(endLabel))
      
    case SeqStmt(left, right) =>
      generateIR(left) ++ generateIR(right)

    case _ => throw new Exception(s"Unsupported statement: $stmt")
  }

  def generateIR(expr: Expr): List[IRInstr] = expr match {
    case IntLiteral(value) => List(IRStore("R0", value.toString))  
    case BinaryOp(left, op, right) =>
      val leftIR = generateIR(left)
      val rightIR = generateIR(right)
      leftIR ++ rightIR ++ List(IRBinaryOp(opToIR(op), "R0", "R1", "R2"))
    case Identifier(name) => List(IRLoad("R0", name))
    case _ => throw new Exception(s"Unsupported expression: $expr")
  }

  def opToIR(op: BinaryOperator): String = op match {
    case BinaryOperator.Add => "ADD"
    case BinaryOperator.Subtract => "SUB"
    case BinaryOperator.Multiply => "MUL"
    case BinaryOperator.Divide => "DIV"
    case BinaryOperator.Modulus => "MOD"
    case BinaryOperator.Equal => "CMP"
    case BinaryOperator.NotEqual => "CMP"
    case _ => throw new Exception("Unsupported operator!")
  }

  var labelCounter = 0
  def freshLabel(base: String): String = {
    labelCounter += 1
    s"${base}_$labelCounter"
  }
}
