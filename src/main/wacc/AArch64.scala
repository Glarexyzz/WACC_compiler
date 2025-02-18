package wacc

object AArch64Gen {
  
  def generateAssembly(irList: List[IRInstr]): String = {
    val asm = irList.flatMap(instrToAsm).mkString("\n")
    s"""
    .global main
    .text
    main:
    $asm
    mov x8, #93      // Exit syscall
    svc 0            // Call kernel
    """
  }

  def instrToAsm(instr: IRInstr): List[String] = instr match {
    case IRLoad(dest, src) =>
      List(s"LDR $dest, [$src]")  // Load from memory

    case IRStore(dest, src) =>
      List(s"STR $src, [$dest]")  // Store to memory

    case IRBinaryOp("ADD", dest, left, right) =>
      List(s"ADD $dest, $left, $right")  // Addition
    
    case IRBinaryOp("SUB", dest, left, right) =>
      List(s"SUB $dest, $left, $right")  // Subtraction

    case IRBinaryOp("MUL", dest, left, right) =>
      List(s"MUL $dest, $left, $right")  // Multiplication

    case IRBinaryOp("DIV", dest, left, right) =>
      List(
        s"SDIV $dest, $left, $right"  // Division
      )

    case IRBinaryOp("MOD", dest, left, right) =>
      List(
        s"SDIV X9, $left, $right", // X9 = left / right
        s"MSUB $dest, X9, $right, $left" // dest = left - (X9 * right)
      )

    case IRPrint(value) =>
      List(
        s"MOV X0, $value",
        "BL printf"
      )

    case IRRead(dest) =>
      List(
        s"BL scanf",
        s"STR X0, [$dest]"
      )

    case IRJump(label) =>
      List(s"B $label")  // Unconditional jump

    case IRJumpCond(cond, label) =>
      List(
        s"CMP X0, #0",
        s"B$cond $label"
      )  // Conditional jump

    case IRLabel(name) =>
      List(s"$name:")

    case IRCall(func, args) =>
      args.zipWithIndex.map { case (arg, i) => s"MOV X$i, $arg" } ++
        List(s"BL $func")

    case IRReturn(value) =>
      List(
        s"MOV X0, $value",
        "RET"
      )

    case _ => throw new Exception(s"Unknown IR instruction: $instr")
  }
}
