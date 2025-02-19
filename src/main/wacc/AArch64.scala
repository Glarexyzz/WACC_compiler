package wacc

object AArch64Gen {
  
  def generateAssembly(irList: List[IRInstr]): String = {
    val asm = new StringBuilder
    asm.append(".global _start\n")
    asm.append(".section .text\n")

    asm.append("_start:\n")

    irList.foreach { instr =>
      asm.append(translateInstr(instr) + "\n")
    }

    asm.append("  mov x8, #93  // syscall: exit\n")
    asm.append("  mov x0, #0   // return code 0\n")
    asm.append("  svc #0       // invoke syscall\n")

    asm.toString()
  }

  private def translateInstr(instr: IRInstr): String = instr match {
    // ✅ Memory Operations
    case IRLoad(dest, src) => s"  ldr $dest, [$src]"
    case IRStore(dest, src) => s"  str $src, [$dest]"
    case IRAlloc(name, size) =>
      s"  sub sp, sp, #$size\n  mov $name, sp"

    // ✅ Arithmetic Operations
    case IRBinaryOp(BinaryOperator.Add, dest, left, right) => s"  add $dest, $left, $right"
    case IRBinaryOp(BinaryOperator.Subtract, dest, left, right) => s"  sub $dest, $left, $right"
    case IRBinaryOp(BinaryOperator.Multiply, dest, left, right) => s"  mul $dest, $left, $right"
    case IRBinaryOp(BinaryOperator.Divide, dest, left, right) => s"  sdiv $dest, $left, $right"
    case IRBinaryOp(BinaryOperator.Modulus, dest, left, right) =>
      s"  sdiv x9, $left, $right\n  msub $dest, x9, $right, $left"

    // ✅ Unary Operations
    case IRUnaryOp(UnaryOperator.Not, dest, src) => s"  mvn $dest, $src"
    case IRUnaryOp(UnaryOperator.Negate, dest, src) => s"  neg $dest, $src"

    // ✅ Function Calls & Returns
    case IRCall(func, args) =>
      val argSetup = args.zipWithIndex.map { case (arg, i) => s"  mov x${i}, $arg" }.mkString("\n")
      s"$argSetup\n  bl $func"

    case IRReturn(value) => s"  mov x0, $value\n  ret"

    // ✅ Control Flow
    case IRJump(label) => s"  b $label"
    case IRJumpCond(cond, label) => s"  cmp x0, x1\n  b.ne $label"
    case IRLabel(name) => s"$name:"

    // ✅ Input & Output
    case IRPrint(value) => s"  mov x0, $value\n  bl print_string"
    case IRPrintln(value) => s"  mov x0, $value\n  bl print_string\n  bl print_newline"
    case IRRead(dest) => s"  mov x0, $dest\n  bl read_integer"

    // ✅ Pairs & Arrays
    case IRNewPair(dest, left, right) =>
      s"  mov x0, #16\n  bl malloc\n  str $left, [x0]\n  str $right, [x0, #8]\n  mov $dest, x0"

    case IRPairElem(dest, src, isFirst) =>
      val offset = if (isFirst) "0" else "8"
      s"  ldr $dest, [$src, #$offset]"

    case IRArrayLoad(dest, name, index) =>
      s"  ldr $dest, [$name, $index, LSL #3]"

    case IRArrayStore(name, index, value) =>
      s"  str $value, [$name, $index, LSL #3]"

    case IRFree(dest) =>
      s"  mov x0, $dest\n  bl free"
  }
}
