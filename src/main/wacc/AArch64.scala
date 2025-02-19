package wacc

import scala.collection.mutable

object AArch64Gen {
  
  def generateAssembly(irList: List[IRInstr], stringLiterals: mutable.Map[String, String]): String = {
    val dataSection = new StringBuilder(".data\n")
    val textSection = new StringBuilder(".text\n.global _start\n_start:\n")

    // Handle string literals stored in stringLiterals
    stringLiterals.foreach { case (label, value) =>
      dataSection.append(s"$label: .asciz \"$value\"\n")
    }

    // Generate instructions
    irList.foreach { instr =>
      textSection.append(translateInstr(instr) + "\n")
    }

    // Append exit syscall at the end of the program
    textSection.append("  mov x8, #93  // syscall: exit\n")
    textSection.append("  mov x0, #0   // return code 0\n")
    textSection.append("  svc #0       // invoke syscall\n")

    dataSection.toString() + "\n" + textSection.toString()
  }

  private def translateInstr(instr: IRInstr): String = instr match {
    // ✅ Memory Operations
    case IRLoad(dest, src) => s"  ldr $dest, [$src]"
    case IRStore(dest, src) => s"  str $src, [$dest]"
    case IRLoadImmediate(dest, value) => s"  mov $dest, #$value"
    case IRLoadLabel(dest, label) => s"  adr $dest, $label"
    case IRAlloc(name, size) => s"  sub sp, sp, #$size\n  mov $name, sp"

    // ✅ Arithmetic Operations
    case IRBinaryOp(BinaryOperator.Add, dest, left, right) => s"  add $dest, $left, $right"
    case IRBinaryOp(BinaryOperator.Subtract, dest, left, right) => s"  sub $dest, $left, $right"
    case IRBinaryOp(BinaryOperator.Multiply, dest, left, right) => s"  mul $dest, $left, $right"
    case IRBinaryOp(BinaryOperator.Divide, dest, left, right) => s"  sdiv $dest, $left, $right"
    case IRBinaryOp(BinaryOperator.Modulus, dest, left, right) =>
      s"  sdiv x9, $left, $right\n  msub $dest, x9, $right, $left"
    case IRBinaryOp(BinaryOperator.Greater, dest, left, right) =>
      s"  cmp $left, $right\n  cset $dest, gt"  // Set dest = (left > right) ? 1 : 0
    case IRBinaryOp(BinaryOperator.GreaterEqual, dest, left, right) =>
      s"  cmp $left, $right\n  cset $dest, ge"  // Set dest = (left >= right) ? 1 : 0
    case IRBinaryOp(BinaryOperator.Less, dest, left, right) =>
      s"  cmp $left, $right\n  cset $dest, lt"  // Set dest = (left < right) ? 1 : 0
    case IRBinaryOp(BinaryOperator.LessEqual, dest, left, right) =>
      s"  cmp $left, $right\n  cset $dest, le"  // Set dest = (left <= right) ? 1 : 0
    case IRBinaryOp(BinaryOperator.Equal, dest, left, right) =>
      s"  cmp $left, $right\n  cset $dest, eq"  // Set dest = (left == right) ? 1 : 0
    case IRBinaryOp(BinaryOperator.NotEqual, dest, left, right) =>
      s"  cmp $left, $right\n  cset $dest, ne"  // Set dest = (left != right) ? 1 : 0
    case IRBinaryOp(BinaryOperator.And, dest, left, right) =>
      s"  and $dest, $left, $right"  // Bitwise AND (same as logical AND for boolean values)
    case IRBinaryOp(BinaryOperator.Or, dest, left, right) =>
      s"  orr $dest, $left, $right"  // Bitwise OR (same as logical OR for boolean values)

    // ✅ Unary Operations
    case IRUnaryOp(UnaryOperator.Not, dest, src) => s"  mvn $dest, $src"
    case IRUnaryOp(UnaryOperator.Negate, dest, src) => s"  neg $dest, $src"
    case IRUnaryOp(UnaryOperator.Length, dest, src) => s"  ldr $dest, [$src]"
    case IRUnaryOp(UnaryOperator.Ord, dest, src) =>
      s"  mov $dest, $src"  // ASCII value of character is already stored in register
    case IRUnaryOp(UnaryOperator.Chr, dest, src) =>
      s"  mov $dest, $src"  // Converting back to character is just treating it as a char

    // ✅ Function Calls & Returns
    case IRCall(func, args) =>
      val regArgs = args.take(8).zipWithIndex.map { case (arg, i) => s"  mov x${i}, $arg" }
      val stackArgs = args.drop(8).reverse.map(arg => s"  str $arg, [sp, #-16]!")
      (stackArgs ++ regArgs :+ s"  bl $func").mkString("\n")

    case IRReturn(Some(value)) => s"  mov x0, $value\n  ret"
    case IRReturn(None) => "  mov x0, #0\n  ret"  // Fix: Ensure void return sets x0 to 0

    // ✅ Control Flow
    case IRJump(label) => s"  b $label"
    case IRJumpCond(cond, label) => s"  cmp x0, #0\n  b.$cond $label"
    case IRLabel(name) => s"$name:"

    // ✅ Input & Output
    case IRPrint(value) =>
      if (value.startsWith("str_")) s"  adr x0, $value\n  bl print_string"
      else s"  mov x0, $value\n  bl print_integer"

    case IRPrintln(value) =>
      if (value.startsWith("str_")) s"  adr x0, $value\n  bl print_string\n  bl print_newline"
      else s"  mov x0, $value\n  bl print_integer\n  bl print_newline"

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
