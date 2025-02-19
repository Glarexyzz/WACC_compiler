package wacc

// Intermediate Representation
sealed trait IRInstr

// 📌 Load & Store Instructions
case class IRLoad(dest: String, src: String) extends IRInstr  // Load from memory
case class IRStore(dest: String, src: String) extends IRInstr // Store to memory
case class IRLoadImmediate(dest: String, value: BigInt) extends IRInstr // Load a constant value

// 📌 Arithmetic & Boolean Operations (Typed for Safety)
case class IRBinaryOp(op: BinaryOperator, dest: String, left: String, right: String) extends IRInstr
case class IRUnaryOp(op: UnaryOperator, dest: String, src: String) extends IRInstr

// 📌 Function Calls & Returns
case class IRCall(func: String, args: List[String]) extends IRInstr // Function call
case class IRReturn(value: Option[String]) extends IRInstr // Return value (or void)

// 📌 Control Flow (If-Else, Loops, Function Flow)
case class IRJump(label: String) extends IRInstr // Unconditional jump
case class IRJumpCond(cond: String, label: String) extends IRInstr // Conditional jump
case class IRLabel(name: String) extends IRInstr // Label for jumps

// 📌 Input & Output Operations
case class IRPrint(value: String) extends IRInstr // Print a value
case class IRPrintln(value: String) extends IRInstr // Print a value with newline
case class IRRead(dest: String) extends IRInstr // Read into a variable

// 📌 Memory Management
case class IRAlloc(dest: String, size: Int) extends IRInstr // Allocate memory for arrays, structs
case class IRFree(value: String) extends IRInstr // Free allocated memory

// 📌 Array Operations
case class IRArrayLoad(dest: String, array: String, index: String) extends IRInstr
case class IRArrayStore(array: String, index: String, value: String) extends IRInstr

// 📌 Pair Operations
case class IRPairElem(dest: String, pair: String, isFirst: Boolean) extends IRInstr
case class IRNewPair(dest: String, left: String, right: String) extends IRInstr

// 📌 Debugging
case class IRComment(comment: String) extends IRInstr