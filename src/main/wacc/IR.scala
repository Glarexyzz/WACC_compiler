package wacc

// Intermediate Representation
sealed trait IRInstr

// Load & Store Instructions
case class IRLoad(dest: String, src: String) extends IRInstr  // Load from memory
case class IRStore(dest: String, src: String) extends IRInstr // Store to memory

// Arithmetic & Boolean Operations
case class IRBinaryOp(op: String, dest: String, left: String, right: String) extends IRInstr
case class IRUnaryOp(op: String, dest: String, src: String) extends IRInstr

// Function Calls & Returns
case class IRCall(func: String, args: List[String]) extends IRInstr
case class IRReturn(value: String) extends IRInstr

// Control Flow (If-Else, Loops)
case class IRJump(label: String) extends IRInstr
case class IRJumpCond(cond: String, label: String) extends IRInstr
case class IRLabel(name: String) extends IRInstr

// Input & Output
case class IRPrint(value: String) extends IRInstr
case class IRRead(dest: String) extends IRInstr