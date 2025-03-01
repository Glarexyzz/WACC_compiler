package wacc

// Intermediate Representation
sealed trait IRInstr

// 📌 Data Movement Instructions
// Move an immediate or register value
case class IRMov(dest: Register, value: Int) extends IRInstr {
    override def toString: String = s"mov $dest, #$value"
}
case class IRMovReg(dest: Register, src: Register) extends IRInstr {
  override def toString: String = s"mov $dest, $src"
}

case class IRMovk(dest: Register, immediate: Int, shift: Int) extends IRInstr {
  override def toString: String = s"movk $dest, #$immediate, lsl #$shift"
}

// Bitwise NOT (negates bits)	
case class IRMvn(dest: Register, src: Register) extends IRInstr {
    override def toString: String = s"mvn $dest, $src"
}
// Load address of a label into a register	
case class IRAdr(dest: Register, label: String) extends IRInstr {
    override def toString: String = s"adr $dest, $label"
}
// Load address of a label with offset into a register
case class IRAdrp(dest: Register, label: String) extends IRInstr {
    override def toString: String = s"adrp $dest, $label"
}
// Load from memory into a register	
case class IRLdr(dest: Register, addr: Register) extends IRInstr {
    override def toString: String = s"ldr $dest, [$addr]"
}
case class IRLdrsb(dest: Register, addr1: Register, addr2: Register, offset: Int) extends IRInstr {
    override def toString: String = s"ldr $dest, [$addr1, $addr2, lsl #$offset]"
}
// Load an unsigned register
case class IRLdur(dest: Register, addr: Register, offset: Int) extends IRInstr {
    override def toString: String = s"ldur $dest, [$addr, #$offset]"
}
// Store register value into memory	
case class IRStr(value: Register, addr: Register, offset: Option[Int] = None) extends IRInstr {
    override def toString: String = offset match {
        case Some(off) => s"str $value, [$addr, #$off]"
        case None      => s"str $value, [$addr]"
    }
}
case class IRStrb(value: Register, addr: Register, offset: Option[Int] = None) extends IRInstr {
    override def toString: String = offset match {
        case Some(off) => s"strb $value, [$addr, #$off]"
        case None      => s"strb $value, [$addr]"
    }
}
case class IRStrsb(value: Register, addr1: Register, addr2: Register, offset: Int) extends IRInstr {
    override def toString: String =  s"str $value, [$addr1, $addr2, lsl #$offset]"
          
}

// Store a pair of registers onto the stack	
case class IRStp(reg1: Register, reg2: Register, offset: Int, preDecrement: Boolean = false) extends IRInstr {
  override def toString: String =
    if (preDecrement) s"stp $reg1, $reg2, [sp, #$offset]!"
    else s"stp $reg1, $reg2, [sp, #$offset]"
}
// Store register value with offset into memory
case class IRStur(reg: Register, base: Register, offset: Int) extends IRInstr {
  override def toString: String = s"stur $reg, [$base, #$offset]"
}
// Load a pair of registers from the stack
case class IRLdp(reg1: Register, reg2: Register, offset: Int, postIncrement: Boolean = false) extends IRInstr {
  override def toString: String =
    if (postIncrement) s"ldp $reg1, $reg2, [sp], #$offset" 
    else s"ldp $reg1, $reg2, [sp]" 
}

// 📌 Arithmetic & Boolean Operations (Typed for Safety)
case class IRAdd(dest: Register, left: Register, right: Register) extends IRInstr {
    override def toString: String = s"add $dest, $left, $right"
}
case class IRAdds(dest: Register, left: Register, right: Register) extends IRInstr {
    override def toString: String = s"adds $dest, $left, $right"
}
case class IRAddsImm(dest: Register, left: Register, right: Int) extends IRInstr {
    override def toString: String = s"adds $dest, $left, #$right"
}
// to handle immediate values derived from labels
case class IRAddImm(dest: Register, left: Register, imm: String) extends IRInstr {
    override def toString: String = s"add $dest, $left, $imm"
}
case class IRAddImmInt(dest: Register, left: Register, imm: Int) extends IRInstr {
    override def toString: String = s"add $dest, $left, #$imm"
}

case class IRSub(dest: Register, left: Register, right: Register) extends IRInstr {
    override def toString: String = s"sub $dest, $left, $right"
}
case class IRSubImm(dest: Register, left: Register, right: Int) extends IRInstr {
    override def toString: String = s"sub $dest, $left, #$right"
}
case class IRMSub(dest: Register, q1: Register, q2: Register, s: Register) extends IRInstr {
    override def toString: String = s"msub $dest, $q1, $q2, $s"
}
case class IRMul(dest: Register, left: Register, right: Register) extends IRInstr {
    override def toString: String = s"mul $dest, $left, $right"
}
case class IRSMull(dest: Register, left: Register, right: Register) extends IRInstr {
    override def toString: String = s"smull $dest, $left, $right"
}
case class IRUDiv(dest: Register, left: Register, right: Register) extends IRInstr {
    override def toString: String = s"udiv $dest, $left, $right"
}
case class IRSDiv(dest: Register, left: Register, right: Register) extends IRInstr {
    override def toString: String = s"sdiv $dest, $left, $right"
}
case class IRAnd(dest: Register, left: Register, right: Register) extends IRInstr {
    override def toString: String = s"and $dest, $left, $right"
}
case class IROr(dest: Register, left: Register, right: Register) extends IRInstr {
    override def toString: String = s"orr $dest, $left, $right"
}
case class IRXor(dest: Register, left: Register, right: Register) extends IRInstr {
    override def toString: String = s"eor $dest, $left, $right"
}
// consider if needed: 
// https://developer.arm.com/documentation/ddi0602/2023-12/Base-Instructions/NEG--shifted-register---Negate--shifted-register---an-alias-of-SUB--shifted-register--?lang=encase 
class IRNeg(dest: Register, src: Register) extends IRInstr {
    override def toString: String = s"neg $dest, $src"
}

// 📌 Comparison Operations
// Compare two registers and set flags
case class IRCmp(left: Register, right: Register) extends IRInstr {
    override def toString: String = s"cmp $left, $right"
}

// Compare a register with an immediate value
case class IRCmpImm(left: Register, imm: Int) extends IRInstr {
    override def toString: String = s"cmp $left, #$imm"
}

case class IRCmpExt(left: Register, right: Register) extends IRInstr {
    override def toString: String = s"cmp $left, $right, sxtw"
}

// Set a register based on a condition flag
case class IRCset(reg: Register, condition: Condition) extends IRInstr {
    override def toString: String = s"cset $reg, $condition"
}
case class IRCsel(dest: Register, src1: Register, src2: Register, cond: Condition) extends IRInstr {
  override def toString: String = s"csel $dest, $src1, $src2, $cond"
}



// 📌 Branching and Function calls
// Jump to a label (b label_name)
case class IRJump(label: String) extends IRInstr {
    override def toString: String = s"b $label"
}
// Branch with link (calls a function)	
case class IRBl(func: String) extends IRInstr {
    override def toString: String = s"bl $func"
}
// Return from function	
case class IRRet() extends IRInstr {
    override def toString: String = "ret\n"
}
// Branch if condition 
case class IRJumpCond(cond: Condition, label: String) extends IRInstr {
    override def toString: String = s"b.$cond $label"
}

case class IRTst(reg: Register, value: Int) extends IRInstr {
  override def toString: String = s"tst $reg, #$value"
}

// 📌 System calls
case class IRSvc(syscallNum: Int) extends IRInstr {
    override def toString: String = s"svc $syscallNum"
}

// 📌 Functions and Comments
// Function label
case class IRLabel(name: String) extends IRInstr {
    override def toString: String = s"$name"
}

// Function label with indented instructions
case class IRFuncLabel(label: IRLabel, instr: List[IRInstr]) extends IRInstr {
    override def toString: String = {
        val instrStr = instr.map(instr => s"    $instr").mkString("\n")
        s"${label.name}:\n$instrStr"
  }
}
// Comment
case class IRCmt(comment: String) extends IRInstr {
    override def toString: String = s"// $comment"
}

// 📌 Specific Label Types
case class IRWord(value: Int) extends IRInstr {
  override def toString: String = s"    .word $value"
}
case class IRAsciz(value: String) extends IRInstr {
  override def toString: String = s""".asciz "$value""""
}
case class IRAlign(value: Int) extends IRInstr {
  override def toString: String = s".align $value"
}
case class IRGlobal(name: String) extends IRInstr {
  override def toString: String = s".global $name"
}

// 📌 Conditional Branch if Zero
case class IRCbz(reg: Register, label: String) extends IRInstr {
    override def toString: String = s"cbz $reg, $label"
}
