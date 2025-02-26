package wacc
import scala.collection.mutable
// import wacc.Constants._

// I really need a better name
object Helpers{
    // Helpers for setting up
    def pushReg(reg1: Register, reg2: Register): IRInstr = IRStp(reg1, reg2, -16, true) // negated in toString
    def popReg(reg1: Register, reg2: Register): IRInstr = IRLdp(reg1, reg2, 16, true) // also ooh magic numbers

    def pushRegs(registers: List[Register]): List[IRInstr] = {
        val instrs = mutable.ListBuffer[IRInstr]()
        val totalRegs = registers.length

        if (totalRegs == 1) {
            return List(pushReg(registers.head, XZR))
        }
        // Calculate total stack space needed
        val totalStackSpace: Int = ((totalRegs + 1) / 2) * 16

        val pairs = registers.grouped(2).toList
        var offset = -totalStackSpace

        // Push pairs first
        pairs.zipWithIndex.foreach {
            case (List(reg1, reg2), index) =>
                if (index == 0) {
                    // First pair moves the stack pointer
                    instrs += IRStp(reg1, reg2, offset, preDecrement = true)
                    offset = 16 // Subsequent offsets start at +16 from SP
                } else {
                    instrs += IRStp(reg1, reg2, offset, preDecrement = false)
                    offset += 16
                }
            case (List(reg), _) => // Odd register remaining
                instrs += IRStur(reg, SP, offset)
            case(_, _) => // No registers remaining
        }
        instrs.toList
    }

    def popRegs(registers: List[Register]): List[IRInstr] = {
        val instrs = mutable.ListBuffer[IRInstr]()
        val totalRegs = registers.length

        if (totalRegs == 1) {
            return List(popReg(registers.head, XZR))
        }

        val totalStackSpace = ((totalRegs + 1) / 2) * 16 // total stack space to restore at the end

        val pairs = registers.grouped(2).toList
        var offset = 16

        if (totalRegs % 2 == 1) { 
            // Handle single register (odd case) first
            instrs += IRLdur(pairs.last.head, SP, offset)
            pairs.dropRight(1).reverse.zipWithIndex.foreach {
            case (List(reg1, reg2), index) =>
                val isLast = index == pairs.length - 2
                if (isLast) {
                    instrs += IRLdp(reg1, reg2, totalStackSpace, postIncrement = true)
                } else {
                    instrs += IRLdp(reg1, reg2, offset, postIncrement = false)
                    offset += 16
                }
            case (_, _) => // No registers remaining
            }
        } else {
            // Even number of registers
            pairs.reverse.zipWithIndex.foreach {
            case (List(reg1, reg2), index) =>
                val isLast = index == pairs.length - 1
                if (isLast) {
                    instrs += IRLdp(reg1, reg2, totalStackSpace, postIncrement = true)
                } else {
                    instrs += IRLdp(reg1, reg2, offset, postIncrement = false)
                    offset += 16
                }
            case (_, _) => // No registers remaining
            }
        }
        instrs.toList
    }


    // Helpers for printing
    private def strLabel(label: String, no: Int): String = s".L.${label}_str$no"
    def wordLabel(length: Int, label: String, asciz: String): List[IRInstr] = {
        List(
            IRCmt(s"length of ${label}"),
            IRWord(length), 
            IRFuncLabel(IRLabel(label), List(IRAsciz(asciz)))
        )
    }

    private def printFunc(label: String): IRFuncLabel = {
        val str0label = strLabel(label, 0)
        val instructions: List[IRInstr] = List(
            pushReg(LR, XZR),
            IRMovReg(X1, X0),
            IRAdr(X0, str0label),
            IRBl("printf"),
            IRMov(X0, 0),
            IRBl("fflush"),
            popReg(LR, XZR),
            IRRet()
        )
        IRFuncLabel(IRLabel(label), instructions)
    }
    private def print(label: String, asciz: String, func: IRFuncLabel): List[IRInstr] = {
        val str0label = strLabel(label, 0)
        val length = asciz.length
        wordLabel(length, str0label, asciz) :+ IRAlign(4) :+ func
    }

    def printi(): List[IRInstr] = {
        print("_printi", "%d", printFunc("_printi"))
    }

    def printc(): List[IRInstr] = {
        print("_printc", "%c", printFunc("_printc"))
    }

    def prints(): List[IRInstr] = {
        val label = "_prints"
        val str0label = strLabel(label, 0)
        val instructions: List[IRInstr] = List(
            pushReg(LR, XZR),
            IRMovReg(X2, X0),
            IRLdur(W1, X0, -4),
            IRAdr(X0, str0label),
            IRBl("printf"),
            IRMov(X0, 0),
            IRBl("fflush"),
            popReg(LR, XZR),
            IRRet()
        )
        print(label, "%.*s", IRFuncLabel(IRLabel(label), instructions))
    }

    def printb(): List[IRInstr] = {
        val label = "_printb"
        val falseAsciz = "false"
        val trueAsciz = "true"
        val stringAsciz = "%.*s"
        val data = List (
            wordLabel(falseAsciz.length, strLabel(label, 0), falseAsciz),
            wordLabel(trueAsciz.length, strLabel(label, 1), trueAsciz),
            wordLabel(stringAsciz.length, strLabel(label, 2), stringAsciz),
        ).flatten

        data ++ List (
            IRAlign(4),
            IRFuncLabel(IRLabel(label), List(
                pushReg(LR, XZR),
                IRCmpImm(W0, 0),
                IRJumpCond(NE, ".L_printb0"),
                IRAdr(X2, strLabel(label, 0)),
                IRJump(".L_printb1"),
            )),
            IRFuncLabel(IRLabel(".L_printb0"), List(
                IRAdr(X2, strLabel(label, 1))
            )),
            IRFuncLabel(IRLabel(".L_printb1"), List(
                IRLdur(W1, X2, -4),
                IRAdr(X0, strLabel(label, 2)),
                IRBl ("printf"),
                IRMov(X0, 0),
                IRBl("fflush"),
                popReg(LR, XZR),
                IRRet()
            ))
        )
    }
// length of .L._println_str0
// 	.word 0
// .L._println_str0:
// 	.asciz ""
// .align 4
// _println:
// 	// push {lr}
// 	stp lr, xzr, [sp, #-16]!
// 	adr x0, .L._println_str0
// 	bl puts
// 	mov x0, #0
// 	bl fflush
// 	// pop {lr}
// 	ldp lr, xzr, [sp], #16
// 	ret
    def printlnFunc(): List[IRInstr] = {
        val str0label = strLabel("_println", 0)
        val instructions: List[IRInstr] = List(
            pushReg(LR, XZR),
            IRAdr(X0, str0label),
            IRBl("puts"),
            IRMov(X0, 0),
            IRBl("fflush"),
            popReg(LR, XZR),
            IRRet()
        )
        wordLabel(0, str0label, "") :+ IRAlign(4) :+ IRFuncLabel(IRLabel("_println"), instructions)
    }

    def errGen(errStr: String): List[IRInstr] = {
        List(
            IRAdr(X0, errStr),
            IRBl("_prints"),
            IRMov(W0, -1),
            IRBl("exit")
        )
    }

    def errOverflow(): List[IRInstr] = {
        val errStr = strLabel("errOverflow", 0)
        val errWord = wordLabel(52, errStr, "fatal error: integer overflow or underflow occurred\\n")
        errWord :+ IRAlign(4) :+ IRFuncLabel(IRLabel("_errOverflow"), errGen(errStr))
    }

    def errOutOfMemory(): List[IRInstr] = {
        val errStr = strLabel("errOutOfMemory", 0)
        val errWord = wordLabel(27, errStr, "fatal error: out of memory\\n")
        errWord :+ IRAlign(4) :+ IRFuncLabel(IRLabel("_errOutOfMemory"), errGen(errStr))
    }

    def errOutOfBounds(): List[IRInstr] = {
        val errStr = strLabel("errOutOfBounds", 0)
        val errWord = wordLabel(42, errStr, "fatal error: array index %d out of bounds\\n")
        errWord :+ IRAlign(4) :+ IRFuncLabel(IRLabel("_errOutOfBounds"), errGen(errStr))
    }

    def errBadChar(): List[IRInstr] = {
        val errStr = strLabel("errBadChar", 0)
        val errWord = wordLabel(50, errStr, "fatal error: int %d is not ascii character 0-127 \\n")
        errWord :+ IRAlign(4) :+ IRFuncLabel(IRLabel("_errBadChar"), List(
            IRAdr(X0, errStr),
            IRBl("printf"),
            IRMov(X0, 0),
            IRBl("fflush"),
            IRMov(W0, -1),
            IRBl("exit")
        ))
    }
}