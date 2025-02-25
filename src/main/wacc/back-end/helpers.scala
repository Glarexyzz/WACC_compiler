package wacc

// I really need a better name
object Helpers{
    // Helpers for setting up
    def pushReg(reg1: Register, reg2: Register): IRInstr = IRStp(reg1, reg2, 16, true) // negated in toString
    def popReg(reg1: Register, reg2: Register): IRInstr = IRLdp(reg1, reg2, 16, true) // also ooh magic numbers

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

    def errOverflow(): List[IRInstr] = {
        val errStr = strLabel("errOverflow", 0)
        val errWord = wordLabel(52, errStr, "fatal error: integer overflow or underflow occurred\n")
        val instructions: List[IRInstr] = List(
            IRAdr(X0, errStr),
            IRBl("_prints"),
            IRMov(W0, -1),
            IRBl("exit")
        )
        errWord :+ IRAlign(4) :+ IRFuncLabel(IRLabel("_errOverflow"), instructions)
    }
}