package wacc

// I really need a better name
object Helpers{
    // Helpers for setting up
    def pushReg(reg1: Register, reg2: Register): IRInstr = IRStp(reg1, reg2, 16, true) // negated in toString
    def popReg(reg1: Register, reg2: Register): IRInstr = IRLdp(reg1, reg2, 16, true) // also ooh magic numbers

    // Helpers for printing
    private def strLabel(label: String, no: Int): String = s".L._${label}_str$no"
    private def wordLabel(length: Int, label: String, asciz: String): List[IRInstr] = {
        List(
            IRCmt(s"length of ${label}"),
            IRWord(length), 
            IRFuncLabel(IRLabel(label), List(
            IRAsciz(asciz)))
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
    private def print(label: String, asciz: String): List[IRInstr] = {
        val str0label = strLabel(label, 0)
        val length = asciz.length
        wordLabel(length, str0label, asciz) :+ IRAlign(4) :+ printFunc(label)
    }

    def printi(): List[IRInstr] = {
        print("printi", "%d")
    }

    def printc(): List[IRInstr] = {
        print("printc", "%c")
    }

    def prints(): List[IRInstr] = {
        print("prints", "%.*s")

    }

    // Oh print b works entirely differently
    def printb(): List[IRInstr] = {
        val printB = "printb"
        val falseAsciz = "false"
        val trueAsciz = "true"
        val stringAsciz = "%.*s"
        val data = List (
            wordLabel(falseAsciz.length, strLabel(printB, 0), falseAsciz),
            wordLabel(trueAsciz.length, strLabel(printB, 1), trueAsciz),
            wordLabel(stringAsciz.length, strLabel(printB, 2), stringAsciz),
        ).flatten

        data ++ List (
            IRAlign(4),
            IRFuncLabel(IRLabel(printB), List(
                pushReg(LR, XZR),
                IRCmpImm(W0, 0),
                IRJumpCond(NE, ".L_printb0"),
                IRAdr(X2, strLabel(printB, 0)),
                IRJump(".L_printb1"),
            )),
            IRFuncLabel(IRLabel(".L_printb0"), List(
                IRAdr(X2, strLabel(printB, 1))
            )),
            IRFuncLabel(IRLabel(".L_printb1"), List(
                IRLdur(W1, X2, -4),
                IRAdr(X0, strLabel(printB, 2)),
                IRBl ("printf"),
                IRMov(X0, 0),
                IRBl("fflush"),
                popReg(LR, XZR),
                IRRet()
            ))
        )
    }

// // length of .L._printb_str0
// 	.word 5
// .L._printb_str0:
// 	.asciz "false"
// // length of .L._printb_str1
// 	.word 4
// .L._printb_str1:
// 	.asciz "true"
// // length of .L._printb_str2
// 	.word 4
// .L._printb_str2:
// 	.asciz "%.*s"
    // .align 4
// _printb:
// 	// push {lr}
// 	stp lr, xzr, [sp, #-16]!
// 	cmp w0, #0
// 	b.ne .L_printb0
// 	adr x2, .L._printb_str0
// 	b .L_printb1
// .L_printb0:
// 	adr x2, .L._printb_str1
// .L_printb1:
// 	ldur w1, [x2, #-4]
// 	adr x0, .L._printb_str2
// 	bl printf
// 	mov x0, #0
// 	bl fflush
// 	// pop {lr}
// 	ldp lr, xzr, [sp], #16
// 	ret


}