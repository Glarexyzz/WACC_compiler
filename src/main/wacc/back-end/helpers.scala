package wacc
import scala.collection.mutable
import wacc.Constants.alignmentOffset
import wacc.Constants.stackOffset
// import wacc.Constants._

object Helpers{
    // 📌 Helpers for Pushing and Popping
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


    // 📌 Helpers for Labels
    private def strLabel(label: String, no: Int): String = s".L.${label}_str$no"
    def wordLabel(length: Int, label: String, asciz: String): List[IRInstr] = {
        List(
            IRCmt(s"length of ${label}"),
            IRWord(length), 
            IRFuncLabel(IRLabel(label), List(IRAsciz(asciz)))
        )
    }

    // 📌 Helpers for Printing
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
        wordLabel(length, str0label, asciz) :+ IRAlign(alignmentOffset) :+ func
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
            IRLdur(W1, X0, -stackOffset),
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
            IRAlign(alignmentOffset),
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
                IRLdur(W1, X2, -stackOffset),
                IRAdr(X0, strLabel(label, 2)),
                IRBl ("printf"),
                IRMov(X0, 0),
                IRBl("fflush"),
                popReg(LR, XZR),
                IRRet()
            ))
        )
    }
    def printp(): List[IRInstr] = {
        val label = "_printp"
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

        print(label, "%p", IRFuncLabel(IRLabel(label), instructions))
    }


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
        wordLabel(0, str0label, "") :+ IRAlign(alignmentOffset) :+ IRFuncLabel(IRLabel("_println"), instructions)
    }

    // 📌 Helpers for Reading

    def readFunc(label: String, asciz: String): List[IRInstr]= {
        val str0label = strLabel(label, 0)
        val instructions: List[IRInstr] = 
            pushRegs(List(X0, LR)) ++ List(
            IRMovReg(X1, SP),
            IRAdr(X0, str0label),
            IRBl("scanf"),
        ) ++ popRegs(List(X0, LR)) ++ List(
            IRRet()
        )
        wordLabel(asciz.length, str0label, asciz) :+ IRAlign(alignmentOffset) :+ IRFuncLabel(IRLabel(label), instructions)
    }

    def readi(): List[IRInstr] = {
        readFunc("_readi", "%d")
    }

    def readc(): List[IRInstr] = {
        readFunc("_readc", " %c")
    }

    // 📌 Helpers for Error Handling
    def errGen(name: String, errMsg: String, hasFormatting: Boolean = false): List[IRInstr] = {
        val errStr = strLabel(name, 0)
        val errWord = wordLabel(errMsg.length - 1, errStr, errMsg)
        val base = List(IRAdr(X0, errStr))
        val midsection = if (hasFormatting) List(
            IRBl("printf"),
            IRMov(X0, 0),
            IRBl("fflush")
        ) else List(
            IRBl("_prints")
        )
        errWord :+ IRAlign(alignmentOffset) :+ IRFuncLabel(IRLabel(name), base ++ midsection ++ List(IRMov(W0, -1), IRBl("exit")))
    }


    // 📌 Error Handling
    // Overflow, Out of Memory and Division by Zero errors do not require any additional helpers
    def errOverflow(): List[IRInstr] = {
        errGen("_errOverflow", "fatal error: integer overflow or underflow occurred\\n")
    }

    def errOutOfMemory(): List[IRInstr] = {
        errGen("_errOutOfMemory", "fatal error: out of memory\\n")
    }

    def errDivZero(): List[IRInstr] = {
        errGen("_errDivZero", "fatal error: division or modulo by zero\\n")
    }

    def errNull(): List[IRInstr] = {
        errGen("_errNull", "fatal error: null pair dereferenced or freed\\n")
    }

    // Out of Bounds and Bad Char errors require "_prints" to be defined in CodeGen
    def errOutOfBounds(): List[IRInstr] = {
        errGen("_errOutOfBounds", "fatal error: array index %d out of bounds\\n", true)
    }

    def errBadChar(): List[IRInstr] = {
        errGen("_errBadChar", "fatal error: int %d is not ascii character 0-127\\n", true)
    }

    def malloc(): List[IRInstr] = {
        val label = "_malloc"
        val instructions: List[IRInstr] = List(
            pushReg(LR, XZR),       
            IRBl("malloc"),         
            IRCbz(X0, "_errOutOfMemory"), 
            popReg(LR, XZR),         
            IRRet()
        )
        List(IRFuncLabel(IRLabel(label), instructions))
    }

    // 📌 Array Store and Array Load
    def arrLoad(label: String, dest: Register, shift: Int): List[IRInstr] = {
        val instructions: List[IRInstr] = List(
            pushReg(LR, XZR), 
            IRCmpImm(W17, 0), 
            IRCsel(X1, X17, X1, LT), 
            IRJumpCond(LT, "_errOutOfBounds"), 

            IRLdur(W30, X7, -stackOffset), 
            IRCmp(W17, W30), 
            IRCsel(X1, X17, X1, GE), 
            IRJumpCond(GE, "_errOutOfBounds"), 

            IRLdrsb(dest, X7, X17, shift), // Load with shift

            popReg(LR, XZR),
            IRRet()
        )

        List(IRFuncLabel(IRLabel(label), instructions))
        }

    // Define arrLoad4 and arrLoad8 using the generalized function
    def arrLoad4(): List[IRInstr] = arrLoad("_arrLoad4", W7, shift = 2)
    def arrLoad8(): List[IRInstr] = arrLoad("_arrLoad8", X7, shift = 3)

    
     
    def arrStore(label: String, reg: Register, shift: Int): List[IRInstr] = {
        val storeInstr = 
            if (shift == 2) {
                IRStrsb(reg, X7, X17, 2) 
            } else if (shift == 3) {
                IRStrsb(reg, X7, X17, 3)
            } else {
                IRStrbReg(reg, X7, X17)      
            }       

        val instructions: List[IRInstr] = List(
            pushReg(LR, XZR), 
            IRCmpImm(W17, 0), 
            IRCsel(X1, X17, X1, LT), 
            IRJumpCond(LT, "_errOutOfBounds"), 

            IRLdur(W30, X7, -stackOffset), 
            IRCmp(W17, W30), 
            IRCsel(X1, X17, X1, GE), 
            IRJumpCond(GE, "_errOutOfBounds"),

            storeInstr, // Store instruction based on `isWord`

            popReg(LR, XZR),
            IRRet()
        )

        List(IRFuncLabel(IRLabel(label), instructions))
        }

    // Define arrStore4 and arrStore1 using the generalized function
    def arrStore8(reg: Register): List[IRInstr] = arrStore("_arrStore8", reg, shift = 3)
    def arrStore4(reg: Register): List[IRInstr] = arrStore("_arrStore4", reg, shift = 2)
    def arrStore1(reg: Register): List[IRInstr] = arrStore("_arrStore1", reg, shift = 0)

        
    // 📌 Freeing Pairs
    def freepair() : List[IRInstr] =
        val label = "_freepair"
        val instructions: List[IRInstr] = List(
            pushReg(LR, XZR),
            IRCbz(X0, "_errNull"),
            IRBl("free"),
            popReg(LR, XZR),
            IRRet()
        )
        List(IRFuncLabel(IRLabel(label), instructions)) 

}