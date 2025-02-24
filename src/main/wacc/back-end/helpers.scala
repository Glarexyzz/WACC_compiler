package wacc

// I really need a better name
object Helpers{
    // idea:
    // There are some common set of instructions that are used
    // For example:
    // start of program / start of function
    // end of program / end of function
    // 

    def pushReg(reg1: Register, reg2: Register): IRInstr = IRStp(reg1, reg2, 16, true) // where did the extra negative come from?
    def popReg(reg1: Register, reg2: Register): IRInstr = IRLdp(reg1, reg2, 16, true) // also ooh magic numbers
}
/*
 _errOverflow:
	adr x0, .L._errOverflow_str0
	bl _prints
	mov w0, #-1
	bl exit
*/    