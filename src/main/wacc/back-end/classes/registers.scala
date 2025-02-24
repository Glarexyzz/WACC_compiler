package wacc

sealed trait Register {
  override def toString: String = this.getClass.getSimpleName.toLowerCase.stripSuffix("$")
}

// Argument registers
case object X0 extends Register
case object X1 extends Register
case object X2 extends Register
case object X3 extends Register
case object X4 extends Register
case object X5 extends Register
case object X6 extends Register
case object X7 extends Register
// Indirect Result Register
case object X8 extends Register
// General Purpose Registers
case object X9 extends Register
case object X10 extends Register
case object X11 extends Register
case object X12 extends Register
case object X13 extends Register
case object X14 extends Register
case object X15 extends Register
// "inter-procedural scratch register"
case object X16 extends Register
case object X17 extends Register
// Platform Register
case object X18 extends Register
// General Purpose Registers
case object X19 extends Register
case object X20 extends Register
case object X21 extends Register
case object X22 extends Register
case object X23 extends Register
case object X24 extends Register
case object X25 extends Register
case object X26 extends Register
case object X27 extends Register
case object X28 extends Register
case object FP extends Register // X29 - Frame Pointer
case object LR extends Register // X30 - Link Register
case object SP extends Register // Stack Pointer
case object XZR extends Register // Zero Register

// 32-bit registers
case object W0 extends Register
case object W1 extends Register
// case object W2 extends Register
// case object W3 extends Register
// case object W4 extends Register
// case object W5 extends Register
// case object W6 extends Register
// case object W7 extends Register
// case object W8 extends Register
// case object W9 extends Register
// case object W10 extends Register
// case object W11 extends Register
// case object W12 extends Register
// case object W13 extends Register
// case object W14 extends Register
// case object W15 extends Register
// case object W16 extends Register
// case object W17 extends Register
// case object W18 extends Register
case object W19 extends Register
// case object W20 extends Register
// case object W21 extends Register
// case object W22 extends Register
// case object W23 extends Register
// case object W24 extends Register
// case object W25 extends Register
// case object W26 extends Register
// case object W27 extends Register
// case object W28 extends Register
// case object W29 extends Register  // Frame Pointer
// case object W30 extends Register  // Link Register
