package wacc

object Constants {
  val exitValid            = 0
  val exitInvalidSyntax    = 100
  val exitInvalidSemantics = 200
  val initStackVarsOffset  = -992
  val defReturnValue       = 0
  val defOffset            = 0
  val defPairNullValue     = 0
  val falseValue           = 0
  val trueValue            = 1
  val defArrPairReg        = X0
  val defChrReg            = X0
  val chrRangeCheckReg     = X1
  val defArrTempReg        = X7
  val defTempReg           = X8
  val arrPairStrReg        = X16
  val paramsReg            = X16
  val defPushTempReg       = X17
  val defNormReg           = X19
  val tempIOReg            = W0
  val defReturnReg         = W0
  val modTempReg           = W1
  val indexReg             = W17
  val pointerSize          = 8    
  val pairMemorySize       = 16      
  val arrayMetadataSize    = 4  
  val intSize              = 4  
  val charSize             = 1            
  val boolSize             = 1
  val firstIndex           = 0
}
