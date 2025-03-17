package wacc

import java.io.{File, PrintWriter}
import scala.collection.mutable
import scala.util.control.Breaks._
import wacc.Helpers._
import wacc.Constants._



/*
1. Define IR representations for AST
2. Implement code generation for expressions and statements
3. Write an AArch64 assembly backend (to plan in detail more later)
*/
object CodeGen {

  private val stringLiterals: mutable.LinkedHashMap[String, String] = mutable.LinkedHashMap() // Store unique string labels
  def nextLabel(): String = s".L.str${stringLiterals.size}"

  private var symbolTable = new SymbolTable()

  // Register allocation
  // We need to account for spill over registers
  private val argumentRegisters: List[Register] = List(X0, X1, X2, X3, X4, X5, X6, X7)
  private val tempRegisters: List[Register] = List(X8, X9, X10, X11, X12, X13, X14, X15)
  private val availableTempRegisters = mutable.Stack[Register](X8, X9, X10, X11, X12, X13, X14, X15)
  private val activeTempRegisters = mutable.Set[Register]()
  private val availableRegisters = mutable.Stack[Register](X19, X20, X21, X22, X23, X24, X25, X26, X27, X28) // Pool of free registers
  private val activeRegisters = mutable.Set[Register]()  // Set of registers currently in use
  private val registerStack = mutable.Stack[Register]()  // Stack for spilled registers
  private val instrBuffer = mutable.ListBuffer[IRInstr]()
  private val variableRegisters = mutable.Map[String, (Register, Type)]()
  private val variableOffsets = mutable.Map[String, (Int, Type)]()
  private var stackVarPointer = initStackVarsOffset
  private var constants = mutable.Map[String, (Type, Any)]()
  private var unusedVars = mutable.Set.empty[String]
  // for variables
  private val availableVariableRegisters = mutable.Stack[Register]()
  private val availableVariableOffsets = mutable.Stack[Int]()
  private val variableRegistersStack = mutable.Stack[mutable.Map[String, (Either[Register, Int], Type)]]()

  private def getRegister(): Option[Register] = {
    if (availableRegisters.nonEmpty) {
      val reg = availableRegisters.pop()
      activeRegisters += reg
      return Some(reg)
    }

    None
  }

  private def getTempRegister(): Option[Register] = {
    if (availableTempRegisters.nonEmpty) {
      val reg = availableTempRegisters.pop()
      activeTempRegisters += reg
      return Some(reg)
    }
    // If no temp registers available, get a normal one
    getRegister()
  }


  def freeRegister(reg: Register): Unit = {
    if (registerStack.nonEmpty && registerStack.top == reg) {
      instrBuffer += IRLdr(reg, SP)  // Restore spilled register
      registerStack.pop()
    } else if (tempRegisters.contains(reg)) {
      availableTempRegisters.push(reg)
    } else {
      availableRegisters.push(reg)
    }
    activeRegisters -= reg
  } 

  private def lookupVariable(name: String): Option[(Either[Register, Int], Type)] = {
    // Check function parameters first
    paramsMap.get(name) match {
      case Some((reg, t)) => 
        Some((Left(reg), t)) // Parameters are stored in registers
      case None =>
        // If not in params, check local variables in scopes
        variableRegistersStack.iterator.flatMap(_.get(name)).nextOption
    }
  }

  private def addVariable(name: String, t: Type): Option[Either[Register, Int]] = {
    if (variableRegistersStack.isEmpty) {
      throw new RuntimeException("Trying to add variable when no active scope exists!")
    }

    val currentScope = variableRegistersStack.top

    // Prevent duplicate variables in the same scope
    if (currentScope.contains(name)) {
      return None
    }

    // Don't allocate if it's already in the paramsMap
    if (paramsMap.contains(name)) {
      return Some(Left(paramsMap(name)._1))
    }

    if (availableVariableRegisters.nonEmpty) {
      val reg = availableVariableRegisters.pop()
      // println(s"Allocated register $reg for variable $name in scope ${variableRegistersStack.size}")
      currentScope(name) = (Left(reg), t)
      Some(Left(reg))
    } else if (availableVariableOffsets.nonEmpty) {
      val off = availableVariableOffsets.pop()
      // println(s"Allocated stack offset $off for variable $name in scope ${variableRegistersStack.size}")
      currentScope(name) = (Right(off), t)
      Some(Right(off))
    } else {
        throw new RuntimeException("Out of registers and stack offsets for variables!")
    }
  }

  private def freeVariableRegister(reg: Register): Unit = {
    availableVariableRegisters.push(reg)
  }

  def getStackVarOffset(varType: Type): Option[Int] = {
    val varSize: Option[Int] = varType match {
      case BaseType.IntType => Some(intSize)
      case BaseType.BoolType => Some(boolSize)
      case BaseType.CharType => Some(charSize)
      case _ => None
    }
    varSize match {
      case Some(value) if (stackVarPointer < -(value)) => 
        val offset = stackVarPointer
        stackVarPointer += value
        return Some(offset)
      case _ => return None
    }
  }
    

  // Helper functions generated 
  private val helpers = mutable.Map[IRLabel, List[IRInstr]]()

  // Main function
  def compile(prog: Program, filepath: String, newSymbolTable: SymbolTable, 
  constantVars: mutable.Map[String, (Type, Any)], unusedVariables: mutable.Set[String]): Unit = {
    //println(constantVars)
    println("Compiling...")
    // initialise symbol table
    symbolTable = newSymbolTable
    constants = constantVars
    unusedVars = unusedVariables
    val ir = generateIR(prog)

    // AArch64 assembly conversion
    // val assembly = AArch64Gen.generateAssembly(ir, stringLiterals)
    val assembly = ir.map(_.toString).mkString("\n") + "\n"
    val asmFileName = new File(filepath).getName.replaceAll("\\.wacc$", ".s")
    val asmFile = new File(asmFileName).getAbsolutePath 

    writeToFile(asmFile, assembly)

  }

    // Utility function to write assembly code to file
  def writeToFile(filename: String, content: String): Unit = {
      val writer = new PrintWriter(new File(filename))
      writer.write(content)
      writer.close()
  }

  // need to create generateMainIR, generateHeadIR, generateHelperIRs
  def generateIR(prog: Program): List[IRInstr] = {
    // evaluate main and func first
    val mainIR = generateMainIR(prog.stmt) // Handles main function
    val funcIRs = prog.funcs.flatMap(generateFunc) // Handles any wacc functions
    val headIR = generateHeadIR()      // Handles .data and .text sections
    val helperIRs = generateHelperIRs() // Handles any _helper functions

    headIR ++ mainIR ++ funcIRs ++ helperIRs
  }

  def generateMainIR(stmt: Stmt): List[IRInstr] = generateFunctionIR(stmt)

  def generateFunc(func: Func): List[IRInstr] = {
    generateFunctionIR(func.stmt, Some(func.name), func.paramList)
  }

  // for storing parameters of functions
  private var paramsMap = mutable.Map[String, (Register, Type)]()

  def assignFuncParams(params: List[Param]):Map[String, (Register, Type)] = {
    params.zip(argumentRegisters).map {
      case (param, reg) => (param.name, (reg, param.t))
    }.toMap
  }

  def pushFunctionParams(params: List[Register]): List[IRInstr] = {
    if (params.isEmpty) return List()

    val numParams = params.length
    if (numParams <= 1) {
      return List() // No need to push a single parameter
    }
    val stackSize = (numParams + 1) / 2 * 16  // Always round up to nearest multiple of 16

    val groupedParams: List[List[Register]] = 
      params.sorted(Ordering.by(argumentRegisters.indexOf)).grouped(2).toList // Group into pairs
    
    // First pair should decrement SP and store
    val firstPush = groupedParams.head match {
      case List(reg1, reg2) =>
        List(
          IRCmt(s"push {$reg1, $reg2}"),
          IRStp(reg1, reg2, -stackSize, true) // First pair decrements SP!
        )
      case List(reg1) =>
        List(
          IRCmt(s"push {$reg1}"),
          IRStp(reg1, XZR, -stackSize, true) // Store single register
        )
      case _ => List()
    }   

    // Remaining pairs use positive offsets
    val remainingPushes = groupedParams.tail.zipWithIndex.flatMap { case (regs, index) =>
      val offset = (index + 1) * 16 // Start at #16, then #32, etc.
      regs match {
        case List(reg1, reg2) =>
          List(
            IRCmt(s"push {$reg1, $reg2}"),
            IRStp(reg1, reg2, offset) // Use positive offsets
          )
        case List(reg1) =>
          List(
            IRCmt(s"push {$reg1}"),
            IRStp(reg1, XZR, offset) // Store single register

          )
        case _ => List()
      }
    }

    firstPush ++ remainingPushes ++ List(IRMovReg(X16, SP))
  }

  def popFunctionParams(params: List[Register]): List[IRInstr] = {
    if (params.isEmpty) return List()
    val numParams = params.length
    if (numParams <= 1) {
      return List() // No need to pop if only one parameter (it remains in x0)
    }

    val sortedParams = params.sorted(Ordering.by(argumentRegisters.indexOf)) // Ensure correct order
    val groupedParams: List[List[Register]] = sortedParams.grouped(2).toList // Group into pairs

    val firstPop = groupedParams.head match {
      case List(reg1, reg2) =>
        List(
          IRCmt(s"pop {$reg1, $reg2}"),
          IRLdp(reg1, reg2) // First pair at SP
        )
      case List(reg1) =>
        List(
          IRCmt(s"pop {$reg1}"),
          IRLdur(reg1, SP, 0) // Load single register with LDUR
        )
      case _ => List()
    }

    val remainingPops = groupedParams.tail.zipWithIndex.flatMap {
      case (List(reg1, reg2), index) =>
        val offset = (index + 1) * 16
        List(
          IRCmt(s"pop {$reg1, $reg2}"),
          IRLdp(reg1, reg2, offset, true) // Pop at offset
        )
      case (List(reg1), index) =>
        val offset = (index + 1) * 16
        List(
          IRCmt(s"pop {$reg1}"),
          IRLdur(reg1, SP, offset) // Load single register with LDUR
        )
      case _ => List()
    }
    firstPop ++ remainingPops
  }

  private val poppedParams = mutable.Set[String]()
  
  // Branches for main function
  private var nBranch = 0 // Track number of branching sections
  private var branches = mutable.ListBuffer[IRInstr]() // Store branches as IRFuncLabels
  private var currentBranch = mutable.ListBuffer[IRInstr]() // The working branch
  // default is currentBranch
  private def branchLabel(n: Int = 0, funcLabel: Option[String] = None): String = {
    val branchNo = nBranch + n
    funcLabel match {
      case Some(name) => s"wacc_$name"
      case None if branchNo == 0 => "main" 
      case None => s".L${branchNo-1}"
    }
  }

  private def addBranch(funcLabel: Option[String] = None): Unit = {
    branches += IRFuncLabel(IRLabel(branchLabel(funcLabel = funcLabel)), currentBranch.toList)
    currentBranch = mutable.ListBuffer[IRInstr]() // Reset for next branch
    nBranch += 1
  }

  private def getBranch(label: String): Option[Int] = {
    var result: Option[Int] = None

    breakable {
      for ((branch, index) <- branches.zipWithIndex) {
        branch match {
          case funcLabel: IRFuncLabel if funcLabel.label.name == label => 
            result = Some(index)
            break()
          case _ =>
        }
      }
    }
    result
  }

  private def adjustLabelBranch(label: String): Unit = {
    // Find the function label entry and remove it from the list
    val (funcLabelBranch, remainingBranches) = branches.partition {
      case IRFuncLabel(IRLabel(name), _) => name == label
      case _ => false
    }

    val funcInstrs = funcLabelBranch.head match {
        case IRFuncLabel(_, instrs) => instrs
        case _ => throw new RuntimeException("Unexpected type in function label branch")
    }
    // Extract all existing labels and their instructions
    val labelsAndInstrs = remainingBranches.collect {
        case IRFuncLabel(IRLabel(name), instrs) => (name, instrs)
    }
    // Shift labels: Keep the same order, but move the function label to the front
    val reorderedBranches = mutable.ListBuffer[IRFuncLabel]()
    // Put function label first with first instr set
    reorderedBranches += 
      IRFuncLabel(IRLabel(label), labelsAndInstrs.head._2.map(decrementJumpTargets)) 
    // Rename labels sequentially
    reorderedBranches ++=
      labelsAndInstrs.tail.zipWithIndex.map { case ((name, instrs), idx) =>
        IRFuncLabel(IRLabel(s".L$idx"), instrs.map(decrementJumpTargets)) 
      }
    reorderedBranches += 
      IRFuncLabel(IRLabel(s".L${labelsAndInstrs.length - 1}"), funcInstrs.map(decrementJumpTargets))
    // Update branches with reordered labels
    branches.clear()
    branches ++= reorderedBranches
  }

  private def decrementJumpTargets(instr: IRInstr): IRInstr = instr match {
    case IRJump(label) if label.startsWith(".L") =>
        val newLabel = label.drop(2).toInt - 1
        IRJump(s".L$newLabel")
    case IRJumpCond(cond, label) if label.startsWith(".L") =>
        val newLabel = label.drop(2).toInt - 1
        IRJumpCond(cond, s".L$newLabel")
    case other => other // Leave other instructions unchanged
  }


  private def overwriteJump(label: String, newJumpLabel: String): Unit =
    getBranch(label) match {
      case Some(i) =>
        branches(i) match {
          case funcLabel: IRFuncLabel =>
            if (funcLabel.instr.nonEmpty) {
              val newInstrs = funcLabel.instr.init :+ IRJump(newJumpLabel) // Remove the last, add the new one
              val newBranch = funcLabel.copy(instr = newInstrs)
              branches.update(i, newBranch) // Overwrite the branch
            }
          case _ =>
        }
      case _ =>
    }

  def generateFunctionIR(
    stmt: Stmt, 
    funcLabel: Option[String] = None, 
    paramRegs: Option[List[wacc.Param]] = None
  ): List[IRInstr] = {
    branches = mutable.ListBuffer[IRInstr]() // cleanup the branches

    
    paramsMap = mutable.Map.from(paramRegs.map(assignFuncParams).getOrElse(Map()))
    variableRegisters ++= paramsMap  // Store parameter registers in variable map

    val allocatedRegs = initialiseVariables(symbolTable, funcLabel)

    // function: Save parameters onto the stack to allow modification
    val paramPushInstrs = pushFunctionParams(paramsMap.values.map(_._1).toList)

    // Generate prologue (Add to first branch)
    val prologue = funcLabel match {
      case Some(_) =>
        List(
          IRCmt("Function prologue"),
          pushReg(FP, LR),
          IRMovReg(FP, SP)
        ) ++ paramPushInstrs
      case None =>
        List(
          IRCmt("Main/Branch prologue"),
          pushReg(FP, LR)
        ) ++ pushRegs(allocatedRegs) ++ List(
          IRMovReg(FP, SP)
        )
    }

    currentBranch ++= prologue

    //println(s"Function $funcLabel Parameters: ${paramsMap.map { case (k, v) => s"$k -> ${v._1}" }.mkString(", ")}")
    enterScope()
    generateStmt(stmt) // Generate IR for main function body
    exitScope()

    // Add function epilogue (add to last branch)
    val epilogue = funcLabel match {
      case Some(_) =>
        None
      case None =>
        List(
          IRCmt("Main/Branch epilogue"),
          IRMov(defReturnReg.asX, defReturnValue) // Default return code
        ) ++ popRegs(allocatedRegs) ++ List(
          popReg(FP, LR),
          IRRet()
        )
    }

    currentBranch ++= epilogue

    addBranch(funcLabel)

    // Reorder branch Ensure function label appears first, 
    // followed by branch labels inside the function
    funcLabel
      .flatMap(label => getBranch("wacc_" + label)
      .filter(_ != 0).map(_ => adjustLabelBranch("wacc_" + label)))

    branches.toList
  }

  // Variable Registers
  def initialiseVariables(symTab: SymbolTable, funcName: Option[String] = None): List[Register] = {
      
      val allocatedParamsRegs = mutable.Stack[Register]()
      var maxParams = 0
      var paramsRegsNeeded = 0
      val allocated = mutable.Stack[Register]()
      var maxVars = 0
      var regsNeeded = 0
      funcName match {
        case Some(name) =>
          // Functions parameters initiation
          maxParams = symTab.getCurrentFunctionParamsNum(name)
          paramsRegsNeeded = math.min(maxParams, argumentRegisters.size)
          allocatedParamsRegs ++= argumentRegisters.take(paramsRegsNeeded)
        case None => 
          // Scopes variables initiation
          maxVars = symTab.getMaxConcurrentVariables
          // println(s"MaxVariableNums: $maxVars")
          regsNeeded = math.min(maxVars, availableRegisters.size)
          allocated ++= availableRegisters.take(regsNeeded)
          availableVariableRegisters.pushAll(allocated.reverse)
          availableRegisters --= allocated
          // println(s"Allocated registers: $allocated")
      }

      // Pre-allocate stack space for the rest
      val spillVars = (maxVars - regsNeeded) + (maxParams - paramsRegsNeeded)
      for (_ <- 0 until spillVars) {
          getStackVarOffset(BaseType.IntType) match {
              case Some(off) => 
                //println(s"📌offset: $off")
                availableVariableOffsets.push(off)
              case None => 
                  throw new Exception("Ran out of stack offsets for spilling variables!")
          }
      }
      // println(s"parameters: $allocatedParamsRegs\n")
      // println(s"variables: $allocated\n")
      (allocated ++= allocatedParamsRegs).toList
  }
  

  def push(reg: Register, off: Int): Unit = {
    if (off < -256) then {
      currentBranch += IRMov(defPushTempReg, off) += IRStrWithReg(reg, FP, defPushTempReg)
    } else {
      currentBranch += IRStur(reg, FP, off)
    }
  }

  // Scopes
  def enterScope() = {
    variableRegistersStack.push(mutable.Map.empty)
  }

  def exitScope() = {
    if (variableRegistersStack.nonEmpty) {
      val currentScopeVars = variableRegistersStack.pop()

      // Only free **local** variables, not function parameters
      currentScopeVars.foreach {
        case (name, (Left(reg), _)) if !paramsMap.contains(name) => 
          // println(s"Freeing variable $name from register $reg")
          freeVariableRegister(reg)
        case (name, (Right(off), _)) if !paramsMap.contains(name) => 
          // println(s"Freeing variable $name from stack offset $off")
          availableVariableOffsets.push(off)
        case _ => // Don't free function parameters
      }

      
      // Restore only shadowed variables, not all from parent scope
      if (variableRegistersStack.nonEmpty) {
        val parentScopeVars = variableRegistersStack.top
        for ((name, (regOrOffset, t)) <- parentScopeVars) {
          if (currentScopeVars.contains(name)) { // Only restore if it was shadowed
            regOrOffset match {
              case Left(reg) =>
                // println(s"Restoring $name into register $reg")
                currentBranch += IRMovReg(reg, reg)
              case Right(off) =>
                // println(s"Restoring $name from stack offset $off")
                val temp = getTempRegister().getOrElse(defTempReg)
                currentBranch += IRLdur(temp, FP, off) // Load back from stack
                push(temp, off)
                //currentBranch += IRStr(temp, FP, Some(off)) // Store it back at correct offset
                freeRegister(temp)
            }
          }
        }
      }
    }
  }
    
  def generateHeadIR(): List[IRInstr] = {
    val dataSection = stringLiterals.map { case (label, value) =>
      wordLabel(value.length, label, value)
    }.flatten.toList

    List(IRLabel(".data")) ++ dataSection ++ List(IRAlign(alignmentOffset), IRLabel(".text"), IRGlobal("main"))
  }

  def generateHelperIRs(): List[IRInstr] = {
    helpers.values.toList.flatten
  }

  def stringToHex(input: String): String = {
    val hexString = input.map(_.toInt.toHexString).mkString
    val trimmedHex = hexString.takeRight(8)
    "0x" + ("0" * (8 - trimmedHex.length)) + trimmedHex
  }

  def genAndPrintExpr(expr: Expr): Unit = {
    val exprType = generateExpr(expr)
    
    if (exprType == BaseType.IntType) {
    helpers.getOrElseUpdate(IRLabel("_printi"), printi())
      currentBranch +=  IRBl("_printi")
    } else if (exprType == BaseType.CharType) {
      helpers.getOrElseUpdate(IRLabel("_printc"), printc())
      currentBranch +=  IRBl("_printc")
    } else if (exprType == BaseType.StrType || exprType == ArrayType(BaseType.CharType)) {
      helpers.getOrElseUpdate(IRLabel("_prints"), prints())
      currentBranch +=  IRBl("_prints")
    } else if (exprType == BaseType.BoolType) {
      helpers.getOrElseUpdate(IRLabel("_printb"), printb())
      currentBranch +=  IRBl("_printb")
    } else if (exprType == ArrayType(BaseType.IntType)) {
      helpers.getOrElseUpdate(IRLabel("_printp"), printp()) //printp
      currentBranch +=  IRBl("_printp")
    } else if (exprType.isInstanceOf[PairType]) {
      helpers.getOrElseUpdate(IRLabel("_printp"), printp())
      currentBranch +=  IRBl("_printp")
    }
  }

  def generateStmt(stmt: Stmt): Unit = stmt match {
      case SkipStmt =>

      // All declared variables are initialised at the start from the symbol table
      case DeclAssignStmt(t, name, value) =>
        // constants.get(name) match {
        //   case Some(_) =>
        //   case None =>
            addVariable(name, t) match {
              case Some(Left(reg)) => 
                generateRValue(value, reg, Some(t))
              case Some(Right(off)) => 
                val temp = getTempRegister().getOrElse(defTempReg)
                generateRValue(value, temp, Some(t))
                push(temp, off)
                freeRegister(temp)
              case _ =>
              }
            //}
        // }
            
      case AssignStmt(lvalue, rvalue) => 
        lvalue match {
          case LValue.LName(name) => 
            lookupVariable(name) match {
              case Some((Left(reg), t)) =>
                generateRValue(rvalue, reg, Some(t))
                // function parameter update push to stack
                if (paramsMap.contains(name)) {
                  currentBranch ++= List(
                    IRCmt(s"push {$reg}"),
                    pushReg(reg, XZR),
                    IRMovReg(paramsReg, SP)
                  )
                }
              case Some((Right(off), t)) =>
                val temp = getTempRegister().getOrElse(defTempReg)
                generateRValue(rvalue, temp, Some(t))
                push(temp, off)
                if (paramsMap.contains(name)) {
                  currentBranch ++= List(
                    IRCmt(s"push {$temp}"),
                    pushReg(temp, XZR),
                    IRMovReg(paramsReg, SP)
                  )
                }
                freeRegister(temp)
              case None =>
                variableOffsets.get(name) match {
                  case Some((off, t)) =>
                    val temp = getTempRegister().getOrElse(defTempReg)
                    generateRValue(rvalue, temp, Some(t))
                    push(temp, off)
                    if (paramsMap.contains(name)) {
                      currentBranch ++= List(
                        IRCmt(s"push {$temp}"),
                        pushReg(temp, XZR),
                        IRMovReg(paramsReg, SP)
                      )
                    }
                    freeRegister(temp)
                  case None => 
                    // This should not be reached
                    throw new Exception(s"Variable $name used before declaration")
                }
            }
          case LValue.LArray(ArrayElem(name, indices)) =>
            lookupVariable(name) match {
              case (Some(Left(baseReg), arrType)) =>
                generateExpr(indices.head, indexReg) // Get index value
                val varReg = getTempRegister().getOrElse(defArrPairReg)
                val elemType = arrType
                generateRValue(rvalue, varReg, Some(elemType))
                currentBranch += IRMovReg(X7, baseReg)
                if (elemType == ArrayType(BaseType.CharType)) {
                  currentBranch += IRBl("_arrStore1")
                  helpers.getOrElseUpdate(IRLabel("_errOutOfBounds"), errOutOfBounds())
                  helpers.getOrElseUpdate(IRLabel("_arrStore1"), arrStore1(varReg.asW))
                } else if (elemType == ArrayType(ArrayType(BaseType.IntType))) {
                  currentBranch += IRBl("_arrStore8")
                  helpers.getOrElseUpdate(IRLabel("_arrStore8"), arrStore8(varReg.asX))
                }
                else {
                  currentBranch += IRBl("_arrStore4")
                  helpers.getOrElseUpdate(IRLabel("_arrStore4"), arrStore4(varReg.asW))
                  
                }
              case _ =>
            }

          case LValue.LPair(pairElem) =>
            val temp = getTempRegister().getOrElse(defArrPairReg)
            generateRValue(rvalue, temp, Some(retrievePairType(pairElem))) 
            generateLPair(pairElem, temp, true)
            freeRegister(temp)
        }

      case ReadStmt(lvalue) => 
        lvalue match {
          case LValue.LName(name) => 
            lookupVariable(name).get match {
              case (Left(regX), t) =>
                val reg = regX.asW
                t match {
                  case BaseType.IntType => 
                    helpers.getOrElseUpdate(IRLabel("_readi"), readi())
                    currentBranch ++= List(IRMovReg(tempIOReg, reg), IRBl("_readi"), IRMovReg(reg, tempIOReg))
                  case BaseType.CharType => 
                    helpers.getOrElseUpdate(IRLabel("_readc"), readc())
                    currentBranch ++= List(IRMovReg(tempIOReg, reg), IRBl("_readc"), IRMovReg(reg, tempIOReg))
                  case _ => List()
                }
              case (Right(off), t) =>
                val reg = getTempRegister().getOrElse(defTempReg)
                t match {
                  case BaseType.IntType => 
                    helpers.getOrElseUpdate(IRLabel("_readi"), readi())
                    currentBranch ++= List(IRMovReg(tempIOReg, reg), IRBl("_readi"), IRMovReg(reg, tempIOReg))
                  case BaseType.CharType => 
                    helpers.getOrElseUpdate(IRLabel("_readc"), readc())
                    currentBranch ++= List(IRMovReg(tempIOReg, reg), IRBl("_readc"), IRMovReg(reg, tempIOReg))
                  case _ => List()
                
                }
                push(reg, off)
                freeRegister(reg)
              case _ =>
            }

          case LValue.LPair(PairElem.FstElem(LValue.LName(name))) =>
            lookupVariable(name).get match {
              case (Left(reg), t) =>
                nullErrorCheck(reg)
                currentBranch += IRLdr(defArrPairReg, reg)
                checkPairType(t, true) match {
                  case BaseType.IntType =>
                    helpers.getOrElseUpdate(IRLabel("_readi"), readi())
                    currentBranch += IRBl("_readi")
                  case BaseType.CharType =>
                    helpers.getOrElseUpdate(IRLabel("_readc"), readc())
                    currentBranch += IRBl("_readi")
                  case _ => List()
                }
                currentBranch ++= List(
                  IRMovReg(arrPairStrReg.asW, tempIOReg),
                  IRStr(arrPairStrReg.asW, reg)
                )
              case _ =>
            }
            
          case LValue.LPair(PairElem.SndElem(LValue.LName(name))) =>
            lookupVariable(name).get match {
              case (Left(reg), t) =>
                nullErrorCheck(reg)
                currentBranch += IRLdr(defArrPairReg, reg, Some(8))
                checkPairType(t, false) match {
                  case BaseType.IntType =>
                    helpers.getOrElseUpdate(IRLabel("_readi"), readi())
                    currentBranch += IRBl("_readi")
                  case BaseType.CharType =>
                    helpers.getOrElseUpdate(IRLabel("_readc"), readc())
                    currentBranch += IRBl("_readi")
                  case _ => List()
                }
                currentBranch ++= List(
                  IRMovReg(arrPairStrReg.asW, tempIOReg),
                  IRStr(arrPairStrReg.asW, reg, Some(8))
                )
              case _ =>
            }
            
            
          case _ => List()
        }

      case FreeStmt(expr) => 
        expr match {
          case (Identifier(name)) => constants.get(name) match {
            case Some(_) =>
            case None => lookupVariable(name).get match {
              case (Left(reg), t) =>
                t match {
                case ArrayType(_) => currentBranch += IRSubImm(defArrPairReg, reg, stackOffset) += IRBl("free")
                case PairType(_, _)=> 
                  currentBranch += IRMovReg(defArrPairReg, reg) += IRBl("_freepair")
                  helpers.getOrElseUpdate(IRLabel("_freepair"), freepair())
                  helpers.getOrElseUpdate(IRLabel("_errNull"), errNull())
                  helpers.getOrElseUpdate(IRLabel("_prints"), prints())
                case _ => throw new Exception(s"Unsupported type for freeing: $t")
                }

              case _ =>
            }
          }
        }

      case PrintStmt(expr) =>
        expr match {
          case Identifier(name) if (lookupVariable(name).isDefined) => 
            val params = paramsMap.values.map(_._1).toList
            lookupVariable(name) match {
              case Some((Left(reg), _)) =>
                if (params.length == 1) {
                  currentBranch ++= List(
                    IRCmt(s"pop/peek {$reg}"),
                    IRLdur(reg, SP, 0),
                    IRMovReg(paramsReg, SP)
                  )
                } else {
                  currentBranch ++= popFunctionParams(params) ++ List(IRMovReg(X16, SP))
                }
                genAndPrintExpr(expr)

              case Some((Right(off), _)) =>
                val temp = getTempRegister().getOrElse(defTempReg)
                if (params.length == 1) {
                  currentBranch ++= List(
                    IRCmt(s"pop/peek from stack offset $off"),
                    IRLdur(temp, FP, off),   
                    IRMovReg(W0, temp.asW)
                  )
                } else {
                  currentBranch ++= popFunctionParams(params) ++ List(IRMovReg(X16, SP))
                }

                freeRegister(temp)
                genAndPrintExpr(expr)
              case _ =>
            }

          case Identifier(name) if (constants.get(name) != None) => constants.get(name) match {
            case Some((ArrayType(_), _)) =>
              generateExpr(StrLiteral(stringToHex(name)))
              helpers.getOrElseUpdate(IRLabel("_prints"), prints())
              currentBranch +=  IRBl("_prints")

            case _ => genAndPrintExpr(expr)
          }
          case _ => genAndPrintExpr(expr)
            None
        }

      case PrintlnStmt(expr) => 
        helpers.getOrElseUpdate(IRLabel("_println"), printlnFunc())
        generateStmt(PrintStmt(expr))
        currentBranch += IRBl("_println")

        expr match {
          case Identifier(name) if paramsMap.contains(name) =>
            val (reg, t) = paramsMap(name)

            // function parameter pop
            poppedParams += name
            currentBranch ++= List(
              IRCmt(s"pop {$reg}"),
              popReg(reg, XZR)
            )
          case _ =>
            None
        }


      case ReturnStmt(expr) => 
        val paramPopInstrs = popFunctionParams(
          paramsMap.view.filterKeys(!poppedParams.contains(_)).values.map(_._1).toList
        )
        generateExpr(expr, W0)
        currentBranch ++= (
          List(
            IRCmt("Function epilogue: reset stack pointer"),
            IRMovReg(SP, FP)  // Reset stack pointer before returning
          ) ++ paramPopInstrs ++ List(
            popReg(FP, LR),
            IRRet()
          )
        )

      case ExitStmt(expr) =>
        generateExpr(expr)
        currentBranch += IRBl("exit")

      case IfStmt(cond, thenStmt, elseStmt) => evalConstants(cond) match {
        case Some(true) => 
          enterScope()
          generateStmt(thenStmt)
          exitScope()
        case Some(false) =>
          enterScope()
          generateStmt(elseStmt)
          exitScope()
        case _ => 
          val temp = getTempRegister().getOrElse(defTempReg)
          generateExpr(cond, temp) // load result in temp register
          /*cond match {
              case BinaryOp(lhs, op, rhs) =>
                currentBranch += IRJumpCond(condToIR(op), branchLabel(1))
              case _ =>
                currentBranch += IRCmpImm(temp.asW, trueValue) += IRJumpCond(EQ, branchLabel(1))
            }*/
          currentBranch += IRCmpImm(temp.asW, trueValue) += IRJumpCond(EQ, branchLabel(1)) // if true, jump to next branch
          freeRegister(temp)
          enterScope()
          generateStmt(elseStmt) // else, continue
          exitScope()
          currentBranch += IRJump(branchLabel(2)) 
          addBranch()
          enterScope()
          generateStmt(thenStmt)
          exitScope()
          addBranch()
        }

      case WhileStmt(cond, body) => evalConstants(cond) match {
        case Some(false) => 
        case _ => 
          val initialBranch = branchLabel(0)
          val bodyBranch = branchLabel(1)
          val condBranch = branchLabel(2)
          currentBranch += IRJump(condBranch) // jump to condition check
          addBranch()
          enterScope()
          generateStmt(body)
          //freeRegister(iReg)
          exitScope()
          addBranch()
          val temp = getTempRegister().getOrElse(defTempReg)
          generateExpr(cond, temp) // if condition true, jump to body
          /*cond match {
            case BinaryOp(lhs, op, rhs) =>
              currentBranch += IRJumpCond(condToIR(op), bodyBranch)
            case _ =>
              currentBranch += IRCmpImm(temp.asW, trueValue) += IRJumpCond(EQ, bodyBranch)
          }*/
          currentBranch += IRCmpImm(temp.asW, trueValue) += IRJumpCond(EQ, bodyBranch)
          if (condBranch != branchLabel(0)) then {
            overwriteJump(initialBranch, branchLabel(0))
          }
          freeRegister(temp)
      }

      case BodyStmt(body) => 
        enterScope()
        generateStmt(body)
        exitScope()

      case SeqStmt(left, right) => generateStmt(left)
                                   generateStmt(right)
  }

  def condToIR(op: BinaryOperator): Condition = {
    op match {
      case BinaryOperator.Equal => EQ
      case BinaryOperator.NotEqual => NE
      case BinaryOperator.Greater => GT
      case BinaryOperator.Less => LT
      case BinaryOperator.GreaterEqual => GE
      case BinaryOperator.LessEqual => LE
      case _ => throw new Exception(s"Not a valid condition type.")
    }
  }

  def genOverflow() = 
    helpers.getOrElseUpdate(IRLabel("_prints"), prints())
    helpers.getOrElseUpdate(IRLabel("_errOverflow"), errOverflow())

  def genDivZero() =
    helpers.getOrElseUpdate(IRLabel("_prints"), prints())
    helpers.getOrElseUpdate(IRLabel("_errDivZero"), errDivZero())

  def extractInt(expr1: Expr, expr2: Expr, isAdd: Boolean): Option[(Expr, Int)] = 
    def valid(value: Int): Boolean = (value >= -4096 && value <= 4095 && isAdd) || (value >= 0 && value <= 4095 && !isAdd)

    (expr1, expr2) match {
      case (_, IntLiteral(value)) if valid(value.toInt) => Some((expr1, value.toInt))
      case (IntLiteral(value), _) if (valid(value.toInt) && isAdd) => Some((expr2, value.toInt))
      case _ => None
    }

  def generateExpr(expr: Expr, dest: Register = defReturnReg): Type = {
    val destX = dest.asX
    val destW = dest.asW
    evalConstants(expr) match {
      case Some(i: Int) => 
        currentBranch += IRMov(destW, i)
        BaseType.IntType
      case Some(c: Char) => 
        currentBranch += IRMov(destW, c.toInt)
        BaseType.CharType
      case Some(b: Boolean) =>
        if (b) {
          currentBranch += IRMov(destW, trueValue)
        } else {
          currentBranch += IRMov(destW, falseValue)
        }
        BaseType.BoolType
      case _ =>
        expr match {
          case IntLiteral(value) =>
            if (value.abs <= max16BitUnsigned || value <= min32BitSigned) {
              currentBranch += IRMov(destW, value.toInt)
              BaseType.IntType
            } else {
              val lower16 = (value & lower16Mask).toInt          // Extract lower 16 bits
              val upper16 = ((value >> upper16Shift) & lower16Mask).toInt  }

          case StrLiteral(value) =>
            val label = nextLabel()
            val formatVal = escapeInnerQuotes(value)
            stringLiterals.getOrElseUpdate(label, formatVal) // Store string in .data
            currentBranch += IRAdrp(destX, label) += IRAddImm(destX, destX, s":lo12:$label")
            BaseType.StrType

          // move the identifier into the destination register
          case Identifier(name) =>
            /*constants.get(name) match {
              case Some((BaseType.IntType, value: Int)) => 
                currentBranch += IRMov(destW, value)
                BaseType.IntType
              case Some((BaseType.BoolType, value: Int)) => 
                currentBranch += IRMov(destW, value)
                BaseType.BoolType
              case Some((BaseType.CharType, value: Int)) => 
                currentBranch += IRMov(destW, value)
                BaseType.CharType
              case _ => */
            lookupVariable(name) match {
              case Some((Left(reg), t)) => 
                if (destW != reg.asW) {
                  t match {
                    //case ArrayType(BaseType.CharType) => currentBranch += IRStr(reg, X16)
                    case ArrayType(_) => currentBranch += IRMovReg(destX, reg.asX)
                    case PairType(_,_) => currentBranch += IRMovReg(destX, reg.asX)
                    case BaseType.StrType => currentBranch += IRMovReg(destX, reg.asX)
                    case _ => currentBranch += IRMovReg(destW, reg.asW)
                  }
                }
                t
              case Some((Right(off),t)) =>
                val temp = getTempRegister().getOrElse(defTempReg)
                currentBranch += IRLdr(temp, FP, Some(off))
                if (destW != temp.asW) {
                  t match {
                    //case ArrayType(BaseType.CharType) => currentBranch += IRStr(reg, X16)
                    case ArrayType(_) => currentBranch += IRMovReg(destX, temp.asX)
                    case PairType(_,_) => currentBranch += IRMovReg(destX, temp.asX)
                    case BaseType.StrType => currentBranch += IRMovReg(destX, temp.asX)
                    case _ => currentBranch += IRMovReg(destW, temp.asW)
                  }
                }
                freeRegister(temp)
                t
              case _ => NullType
            }
        //}
        // compare if the dest and src are the same value or not to reduce redundancy

              currentBranch +=
                IRMov(destW, lower16) +=        
                IRMovk(destW, upper16, upper16Shift)   

              BaseType.IntType
            }
            


          case BoolLiteral(value) =>
            currentBranch += IRMov(destW, if (value) trueValue else falseValue)
            BaseType.BoolType

          case CharLiteral(value) =>
            currentBranch += IRMov(destW, value.toInt)
            BaseType.CharType

          case StrLiteral(value) =>
            val label = nextLabel()
            val formatVal = escapeInnerQuotes(value)
            stringLiterals.getOrElseUpdate(label, formatVal) // Store string in .data
            currentBranch += IRAdrp(destX, label) += IRAddImm(destX, destX, s":lo12:$label")
            BaseType.StrType

          // move the identifier into the destination register
          case Identifier(name) =>
            constants.get(name) match {
              case Some((BaseType.IntType, value: Int)) => 
                currentBranch += IRMov(destW, value)
                BaseType.IntType
              case Some((BaseType.BoolType, value: Int)) => 
                currentBranch += IRMov(destW, value)
                BaseType.BoolType
              case Some((BaseType.CharType, value: Int)) => 
                currentBranch += IRMov(destW, value)
                BaseType.CharType
              case _ =>
                lookupVariable(name) match {
                  case Some((Left(reg), t)) => 
                    if (destW != reg.asW) {
                      t match {
                        //case ArrayType(BaseType.CharType) => currentBranch += IRStr(reg, X16)
                        case ArrayType(_) => currentBranch += IRMovReg(destX, reg.asX)
                        case PairType(_,_) => currentBranch += IRMovReg(destX, reg.asX)
                        case BaseType.StrType => currentBranch += IRMovReg(destX, reg.asX)
                        case _ => currentBranch += IRMovReg(destW, reg.asW)
                      }
                    }
                    t
                  case Some((Right(off),t)) =>
                    val temp = getTempRegister().getOrElse(defTempReg)
                    currentBranch += IRLdr(temp, FP, Some(off))
                    if (destW != temp.asW) {
                      t match {
                        //case ArrayType(BaseType.CharType) => currentBranch += IRStr(reg, X16)
                        case ArrayType(_) => currentBranch += IRMovReg(destX, temp.asX)
                        case PairType(_,_) => currentBranch += IRMovReg(destX, temp.asX)
                        case BaseType.StrType => currentBranch += IRMovReg(destX, temp.asX)
                        case _ => currentBranch += IRMovReg(destW, temp.asW)
                      }
                    }
                    freeRegister(temp)
                    t
                  case _ => NullType
                }
            }
            // compare if the dest and src are the same value or not to reduce redundancy

          case PairLiteral =>
            currentBranch += IRMov(destX, defPairNullValue) // 0 is the null value
            PairType(NullType, NullType)
          
          case UnaryOp(op, expr) => 
            val srcRegX = getTempRegister().getOrElse(defTempReg)
            val srcRegW = srcRegX.asW
            generateExpr(expr, srcRegX)
            val unaryType = op match {
              case UnaryOperator.Negate =>
                currentBranch += IRNeg(destW, srcRegW) += IRJumpCond(VS, "_errOverflow")
                genOverflow()
                BaseType.IntType 
              case UnaryOperator.Not =>
                currentBranch += IRCmpImm(srcRegW, trueValue) += IRCset(destW, NE)
                BaseType.BoolType
              case UnaryOperator.Length =>
                expr match {
                    case Identifier(name) =>
                      constants.get(name) match {
                        case Some((_, arr : List[Any])) => currentBranch += IRMov(destW, arr.length)
                        case _ =>
                      }
                      lookupVariable(name) match {
                        case Some((Left(reg), _)) => currentBranch += IRLdur(destW, reg, -stackOffset)
                        case _ =>
                      }
                    case ArrayElem(name, indices) => constants.get(name) match {
                      case Some((_, arr: List[Any])) => getArrayItem(arr, indices) match {
                        case Some(list: List[Any]) => currentBranch += IRMov(destW, list.length)
                        case _ =>
                      }
                      case _ =>
                    }
                    case _ =>
                }
                BaseType.IntType

                

              case UnaryOperator.Ord => 
                expr match {
                  case CharLiteral(value) =>
                    currentBranch += IRMov(destW, value.toInt) // Convert char to integer
                  case Identifier(name) => 
                    constants.get(name) match {
                      case Some((_,value: Int)) => currentBranch += IRMov(destW, value)
                      case _ =>
                        lookupVariable(name) match {
                          case Some((Left(r), _)) => currentBranch += IRMovReg(destW, r.asW)
                          case _ =>
                        }
                    }
                }
                BaseType.IntType

              case UnaryOperator.Chr =>
                helpers.getOrElseUpdate(IRLabel("_errBadChar"), errBadChar())
                currentBranch += IRTst(srcRegW, min8BitSigned)     // Test if value is within ASCII range (0-127)
                        += IRCsel(chrRangeCheckReg, defChrReg, chrRangeCheckReg, NE) // Conditional move if out of range
                        += IRJumpCond(NE , "_errBadChar") // Branch if invalid
                        += IRMovReg(destW, srcRegW)          // Move the value into W0 (truncate to char)
                BaseType.CharType
                  
            }
            freeRegister(srcRegX)
            unaryType

          // DO REGISTERS AS PARAMETER  
          
          case BinaryOp(expr1, op, expr2) =>
            if (op == BinaryOperator.Add || op == BinaryOperator.Subtract || op == BinaryOperator.Multiply) then {
              genOverflow()
            } else {
              genDivZero()
            }
            // 📌 Helpers for comparisons:
            def compareFunc(cond:Condition): Type = {
              val (wreg1, wreg2) = genExprs(expr1, expr2, false)
              val temp = getTempRegister().getOrElse(defTempReg)
              currentBranch += IRCmp(wreg1, wreg2) += IRCset(temp.asW, cond) += IRMovReg(destW, temp.asW)
              freeRegister(temp)
              freeRegister(wreg1.asX)
              freeRegister(wreg2.asX)
              BaseType.BoolType
            }
            // Helper to generate both sides of a binary operator
            def genExprs(expr1: Expr, expr2: Expr, useTemp: Boolean): (Register, Register) = {
              val xreg1 = if (useTemp) then getTempRegister().getOrElse(defTempReg) else getRegister().getOrElse(defNormReg)
              val wreg1 = xreg1.asW        
              generateExpr(expr1, xreg1)
              val xreg2 = if (useTemp) then getTempRegister().getOrElse(defTempReg) else getRegister().getOrElse(defNormReg)
              val wreg2 = xreg2.asW 
              generateExpr(expr2, xreg2)
              (wreg1, wreg2)
            }
            val binaryInstrs = op match {
              case BinaryOperator.Add => 
                extractInt(expr1, expr2, true) match {
                  case Some((expr, value)) =>
                    generateExpr(expr, dest)
                    currentBranch += IRAddsImm(destW, destW, value) += IRJumpCond(VS, "_errOverflow")
                  case _ =>
                    val (wreg1, wreg2) = genExprs(expr1, expr2, true)
                    currentBranch += IRAdds(destW, wreg1, wreg2) += IRJumpCond(VS, "_errOverflow")
                    freeRegister(wreg1.asX)
                    freeRegister(wreg2.asX)
                }
                BaseType.IntType
              
              case BinaryOperator.Subtract =>
                extractInt(expr1, expr2, false) match {
                  case Some((expr, value)) => 
                    generateExpr(expr, dest)
                    currentBranch += IRSubImm(destW, destW, value) += IRJumpCond(VS, "_errOverflow")
                  case _ =>
                    val (wreg1, wreg2) = genExprs(expr1, expr2, true)
                    currentBranch += IRSub(destW, wreg1, wreg2) += IRJumpCond(VS, "_errOverflow")
                    freeRegister(wreg1.asX)
                    freeRegister(wreg2.asX)
                }
                BaseType.IntType

              case BinaryOperator.Multiply =>
                val (wreg1, wreg2) = genExprs(expr1, expr2, true)
                currentBranch += IRSMull(destX, wreg1, wreg2) += IRCmpExt(destX, destW) += IRJumpCond(NE, "_errOverflow")
                freeRegister(wreg1.asX)
                freeRegister(wreg2.asX)
                BaseType.IntType


              case BinaryOperator.Divide => 
                val (wreg1, wreg2) = genExprs(expr1, expr2, true)
                currentBranch += IRCmpImm(wreg2, falseValue) += IRJumpCond(EQ, "_errDivZero") += IRSDiv(destW, wreg1, wreg2)
                freeRegister(wreg1.asX)
                freeRegister(wreg2.asX)
                BaseType.IntType

              case BinaryOperator.Modulus => 
                val (wreg1, wreg2) = genExprs(expr1, expr2, true)
                currentBranch += IRCmpImm(wreg2, falseValue) += IRJumpCond(EQ, "_errDivZero")
                currentBranch += IRSDiv(modTempReg, wreg1, wreg2) += IRMSub(destW, modTempReg, wreg2, wreg1)
                freeRegister(wreg1.asX)
                freeRegister(wreg2.asX)
                BaseType.IntType

              case BinaryOperator.Greater =>
                compareFunc(GT)
              case BinaryOperator.GreaterEqual =>
                compareFunc(GE)
              case BinaryOperator.Less =>
                compareFunc(LT)
              case BinaryOperator.LessEqual =>
                compareFunc(LE)
              case BinaryOperator.Equal =>
                compareFunc(EQ)
              case BinaryOperator.NotEqual =>
                compareFunc(NE)

              case BinaryOperator.Or =>
                val (wreg1, wreg2) = genExprs(expr1, expr2, false)
                currentBranch += IRCmpImm(wreg1, trueValue) += IRCset(wreg1, EQ)
                currentBranch += IRCmpImm(wreg2, trueValue)  += IRCset(wreg2, EQ) += IROr(destW, wreg1, wreg2)
                freeRegister(wreg1.asX)
                freeRegister(wreg2.asX)
                BaseType.BoolType
              
              case BinaryOperator.And =>
                val (wreg1, wreg2) = genExprs(expr1, expr2, false)
                currentBranch += IRCmpImm(wreg1, trueValue) += IRJumpCond(NE, branchLabel(1)) += IRCmpImm(wreg2, trueValue)
                addBranch()
                currentBranch += IRCset(destW, EQ)
                freeRegister(wreg1.asX)
                freeRegister(wreg2.asX)
                BaseType.BoolType
            }
            binaryInstrs  

          case ArrayElem(name, indices) => 
            constants.get(name) match {
              case Some(arrType, array) => getArrayItem(array, indices) match {
                case Some(n: Int) => currentBranch += IRMov(destW, n)
                case _ =>
              }
                getAccessedArrayType(arrType, indices)
              case None => lookupVariable(name) match {
                case Some((Left(baseReg), arrType)) => // Base address
                  generateExpr(indices.head, indexReg) // Get index value
                  arrType match {
                    case ArrayType(ArrayType(_)) => 
                      helpers.getOrElseUpdate(IRLabel("_arrLoad8"), arrLoad8())
                      helpers.getOrElseUpdate(IRLabel("_errOutOfBounds"), errOutOfBounds())
                      currentBranch += IRMovReg(defArrTempReg, baseReg) += IRBl("_arrLoad8")
                      currentBranch += IRMovReg(defTempReg, defArrTempReg) += IRLdur(defArrPairReg.asW, defTempReg, -stackOffset)
                    case ArrayType(PairType(_,_)) =>
                      helpers.getOrElseUpdate(IRLabel("_arrLoad8"), arrLoad8())
                      helpers.getOrElseUpdate(IRLabel("_errOutOfBounds"), errOutOfBounds())
                      currentBranch += IRMovReg(defArrTempReg, baseReg) 
                      currentBranch += IRBl("_arrLoad8") += IRMovReg(dest, defArrTempReg)

                    case _ =>
                      helpers.getOrElseUpdate(IRLabel("_arrLoad4"), arrLoad4())
                      helpers.getOrElseUpdate(IRLabel("_errOutOfBounds"), errOutOfBounds())
                      currentBranch += IRMovReg(defArrTempReg, baseReg) 
                      currentBranch += IRBl("_arrLoad4") += IRMovReg(destW, defArrTempReg.asW)
                  }
                  if (indices.size > 1) {
                    helpers.getOrElseUpdate(IRLabel("_arrLoad4"), arrLoad4())
                    helpers.getOrElseUpdate(IRLabel("_errOutOfBounds"), errOutOfBounds())
                    currentBranch.remove(currentBranch.size - 1)
                    currentBranch += pushReg(defTempReg, XZR)
                    generateExpr(indices.apply(1), indexReg)
                    currentBranch += popReg(defArrTempReg, XZR)
                    currentBranch += IRBl("_arrLoad4") += IRMovReg(destW, defArrTempReg.asW)
                  }
                  getAccessedArrayType(arrType, indices)
                case _ => BaseType.IntType
              }
            }
          case _ => BaseType.IntType
            
        }
      }
    }
    

  def checkBounds(list: List[Any], i: Int): Boolean = i >= 0 && i < list.length
  
  // Gets an array item from a constant array, if it is valid
  def getArrayItem(arr: Any, indices: List[Expr]): Option[Any] = indices match {
    case Nil => Some(arr)
    case i :: rest => i match {
      case Identifier(name) => constants.get(name) match {
        case Some((BaseType.IntType, index)) => (arr, index) match {
          case (list: List[Any], n: Int) if checkBounds(list, n) => getArrayItem(list(n), rest)
          case _ => 
            helpers.getOrElseUpdate(IRLabel("_errOutOfBounds"), errOutOfBounds())
            currentBranch += IRJump("_errOutOfBounds")
            None
        }
        case Some(_) => None
        case _ => None
      }
      case IntLiteral(n) => arr match {
        case list: List[Any] if checkBounds(list, n.toInt) => getArrayItem(list(n.toInt), rest)
        case _ => 
          helpers.getOrElseUpdate(IRLabel("_errOutOfBounds"), errOutOfBounds())
          currentBranch += IRJump("_errOutOfBounds")
          None
      }
      case ArrayElem(name, is) => constants.get(name) match {
        case Some((_, outerlist: List[Any])) => getArrayItem(outerlist, is) match {
          case Some(n) => (arr, n) match {
            case (list: List[Any], n: Int) if checkBounds(list, n) => getArrayItem(list(n), rest)
            case _ => None
          }
          case _ => None
        }
        case _ => None
      }
    }
  }

  // Evaluates constants for more propagation and optimisation
  def evalConstants(expr: Expr): Option[Any] = expr match {
    case BoolLiteral(value) => Some(value)
    case IntLiteral(value) => Some(value)
    case CharLiteral(value) => Some(value)
    case Identifier(name) => constants.get(name) match {
      case Some((BaseType.BoolType, value: Int)) => Some(value == trueValue)
      case Some((BaseType.CharType, value: Int)) => Some(value.toChar)
      case Some((BaseType.IntType, value: Int)) => Some(value)
      case _ => None
    }
    case UnaryOp(op, innerExpr) => op match {
      case UnaryOperator.Negate => evalConstants(innerExpr) match {
        case Some(value: Int) => Some(-value)
        case _ => None
      }
      case UnaryOperator.Not => evalConstants(innerExpr) match {
        case Some(value: Boolean) => Some(!value)
        case _ => None
      }
      case UnaryOperator.Ord => evalConstants(innerExpr) match {
        case Some(value: Char) => Some(value.toInt)
        case _ => None
      }
      case UnaryOperator.Chr => evalConstants(innerExpr) match {
        case Some(value: Int) => Some(value.toChar)
        case _ => None
      }
      case _ => None
    }
    case BinaryOp(expr1, op, expr2) => op match {
      case BinaryOperator.Add => (evalConstants(expr1), evalConstants(expr2)) match {
        case (Some(i1: Int), Some(i2: Int)) => Some(i1 + i2)
        case _ => None
      }
      case BinaryOperator.Subtract => (evalConstants(expr1), evalConstants(expr2)) match {
        case (Some(i1: Int), Some(i2: Int)) => Some(i1 - i2)
        case _ => None
      }
      case BinaryOperator.Multiply => (evalConstants(expr1), evalConstants(expr2)) match {
        case (Some(i1: Int), Some(i2: Int)) => Some(i1 * i2)
        case _ => None
      }
      case BinaryOperator.Divide => (evalConstants(expr1), evalConstants(expr2)) match {
        case (Some(i1: Int), Some(i2: Int)) => if (i2 != 0) Some(i1 / i2) else None
        case _ => None
      }
      case BinaryOperator.Modulus => (evalConstants(expr1), evalConstants(expr2)) match {
        case (Some(i1: Int), Some(i2: Int)) => if (i2 != 0) Some(i1 % i2) else None
        case _ => None
      }
      case BinaryOperator.Greater => (evalConstants(expr1), evalConstants(expr2)) match {
        case (Some(i1: Int), Some(i2: Int)) => Some(i1 > i2)
        case (Some(c1: Char), Some(c2: Char)) => Some(c1 > c2)
        case _ => None
      }
      case BinaryOperator.GreaterEqual => (evalConstants(expr1), evalConstants(expr2)) match {
        case (Some(i1: Int), Some(i2: Int)) => Some(i1 >= i2)
        case (Some(c1: Char), Some(c2: Char)) => Some(c1 >= c2)
        case _ => None
      }
      case BinaryOperator.Less => (evalConstants(expr1), evalConstants(expr2)) match {
        case (Some(i1: Int), Some(i2: Int)) => Some(i1 < i2)
        case (Some(c1: Char), Some(c2: Char)) => Some(c1 < c2)
        case _ => None
      }
      case BinaryOperator.LessEqual => (evalConstants(expr1), evalConstants(expr2)) match {
        case (Some(i1: Int), Some(i2: Int)) => Some(i1 <= i2)
        case (Some(c1: Char), Some(c2: Char)) => Some(c1 <= c2)
        case _ => None
      }
      case BinaryOperator.Equal => (evalConstants(expr1), evalConstants(expr2)) match {
        case (Some(i1: Int), Some(i2: Int)) => Some(i1 == i2)
        case (Some(c1: Char), Some(c2: Char)) => Some(c1 == c2)
        case (Some(b1: Boolean), Some(b2: Boolean)) => Some(b1 == b2)
        case _ => None
      }
      case BinaryOperator.NotEqual => (evalConstants(expr1), evalConstants(expr2)) match {
        case (Some(i1: Int), Some(i2: Int)) => Some(i1 != i2)
        case (Some(c1: Char), Some(c2: Char)) => Some(c1 != c2)
        case (Some(b1: Boolean), Some(b2: Boolean)) => Some(b1 != b2)
        case _ => None
      }
      case BinaryOperator.And => (evalConstants(expr1), evalConstants(expr2)) match {
        case (Some(b1: Boolean), Some(b2: Boolean)) => Some(b1 && b2)
        case _ => None
      }
      case BinaryOperator.Or => (evalConstants(expr1), evalConstants(expr2)) match {
        case (Some(b1: Boolean), Some(b2: Boolean)) => Some(b1 || b2)
        case _ => None
      }
    }
    case _ => None
  }
  
  def retrievePairType(pair: PairElem): Type =
    pair match {
      case PairElem.FstElem(LValue.LName(p)) => 
        val (_, t) = lookupVariable(p).get
        checkPairType(t, isFst = true)
      case PairElem.SndElem(LValue.LName(p)) => 
        val (_, t) = lookupVariable(p).get
        checkPairType(t, isFst = false)
      case PairElem.FstElem(LValue.LArray(ArrayElem(name, _))) =>
        val (_, t) = lookupVariable(name).get
        t match {
          case ArrayType(inner) => inner
          case _ => throw new Exception(s"Variable $name is not an array")
        }
      case PairElem.SndElem(LValue.LArray(ArrayElem(name, _))) =>
        val (_, t) = lookupVariable(name).get
        t match {
          case ArrayType(inner) => inner
          case _ => throw new Exception(s"Variable $name is not an array")
        }
      case PairElem.FstElem(LValue.LPair(innerPair)) =>
        retrievePairType(innerPair)
      case PairElem.SndElem(LValue.LPair(innerPair)) =>
        retrievePairType(innerPair)
    }

  def generateLPair(pair: PairElem, dest: Register, isStr: Boolean, isFirst: Boolean = true): Type =
    pair match {
      case PairElem.FstElem(LValue.LName(p)) =>
        lookupVariable(p) match {
          case Some((Left(reg), t)) => 
            nullErrorCheck(reg)
            if (isFirst && isStr) then {
              currentBranch += IRStr(dest, reg)
            } else {
              currentBranch += IRLdr(dest, reg)
            }
            t
          case _ => NullType
        }
      case PairElem.SndElem(LValue.LName(p)) => 
        lookupVariable(p) match {
          case Some((Left(reg), t)) => 
            nullErrorCheck(reg)
            if (isFirst && isStr) then {
              currentBranch += IRStr(dest, reg, Some(pointerSize))
            } else {
              currentBranch += IRLdr(dest, reg, Some(pointerSize))
            }
            t
          case _ => NullType
        }
      case PairElem.FstElem(LValue.LPair(innerPair)) => 
        val temp = getTempRegister().getOrElse(defArrPairReg)
        val t = generateLPair(innerPair, temp, isStr, false)
        nullErrorCheck(temp)
        if (isFirst && isStr) then {
          currentBranch += IRStr(dest, temp)
        } else {
          currentBranch += IRLdr(dest, temp)
        }
        freeRegister(temp)
        t
      case PairElem.SndElem(LValue.LPair(innerPair)) => 
        val temp = getTempRegister().getOrElse(defArrPairReg)
        val t = generateLPair(innerPair, temp, isStr, false)
        nullErrorCheck(temp)
        if (isFirst && isStr) then {
          currentBranch += IRStr(dest, temp, Some(pointerSize))
        } else {
          currentBranch += IRLdr(dest, temp, Some(pointerSize))
        }
        freeRegister(temp)
        t

      case PairElem.FstElem(LValue.LArray(arrayElem)) =>
        val tempX = getTempRegister().getOrElse(defArrPairReg)
        val t = generateExpr(arrayElem,  tempX)
        nullErrorCheck(tempX)
        if (isFirst && isStr) then {
          currentBranch += IRStr(dest, tempX)
        } else {
          currentBranch += IRLdr(dest, tempX)
        }
        t

      case PairElem.SndElem(LValue.LArray(arrayElem)) =>
        val tempX = getTempRegister().getOrElse(defArrPairReg)
        val t = generateExpr(arrayElem,  tempX)
        nullErrorCheck(tempX)
        if (isFirst && isStr) then {
          currentBranch += IRStr(dest, tempX, Some(pointerSize))
        } else {
          currentBranch += IRLdr(dest, tempX, Some(pointerSize))
        }    
        t
    }
  

  def generateRValue(rvalue: RValue, reg: Register, exprType: Option[Type] = None): Unit = {
    rvalue match {
      case RValue.RExpr(expr) => generateExpr(expr, reg) 

      case RValue.RArrayLiter(arrayLiter) => 
        val elementsIR = arrayLiter.elements.getOrElse(List()) // list of elements
        val size = elementsIR.size // number of elements 
        val arrayMemory = arrayMemorySize(size, exprType.get)
        val tempX = getTempRegister().getOrElse(defArrPairReg)
        val temp = tempX.asW

        currentBranch += IRMov(defArrPairReg.asW, arrayMemory) += IRBl("_malloc")
        += IRMovReg(arrPairStrReg, defArrPairReg) += IRAddImmInt(arrPairStrReg, arrPairStrReg, stackOffset)
        += IRMov(temp, size) += IRStur(temp, arrPairStrReg, -stackOffset)
        
        for ((element, i) <- elementsIR.zipWithIndex) { // iterate over each expr 
          val elType = generateExpr(element, temp)
          elType match {
            case BaseType.IntType => 
              if (i == firstIndex) { // separate case for first element
                currentBranch += IRStr(temp, arrPairStrReg)
              } else {
              currentBranch += IRStr(temp, arrPairStrReg, Some(i * intSize)) // Store element
              }
            case BaseType.CharType => 
              if (i == firstIndex) { 
                currentBranch += IRStrb(temp, arrPairStrReg)
              } else {
              currentBranch += IRStrb(temp, arrPairStrReg, Some(i)) 
              }
            case BaseType.BoolType => 
              if (i == firstIndex) { 
                currentBranch += IRStrb(temp, arrPairStrReg)
              } else {
                currentBranch += IRStrb(temp, arrPairStrReg, Some(i)) 
              }
            case BaseType.StrType => 
              if (i == firstIndex) { 
                currentBranch += IRStrb(temp, arrPairStrReg)
              } else {
                currentBranch += IRStrb(temp, arrPairStrReg, Some(i)) 
              }
            case PairType(_,_) =>
              currentBranch.remove(currentBranch.length - 1)
              element match {
                case Identifier(name) =>
                  lookupVariable(name) match {
                    case Some((Left(elemReg), _)) =>
                      if (i == firstIndex) { // separate case for first element
                        currentBranch += IRStr(elemReg, arrPairStrReg)
                      } else {
                        currentBranch += IRStr(elemReg, arrPairStrReg, Some(i * pointerSize)) 
                      }
                    case _ =>
                  }
              }
            case ArrayType(inner) =>
              currentBranch.remove(currentBranch.length - 1)
              element match {
                case Identifier(name) =>
                  lookupVariable(name) match {
                    case Some((Left(elemReg), _)) =>
                      if (i == 0) { // separate case for first element
                        currentBranch += IRStr(elemReg, arrPairStrReg)
                      } else {
                        currentBranch += IRStr(elemReg, arrPairStrReg, Some(i * pointerSize)) 
                      }
                    case _ =>
                  }
              }
              
            case _ =>
          }
        }
        freeRegister(tempX)
        currentBranch += IRMovReg(reg, arrPairStrReg) 
        helpers.getOrElseUpdate(IRLabel("_prints"), prints())
        helpers.getOrElseUpdate(IRLabel("_malloc"), malloc())
        helpers.getOrElseUpdate(IRLabel("_errOutOfMemory"), errOutOfMemory())
        

      case RValue.RNewPair(fst, snd) => 
        def addPairElem(expr: Expr, offset: Option[Int]) = {
          expr match {
            case Identifier(name) => 
              lookupVariable(name) match {
                case Some((Left(reg), t)) => currentBranch += IRStr(reg, arrPairStrReg, offset)
                case _ =>
              }
            case _ => 
              val temp = getTempRegister().getOrElse(defArrPairReg)
              generateExpr(expr, temp)
              currentBranch += IRStr(temp, arrPairStrReg, offset)
              freeRegister(temp)
          }
        }
       
        currentBranch += IRMov(defArrPairReg.asW, pairMemorySize)
        += IRBl("_malloc")+= IRMovReg(arrPairStrReg, defArrPairReg)
        addPairElem(fst, None)
        addPairElem(snd, Some(pointerSize))
        currentBranch += IRMovReg(reg, arrPairStrReg) // move pair memory to destination register

        helpers.getOrElseUpdate(IRLabel("_prints"), prints())
        helpers.getOrElseUpdate(IRLabel("_malloc"), malloc())
        helpers.getOrElseUpdate(IRLabel("_errOutOfMemory"), errOutOfMemory())

      case RValue.RPair(pairElem) => 
        generateLPair(pairElem, reg, false)

      case RValue.RCall(name, Some(args)) => 
        val paramRegs = argumentRegisters
        args.zip(paramRegs).foreach { case (arg, reg) =>
          generateExpr(arg, reg) // Move argument values into x8-x15
        }
        currentBranch ++= List(
          IRBl(s"wacc_$name"), 
          IRMovReg(reg.asW, defReturnReg)
        )

      case RValue.RCall(name, None) =>
        currentBranch ++= List(IRBl(s"wacc_$name"), IRMovReg(reg.asW, defReturnReg))    }

    
  }

  def elementSize(expType: Type): Int = {
    expType match {
      case BaseType.IntType => intSize // Integers are 4 bytes
      case BaseType.CharType => charSize // Chars are 1 byte
      case BaseType.BoolType => boolSize // Bools are 1 byte
      case BaseType.StrType => pointerSize // Strings are pointers (8 bytes)
      case ArrayType(t) => elementSize(t)
      case BaseTElem(t) => elementSize(t)
      case ArrayTElem(t) => elementSize(t) // does the pair contain an array or an element of the array?
      case PairType(_,_) => pointerSize
      case _ => throw new IllegalArgumentException(s"Unsupported element type: $expType")
    }
  }

  def arrayMemorySize(size: Int, expType: Type): Int = {
    expType match {
      case PairKeyword => arrayMetadataSize + size * pointerSize // Pairs are stored as pointers
      case ArrayType(ArrayType(t)) => arrayMetadataSize + size * pointerSize // Nested arrays use pointers
      case ArrayType(t)  => arrayMetadataSize + (size * elementSize(t))  // Use elementSize function for flexibility
      case BaseType.StrType  => arrayMetadataSize + size * charSize // Strings are arrays of chars
      case _ => throw new IllegalArgumentException(s"Unsupported array type: $expType")
    }
  }

  def checkPairType(pairType: Type, isFst: Boolean): Type = {
    def toBaseType(t: Type): Type = t match {
      case BaseTElem(t) => t
      case ArrayTElem(t) => t
      case _ => t
    }
    pairType match {
      case PairType(fst, snd) => if (isFst) then toBaseType(fst) else toBaseType(snd)
      case _ => pairType
    }
  }



  def generateArrayElem(arrayElem: ArrayElem): List[IRInstr] = List()
  

  def generatePairElem(pairElem: PairElem): List[IRInstr] = List()
  
  def escapeInnerQuotes(str: String): String = {
    if (str.startsWith("\"") && str.endsWith("\"")) {
      "\"" + str.drop(1).dropRight(1).replace("\"", "\\\"") + "\""
    } else {
      str.replace("\"", "\\\"") // Just escape quotes if no surrounding quotes
    }
  }

  def nullErrorCheck(reg: Register): Unit = {
    helpers.getOrElseUpdate(IRLabel("_prints"), prints())
    helpers.getOrElseUpdate(IRLabel("_errNull"), errNull())
    currentBranch += IRCmpImm(reg, falseValue) += IRJumpCond(EQ, "_errNull")
  }

  def getAccessedArrayType(arrType: Type, indices: List[Expr]): Type = {
  indices match {
    case Nil => arrType  // No indices, return the original type
    case _ :: rest => arrType match {
      case ArrayType(inner) => getAccessedArrayType(inner, rest) // Recursively go deeper
      case _ => throw new Exception("Too many indices for type")
    }
  }
}

}
