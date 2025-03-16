package wacc

import Constants._
import CodeGen._
import scala.io.Source

def main(args: Array[String]): Unit = {
    var exitCode = exitValid

    args match {
        case Array(input) =>
            println(input)
            val prog = Source.fromFile(input).mkString
            val result = parser.parse(prog) // Call the central parse function
            // put a wrapper around parsed to check semantic
            result match {
                case Right(parsed) =>
                    semanticChecker.checkSemantic(parsed) match {
                        case Left(errors) => 
                            println(s"Semantic errors found:\n$errors\n")
                            exitCode = exitInvalidSemantics
                        case Right(symbolTable, constants, unusedVariables) => 
                            //println(s"func table: ${symbolTable.getFunctionTable}")
                            //println(s"vars table: ${symbolTable.getVariableScopes}")
                            parsed match {
                                case program: Program =>
                                    compile(program, input, symbolTable, constants, unusedVariables)
                                    println(s"Successfully parsed & compiled:\n$parsed\n")
                                    exitCode = exitValid

                                case _ =>
                                    println(s"Parsed type unknown error.")
                                    exitCode = exitInvalidSyntax
                            }
                            
                    }
                case Left(error) =>
                    println(s"Syntax Errors found: $error\n")
                    exitCode = exitInvalidSyntax
            }
    }
    System.exit(exitCode)
}
