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
                        case Some(errors) => 
                            println(s"Semantic errors found:\n$errors\n")
                            exitCode = exitInvalidSemantics
                        case None => 
                            compile(parsed, "temp")
                            println(s"Successfully parsed & compiled:\n$parsed\n")
                            exitCode = exitValid
                    }
                case Left(error) =>
                    println(s"Syntax Errors found: $error\n")
                    exitCode = exitInvalidSyntax
            }
    }
    System.exit(exitCode)
}
