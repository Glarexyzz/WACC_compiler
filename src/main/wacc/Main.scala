package wacc

import Constants._
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
                            println(s"Semantic errors found:\n$errors")
                            exitCode = exitInvalidSemantics
                        case None => 
                            println(s"Successfully parsed:\n$parsed")
                    }
                case Left(error) =>
                    println(s"Parsing failed: $error")
                    exitCode = exitInvalidSyntax
            }
    }
    System.exit(exitCode)
}
