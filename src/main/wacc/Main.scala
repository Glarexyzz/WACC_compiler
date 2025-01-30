package wacc

import Constants._
import scala.io.Source

def main(args: Array[String]): Unit = {
    var exitCode = exitValid

    args match {
        case Array(input) =>
            val prog = Source.fromFile(input).mkString
            val result = parser.parse(prog) // Call the central parse function
            result match {
                case Right(parsed) =>
                    println(s"Successfully parsed:\n$parsed")
                    exitCode = exitValid
                case Left(error) =>
                    println(s"Parsing failed: $error")
                    exitCode = exitInvalidSyntax
            }
            
    }
    System.exit(exitCode)
}
