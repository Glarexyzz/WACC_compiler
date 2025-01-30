package wacc

import Constants._

def main(args: Array[String]): Unit = {
    var exitCode = exitValid

    args match {
        case Array(input) =>
            val result = parser.parse(input) // Call the central parse function
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
