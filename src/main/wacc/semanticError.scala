// import scala.math._
// import scala.io._


// trait Error {
//     val errorType: String // Type of the error
//     val log: String // Log message describing the error
//     val exitStatus: Int // Exit status associated with the error
//     val line: Int // Line number where the error occurred
//     val column: Int // Column number where the error occurred

//     def printErrorMessage(): Unit = {
//         println(s"$errorType at line $line, column $column:")
//         println(log)
//         printSurroundingLines(line)
//     }

//     def printSurroundingLines(lineNum: Int): Unit = {
//         val file = ???
//         val source = Source.fromFile(file)
//         try {
//             val lines = source.getLines().toSeq
//             val startLine = max(1, lineNum - 1)
//             val endLine = min(lines.length, lineNum + 1)
//             for (i <- startLine to endLine) {
//                 println(s"|${lines(i - 1)}")
//                 if (i == lineNum) {
//                     var space = ""
//                     var j = 0
//                     while (j <= column) {
//                         space += " "
//                         j += 1
//                     }
//                     println(s"|$space^")
//                 }
//             }
//         } finally {
//             source.close()
//         }
//     }
// }