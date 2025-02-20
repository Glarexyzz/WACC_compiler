// import java.io.File
// import scala.sys.process._
// import org.scalatest.flatspec.AnyFlatSpec
// import org.scalatest.matchers.should.Matchers

// class BackendTest extends AnyFlatSpec with Matchers {

//   val testRootAssembly = new File("src/test/wacc/valid-assembly")
//   val testFiles = getListOfSFiles(testRootAssembly)

//   for ((subcat, files) <- testFiles) {
//     subcat should s"execute correctly for subcategory $subcat" in {
//       files.foreach { file =>
//         val executablePath = file.getAbsolutePath.replace(".s", "")

//         // Step 1: Compile `.s` to an executable
//         val compileCommand = s"aarch64-linux-gnu-gcc -o $executablePath ${file.getAbsolutePath}"
//         compileCommand.! shouldBe 0 // ✅ Ensure compilation succeeds

//         // Step 2: Run the executable and capture exit code
//         val exitCode = Process(executablePath).!

//         // Step 3: Extract expected exit code using pattern matching
//         val expectedExitCode = extractExitCode(file.getName)

//         // Step 4: Compare actual vs. expected exit code
//         withClue(s"File ${file.getName} failed exit code check: ") {
//           exitCode shouldBe expectedExitCode
//         }
//       }
//     }
//   }

//   def getListOfSFiles(dir: File): Map[String, List[File]] = {
//     def collectWaccFiles(currentDir: File): List[File] = {
//         val files = currentDir.listFiles
//         val waccFiles = files.filter(_.isFile).filter(_.getName.endsWith(".s")).toList
//         val subdirFiles = files.filter(_.isDirectory).flatMap(collectWaccFiles).toList
//         waccFiles ++ subdirFiles
//     }

//     val allWaccFiles = collectWaccFiles(dir)
//     allWaccFiles.groupBy(file => {
//       val parent = file.getParentFile 
//       dir.toPath.relativize(parent.toPath).toString
//     })
//   }

//   // ✅ Extract expected exit code using pattern matching
//   def extractExitCode(fileName: String): Int = fileName match {
//     case name if name.startsWith("exit-1") => 255
//     case name if name.startsWith("exitBasic") => 7
//     case _ => 0 // Default exit code for all other cases
//   }
// }
