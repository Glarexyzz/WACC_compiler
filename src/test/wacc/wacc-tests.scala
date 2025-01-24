import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.File
import scala.io.Source
import scala.sys.process._
import wacc.Constants._

/* we want to sort the tests by their subdirectories instead of 
  valid, invalid_syntax, invalid_semantics. The idea is that to use the switch case to sort the tests
  "advanced" "array", "basic" "exit, "expressions" "function",  "if" "literals" "IO" "multiple" "pairs" "runtimeErr" "print" "scope" "sequence" "read"  "variables" "while"
  */  

class ValidWACCtests extends AnyFlatSpec with Matchers {

  val testRootValid = "src/test/wacc/valid"
  val testRootSyntax = "src/test/wacc/invalid_syntax"
  val testRootSemantics = "src/test/wacc/invalid_semantics"

  val testFilesValid = getListOfWaccFiles(new File(testRootValid))
  val testFilesSyntax = getListOfWaccFiles(new File(testRootSyntax))
  val testFilesSemantics = getListOfWaccFiles(new File(testRootSemantics))

 // Valid
  testFilesValid.foreach { file => 
    "Valid" should s"return exit code $exitValid for $file" in {
        val exitCode = runCompilerAndGetExitCode(file.getAbsolutePath)
        withClue(s"File ${file.getName} failed: ") {
          exitCode shouldBe exitValid
      }
    }

  }

  // Invalid Syntax
  testFilesSyntax.foreach { file => 
    ignore should s"return exit code $exitInvalidSyntax for $file" in {
        val exitCode = runCompilerAndGetExitCode(file.getAbsolutePath)
        withClue(s"File ${file.getName} failed: ") {
          exitCode shouldBe exitInvalidSyntax
      }
    }

  }

  // Invalid Semantics
  testFilesSemantics.foreach { file => 
    ignore should s"return exit code $exitInvalidSemantics for $file" in {
        val exitCode = runCompilerAndGetExitCode(file.getAbsolutePath)
        withClue(s"File ${file.getName} failed: ") {
          exitCode shouldBe exitInvalidSemantics
      }
    }
  }

  private def getListOfWaccFiles(dir: File): List[File] = {
    val files = dir.listFiles
    if (files != null) {
      files.toList.flatMap { file =>
        if (file.isDirectory) getListOfWaccFiles(file)
        else if (file.getName.endsWith(".wacc")) List(file)
        else Nil
      }
    } else {
      Nil
    }
  }

  private def runCompilerAndGetExitCode(filePath: String): Int = {
    val command = s"scala shebang . $filePath"
    val process = Process(command)
    val exitValue = process.!
    exitValue
  }
}