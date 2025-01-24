import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.File
import scala.sys.process._
import wacc.Constants._

/* all possible subcategories: 
  Set ("advanced", "array", "basic", "exit", "expressions", 
  "function", "if", "literals", "IO", "multiple", "pairs", "runtimeErr", "print", "scope", 
  "sequence", "read",  "variables", "while")
  */
  
trait WACCTestUtils {
  val ignoredSubcategories =   Set ("advanced", "array", "basic", "exit", "expressions", 
  "function", "if", "literals", "IO", "multiple", "pairs", "runtimeErr", "print", "scope", 
  "sequence", "read",  "variables", "while")

  def getListOfWaccFiles(dir: File): Map[String, List[File]] = {
    val subdirs = dir.listFiles.filter(_.isDirectory)
    subdirs.map { subdir =>
      val files = subdir.listFiles.filter(_.getName.endsWith(".wacc")).toList
      subdir.getName -> files
    }.toMap
  }

  def runCompilerAndGetExitCode(filePath: String): Int = {
    val command = s"scala shebang . $filePath"
    val process = Process(command)
    val exitValue = process.!
    exitValue
  }
}

class ValidTest extends AnyFlatSpec with Matchers with WACCTestUtils {
  val testRootValid = new File("src/test/wacc/valid")
  val testFiles = getListOfWaccFiles(testRootValid)

  for ((subcat, files) <- testFiles) {
    if (ignoredSubcategories.contains(subcat)) {
      ignore should s"return exit code $exitValid for subcategory $subcat" in {}
    } else {
      subcat should s"return exit code $exitValid for all files" in {
        files.foreach { file =>
          val exitCode = runCompilerAndGetExitCode(file.getAbsolutePath)
          withClue(s"File ${file.getName} failed: ") {
            exitCode shouldBe exitValid
          }
        }
      }
    }
  }
}

class InvalidSyntaxTest extends AnyFlatSpec with Matchers with WACCTestUtils {
  val testRootSyntax = new File("src/test/wacc/invalid_syntax")
  val testFiles = getListOfWaccFiles(testRootSyntax)

  for ((subcat, files) <- testFiles) {
    if (ignoredSubcategories.contains(subcat)) {
      ignore should s"return exit code $exitInvalidSyntax for subcategory $subcat" in {}
    } else {
      subcat should s"return exit code $exitInvalidSyntax for all files" in {
        files.foreach { file =>
          val exitCode = runCompilerAndGetExitCode(file.getAbsolutePath)
          withClue(s"File ${file.getName} failed: ") {
            exitCode shouldBe exitInvalidSyntax
          }
        }
      }
    }
  }
}

class InvalidSemanticsTest extends AnyFlatSpec with Matchers with WACCTestUtils {
  val testRootSemantics = new File("src/test/wacc/invalid_semantics")
  val testFiles = getListOfWaccFiles(testRootSemantics)

  for ((subcat, files) <- testFiles) {
    if (ignoredSubcategories.contains(subcat)) {
      ignore should s"return exit code $exitInvalidSemantics for subcategory $subcat" in {}
    } else {
      subcat should s"return exit code $exitInvalidSemantics for all files" in {
        files.foreach { file =>
          val exitCode = runCompilerAndGetExitCode(file.getAbsolutePath)
          withClue(s"File ${file.getName} failed: ") {
            exitCode shouldBe exitInvalidSemantics
          }
        }
      }
    }
  }
}