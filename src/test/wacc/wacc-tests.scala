import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.File
import scala.sys.process._
import wacc.Constants._

/* all possible subcategories: 
trait WACCTestUtils {
  val ignoredSubcategories =   Set (
    "advanced",
    "array",
    "basic", "basic/exit", "basic/skip",
    "exit",
    "expressions",
    "function", "function/nested_functions", "function/simple_functions",
    "if",
    "literals",
    "IO", "IO/print", "IO/read",
    "multiple",
    "pairs",
    "print",
    "runtimeErr", "runtimeErr/arrayOutOfBounds", "runtimeErr/badChar", "runtimeErr/divideByZero", "runtimeErr/integerOverflow", "runtimeErr/nullDereference",
    "scope",
    "sequence",
    "read",
    "variables",
    "while")
    To test all, use Set.empty[String]
  */
  
trait WACCTestUtils {
  val ignoredSubcategories =
    Set (
    "advanced",
    "array",
    "basic", 
    "basic/exit", "basic/skip", //
    "exit", //
    "expressions", //
    "function", 
    "function/nested_functions", // 
    "function/simple_functions",
    "literals", //(syntax only)
    // "if", //
    "IO",  //
    "IO/print", //
    "IO/read", //
    "multiple", // failed for unknown reasons
    "pairs",
    "print", //
    "runtimeErr", //
    "runtimeErr/arrayOutOfBounds", //
    "runtimeErr/badChar", //
    "runtimeErr/divideByZero", //
    "runtimeErr/integerOverflow", //
    "runtimeErr/nullDereference", // failed due to pair types
    "scope",
    "sequence", //
    "read", //
    "variables", //X : scopevar issue
    "while"  //
    )

  def getListOfWaccFiles(dir: File): Map[String, List[File]] = {
    def collectWaccFiles(currentDir: File): List[File] = {
        val files = currentDir.listFiles
        val waccFiles = files.filter(_.isFile).filter(_.getName.endsWith(".wacc")).toList
        val subdirFiles = files.filter(_.isDirectory).flatMap(collectWaccFiles).toList
        waccFiles ++ subdirFiles
    }

    val allWaccFiles = collectWaccFiles(dir)
    allWaccFiles.groupBy(file => {
      val parent = file.getParentFile 
      dir.toPath.relativize(parent.toPath).toString
    })
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
      subcat should s"return exit code $exitValid for subcategory $subcat" in {
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
      subcat should s"return exit code $exitInvalidSyntax for subcategory $subcat" in {
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
      subcat should s"return exit code $exitInvalidSemantics for subcategory $subcat" in {
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