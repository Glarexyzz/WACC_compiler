import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.File
import scala.sys.process._
import wacc.Constants._
import java.nio.file.{Files, Paths, StandardCopyOption}


/* all possible subcategories: 
trait WACCTestUtils {
  val pendingSubcategories =   Set (
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
  val pendingSubcategories =
    Set (
    "advanced",
    "array",
    "basic", 
    "basic/exit", "basic/skip", 
    "exit", 
    "expressions", 
    "function", 
    "function/nested_functions",  
    "function/simple_functions",
    "literals", 
    "if", 
    "IO",  
    "IO/print", 
    "IO/read", 
    "multiple",
    "pairs",
    "print", 
    "runtimeErr", 
    "runtimeErr/arrayOutOfBounds", 
    "runtimeErr/badChar", 
    "runtimeErr/divideByZero", 
    "runtimeErr/integerOverflow", 
    "runtimeErr/nullDereference", 
    "scope",
    "sequence", 
    "read", 
    "variables", 
    "while" 
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
    // Move the generated `.s` file to the correct test directory
    moveGeneratedSFile(filePath)

    exitValue
  } 

  def moveGeneratedSFile(waccFilePath: String): Unit = {
    val waccFileName = new File(waccFilePath).getName.replace(".wacc", ".s")
    val generatedSFile = Paths.get(s"./$waccFileName")

    val testSFile = Paths.get(
      waccFilePath.replace("src/test/wacc/valid", "src/test/wacc/valid-assembly").replace(".wacc", ".s")
    )

    if (Files.exists(generatedSFile)) {
      Files.createDirectories(testSFile.getParent)
      Files.move(generatedSFile, testSFile, StandardCopyOption.REPLACE_EXISTING)
    }
  } 
}

class ValidTest extends AnyFlatSpec with Matchers with WACCTestUtils {
  val testRootValid = new File("src/test/wacc/valid")
  val testFiles = getListOfWaccFiles(testRootValid)

  for ((subcat, files) <- testFiles) {
    if (pendingSubcategories.contains(subcat)) {
      subcat should s"return exit code $exitValid for subcategory $subcat" in pending
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
    if (pendingSubcategories.contains(subcat)) {
      ignore should s"return exit code $exitInvalidSyntax for subcategory $subcat" in {}
    } else {
      ignore should s"return exit code $exitInvalidSyntax for subcategory $subcat" in {
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
    if (pendingSubcategories.contains(subcat)) {
      ignore should s"return exit code $exitInvalidSemantics for subcategory $subcat" in {}
    } else {
      ignore should s"return exit code $exitInvalidSemantics for subcategory $subcat" in {
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