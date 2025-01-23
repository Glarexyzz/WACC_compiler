import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.File
import scala.sys.process._

class WACCCompilerSpec extends AnyFlatSpec with Matchers {

  val compilerScript = "./compile" // Path to your compiler script
  val testRoot = "src/test/wacc/valid" // Root directory of test cases
  val expectedExitCode = 0 // Expected exit code for valid files can't we make this a... global value perhaps?

  "WACC Compiler" should s"return exit code $expectedExitCode for files in $testRoot" in {
    val testFiles = getListOfWaccFiles(new File(testRoot))

    testFiles.foreach { file =>
      val exitCode = runCompilerAndGetExitCode(file.getAbsolutePath)
      withClue(s"File ${file.getName} failed: ") {
        exitCode shouldBe expectedExitCode
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
    val command = s"$compilerScript $filePath"
    val process = Process(command)
    val exitValue = process.!
    exitValue
  }
}