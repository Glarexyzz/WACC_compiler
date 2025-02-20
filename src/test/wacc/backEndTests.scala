/*
    // compile into binary and emulates it
    val outputFile = filepath.replaceAll("\\.wacc$", "")
    val compileCmd = s"aarch64-linux-gnu-gcc -o $outputFile $asmFile"
    if (compileCmd.! == 0) {
      val runCmd = s"qemu-aarch64 ./$outputFile"
      runCmd.!
    } else {
      println(s"❌ Error: Compilation failed for $asmFile")
    }
*/