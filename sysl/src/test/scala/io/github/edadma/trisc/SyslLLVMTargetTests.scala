package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SyslLLVMTargetTests extends AnyFreeSpec with Matchers {

  private def compileWithTarget(source: String, target: String): String =
    val Right(ast) = (new SyslParser).parseProgram(source): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    (new SyslLLVMCodegen(target)).generate(typed)

  private val trivialSrc = "main() -> int = 0\n"

  "x86_64-elf emits x86_64 datalayout and elf triple" in {
    val ir = compileWithTarget(trivialSrc, "x86_64-elf")
    ir should include("target triple = \"x86_64-unknown-elf\"")
    ir should include("e-m:e-p270:32:32")
  }

  "x86_64-linux emits linux-gnu triple" in {
    val ir = compileWithTarget(trivialSrc, "x86_64-linux")
    ir should include("target triple = \"x86_64-unknown-linux-gnu\"")
  }

  "aarch64-elf emits aarch64 datalayout and elf triple" in {
    val ir = compileWithTarget(trivialSrc, "aarch64-elf")
    ir should include("target triple = \"aarch64-unknown-elf\"")
    ir should include("e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128")
  }

  "aarch64 alias matches aarch64-elf" in {
    val ir = compileWithTarget(trivialSrc, "aarch64")
    ir should include("target triple = \"aarch64-unknown-elf\"")
  }

  "aarch64-linux emits linux-gnu triple" in {
    val ir = compileWithTarget(trivialSrc, "aarch64-linux")
    ir should include("target triple = \"aarch64-unknown-linux-gnu\"")
  }

  "host emits no target lines" in {
    val ir = compileWithTarget(trivialSrc, "host")
    ir should not include "target triple ="
    ir should not include "target datalayout ="
  }
}
