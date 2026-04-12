package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class LinkerSegmentTests extends AnyFreeSpec with Matchers {

  // Regression test: cross-segment relocation with boot/io _default_ segments.
  // Caught a bug where the linker used assembler-internal segment offsets as
  // absolute addresses, and where merged same-named segments zero-filled gaps
  // that contained interleaved code/bss segments.
  "cross-segment globals with boot and io modules" in {
    val src = """var counter = 0
                |inc()
                |    counter += 1
                |main() -> int
                |    inc()
                |    inc()
                |    inc()
                |    counter
                |""".stripMargin

    val ast = (new SyslParser).parseProgram(src).toOption.get
    val typed = (new SyslAnalyzer).analyze(ast)
    val asm = (new SyslTriscCodegen).generate(typed)
    val tof = assemble(asm, relocatable = true)

    // bss segment should NOT have explicitOrg (it's assembler-stacked, not user-specified)
    tof.segments.find(_.name == "bss").foreach { seg =>
      seg.explicitOrg shouldBe false
    }

    val linked = Linker.link(Seq(Runtime.bootTof, tof, Runtime.ioTof))

    // bss must be placed after code, not at its assembler-internal offset
    val codeOrg = linked.segments.find(_.name == "code").get.org
    val bssOrg = linked.segments.find(_.name == "bss").get.org
    bssOrg should be > codeOrg

    // execution should produce correct result
    val stdout = new Stdout(Runtime.stdoutAddress)
    val ram = new RAM(0, Runtime.stdoutAddress.toInt)
    val mem = new Memory("Memory", ram, stdout)
    linked.load(mem)
    val cpu = new CPU(mem) { limit = 100000; quiet = true }
    cpu.reset()
    cpu.run()
    cpu.r(1).read shouldBe 3
  }
}
