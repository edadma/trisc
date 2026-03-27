package io.github.edadma.trisc

class StackTests extends TestHelpers {

  val STACK_VECTORS: String =
    """dd 0xFF0
      |dd 160
      |resb 144
      |""".stripMargin

  def withStack(body: String): String =
    STACK_VECTORS +
      s"""movi r7, 0xF00
         |$body
         |halt
         |""".stripMargin

  // ===== PSHB / POPB =====

  "pshb and popb round-trip" in {
    val cpu = runCPU(withStack("ldi r1, 0x42\npshb r1\npopb r2"))
    cpu.r(2).read shouldBe 0x42
  }

  "pshb decrements sp by 1" in {
    val cpu = runCPU(withStack("ldi r1, 0x41\npshb r1"))
    cpu.r(7).read shouldBe 0xEFF
  }

  "popb increments sp by 1" in {
    val cpu = runCPU(withStack("ldi r1, 0x41\npshb r1\npopb r2"))
    cpu.r(7).read shouldBe 0xF00
  }

  "pshb stores only low byte" in {
    val cpu = runCPU(withStack("movi r1, 0x1234\npshb r1\npopb r2"))
    cpu.r(2).read shouldBe 0x34
  }

  "popb sign-extends byte" in {
    val cpu = runCPU(withStack("ldi r1, 0x80\npshb r1\npopb r2"))
    cpu.r(2).read shouldBe -128
  }

  "pshb multiple values LIFO order" in {
    val cpu = runCPU(withStack(
      """ldi r1, 0x41
        |ldi r2, 0x42
        |ldi r3, 0x43
        |pshb r1
        |pshb r2
        |pshb r3
        |popb r4
        |popb r5
        |popb r6
        |""".stripMargin))
    cpu.r(4).read shouldBe 0x43
    cpu.r(5).read shouldBe 0x42
    cpu.r(6).read shouldBe 0x41
  }

  "pshb with r0 pushes zero" in {
    val cpu = runCPU(withStack("pshb r0\npopb r1"))
    cpu.r(1).read shouldBe 0
  }

  // ===== PSHS / POPS =====

  "pshs and pops round-trip" in {
    val cpu = runCPU(withStack("movi r1, 0x1234\npshs r1\npops r2"))
    cpu.r(2).read shouldBe 0x1234
  }

  "pshs decrements sp by 2" in {
    val cpu = runCPU(withStack("ldi r1, 0x41\npshs r1"))
    cpu.r(7).read shouldBe 0xEFE
  }

  "pops increments sp by 2" in {
    val cpu = runCPU(withStack("ldi r1, 0x41\npshs r1\npops r2"))
    cpu.r(7).read shouldBe 0xF00
  }

  "pshs stores only low short" in {
    val cpu = runCPU(withStack(
      """ldi r1, 0x12
        |sli r1, 0x34
        |sli r1, 0x56
        |sli r1, 0x78
        |pshs r1
        |pops r2
        |""".stripMargin))
    cpu.r(2).read shouldBe 0x5678
  }

  "pops sign-extends short" in {
    val cpu = runCPU(withStack(
      """ldi r1, 0x80
        |sli r1, 0x00
        |pshs r1
        |pops r2
        |""".stripMargin))
    cpu.r(2).read shouldBe -32768
  }

  "pshs multiple values LIFO order" in {
    val cpu = runCPU(withStack(
      """movi r1, 0x1111
        |movi r2, 0x2222
        |movi r3, 0x3333
        |pshs r1
        |pshs r2
        |pshs r3
        |pops r4
        |pops r5
        |pops r6
        |""".stripMargin))
    cpu.r(4).read shouldBe 0x3333
    cpu.r(5).read shouldBe 0x2222
    cpu.r(6).read shouldBe 0x1111
  }

  "pshs with r0 pushes zero" in {
    val cpu = runCPU(withStack("pshs r0\npops r1"))
    cpu.r(1).read shouldBe 0
  }

  // ===== PSHW / POPW =====

  "pshw and popw round-trip" in {
    val cpu = runCPU(withStack(
      """ldi r1, 0x12
        |sli r1, 0x34
        |sli r1, 0x56
        |pshw r1
        |popw r2
        |""".stripMargin))
    cpu.r(2).read shouldBe cpu.r(1).read
  }

  "pshw decrements sp by 4" in {
    val cpu = runCPU(withStack("ldi r1, 42\npshw r1"))
    cpu.r(7).read shouldBe 0xEFC
  }

  "popw increments sp by 4" in {
    val cpu = runCPU(withStack("ldi r1, 42\npshw r1\npopw r2"))
    cpu.r(7).read shouldBe 0xF00
  }

  "pshw stores only low word" in {
    val cpu = runCPU(withStack(
      """ldi r1, 0x12
        |sli r1, 0x34
        |sli r1, 0x56
        |sli r1, 0x78
        |sli r1, 0x9A
        |sli r1, 0xBC
        |sli r1, 0xDE
        |sli r1, 0xF0
        |pshw r1
        |popw r2
        |""".stripMargin))
    cpu.r(2).read shouldBe 0x9ABCDEF0L.toInt // sign-extended 32-bit
  }

  "pshw multiple values LIFO order" in {
    val cpu = runCPU(withStack(
      """movi r1, 0x1111
        |movi r2, 0x2222
        |movi r3, 0x3333
        |pshw r1
        |pshw r2
        |pshw r3
        |popw r4
        |popw r5
        |popw r6
        |""".stripMargin))
    cpu.r(4).read shouldBe 0x3333
    cpu.r(5).read shouldBe 0x2222
    cpu.r(6).read shouldBe 0x1111
  }

  "pshw with r0 pushes zero" in {
    val cpu = runCPU(withStack("pshw r0\npopw r1"))
    cpu.r(1).read shouldBe 0
  }

  // ===== PSHD / POPD =====

  "pshd and popd round-trip" in {
    val cpu = runCPU(withStack(
      """ldi r1, 0x12
        |sli r1, 0x34
        |sli r1, 0x56
        |sli r1, 0x78
        |sli r1, 0x9A
        |sli r1, 0xBC
        |sli r1, 0xDE
        |sli r1, 0xF0
        |pshd r1
        |popd r2
        |""".stripMargin))
    cpu.r(2).read shouldBe cpu.r(1).read
  }

  "pshd decrements sp by 8" in {
    val cpu = runCPU(withStack("ldi r1, 42\npshd r1"))
    cpu.r(7).read shouldBe 0xEF8
  }

  "popd increments sp by 8" in {
    val cpu = runCPU(withStack("ldi r1, 42\npshd r1\npopd r2"))
    cpu.r(7).read shouldBe 0xF00
  }

  "pshd preserves full 64-bit value" in {
    val cpu = runCPU(withStack(
      """ldi r1, 0x12
        |sli r1, 0x34
        |sli r1, 0x56
        |sli r1, 0x78
        |sli r1, 0x9A
        |sli r1, 0xBC
        |sli r1, 0xDE
        |sli r1, 0xF0
        |pshd r1
        |popd r2
        |""".stripMargin))
    cpu.r(2).read shouldBe cpu.r(1).read
    cpu.r(1).read shouldBe 0x123456789ABCDEF0L
  }

  "pshd multiple values LIFO order" in {
    val cpu = runCPU(withStack(
      """ldi r1, 11
        |ldi r2, 22
        |ldi r3, 33
        |pshd r1
        |pshd r2
        |pshd r3
        |popd r4
        |popd r5
        |popd r6
        |""".stripMargin))
    cpu.r(4).read shouldBe 33
    cpu.r(5).read shouldBe 22
    cpu.r(6).read shouldBe 11
  }

  "pshd with r0 pushes zero" in {
    val cpu = runCPU(withStack("pshd r0\npopd r1"))
    cpu.r(1).read shouldBe 0
  }

  // ===== Mixed size stack operations =====

  "mixed push/pop sizes maintain correct sp" in {
    val cpu = runCPU(withStack(
      """ldi r1, 0x41
        |ldi r2, 42
        |pshd r2
        |pshb r1
        |""".stripMargin))
    // sp started at 0xF00, pshd -8 = 0xEF8, pshb -1 = 0xEF7
    cpu.r(7).read shouldBe 0xEF7
  }

  // ===== Subroutine call with stack frame =====

  "push/pop in subroutine preserves caller registers" in {
    val cpu = runCPU(
      """dd 0xFF0
        |dd 160
        |resb 144
        |movi r7, 0xF00
        |ldi r1, 42
        |ldi r2, 99
        |movi r3, func
        |pshd r1
        |pshd r2
        |jalr r6, r3
        |popd r2
        |popd r1
        |halt
        |func
        |  ldi r1, 0
        |  ldi r2, 0
        |  jalr r0, r6
        |""".stripMargin)
    cpu.r(1).read shouldBe 42
    cpu.r(2).read shouldBe 99
  }
}
