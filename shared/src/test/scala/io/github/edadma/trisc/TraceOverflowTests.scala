package io.github.edadma.trisc

class TraceOverflowTests extends TestHelpers {

  // ===== OVERFLOW FLAG (V) =====

  "add sets V on signed overflow (positive)" in {
    // Long.MaxValue + 1 overflows
    val cpu = runCPU(VECTORS +
      """movi r1, 0x7FFF
        |sli r1, 0xFF
        |sli r1, 0xFF
        |sli r1, 0xFF
        |sli r1, 0xFF
        |sli r1, 0xFF
        |sli r1, 0xFF
        |ldi r2, 1
        |add r3, r1, r2
        |gpsr r4
        |halt
        |""".stripMargin)
    (cpu.r(4).read & 32) shouldBe 32
  }

  "add clears V when no signed overflow" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 10
        |ldi r2, 20
        |add r3, r1, r2
        |gpsr r4
        |halt
        |""".stripMargin)
    (cpu.r(4).read & 32) shouldBe 0
  }

  "sub sets V on signed overflow" in {
    // Long.MinValue - 1 overflows (movi 0x80 + 7 sli's = 8 bytes = Long.MinValue)
    val cpu = runCPU(VECTORS +
      """movi r1, 0x80
        |sli r1, 0x00
        |sli r1, 0x00
        |sli r1, 0x00
        |sli r1, 0x00
        |sli r1, 0x00
        |sli r1, 0x00
        |sli r1, 0x00
        |ldi r2, 1
        |sub r3, r1, r2
        |gpsr r4
        |halt
        |""".stripMargin)
    (cpu.r(4).read & 32) shouldBe 32
  }

  "sub clears V when no signed overflow" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 50
        |ldi r2, 30
        |sub r3, r1, r2
        |gpsr r4
        |halt
        |""".stripMargin)
    (cpu.r(4).read & 32) shouldBe 0
  }

  "neg sets V when negating Long.MinValue" in {
    val cpu = runCPU(VECTORS +
      """movi r1, 0x80
        |sli r1, 0x00
        |sli r1, 0x00
        |sli r1, 0x00
        |sli r1, 0x00
        |sli r1, 0x00
        |sli r1, 0x00
        |sli r1, 0x00
        |neg r2, r1
        |gpsr r3
        |halt
        |""".stripMargin)
    (cpu.r(3).read & 32) shouldBe 32
  }

  "neg clears V for normal values" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 42
        |neg r2, r1
        |gpsr r3
        |halt
        |""".stripMargin)
    (cpu.r(3).read & 32) shouldBe 0
  }

  "sbc sets V on signed overflow" in {
    // Long.MinValue - 0 - borrow(1) overflows
    val cpu = runCPU(VECTORS +
      """movi r1, 0x80
        |sli r1, 0x00
        |sli r1, 0x00
        |sli r1, 0x00
        |sli r1, 0x00
        |sli r1, 0x00
        |sli r1, 0x00
        |sli r1, 0x00
        |ldi r2, 0
        |ldi r3, 0xFF
        |ldi r4, 0
        |sub r5, r4, r3
        |sbc r5, r1, r2
        |gpsr r6
        |halt
        |""".stripMargin)
    (cpu.r(6).read & 32) shouldBe 32
  }

  "add clears V after non-overflow following overflow" in {
    val cpu = runCPU(VECTORS +
      """movi r1, 0x7FFF
        |sli r1, 0xFF
        |sli r1, 0xFF
        |sli r1, 0xFF
        |sli r1, 0xFF
        |sli r1, 0xFF
        |sli r1, 0xFF
        |ldi r2, 1
        |add r3, r1, r2
        |ldi r4, 5
        |ldi r5, 3
        |add r6, r4, r5
        |gpsr r1
        |halt
        |""".stripMargin)
    (cpu.r(1).read & 32) shouldBe 0
  }

  // ===== TRAPV =====

  "trapv does nothing when V is clear" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 5
        |ldi r2, 3
        |add r3, r1, r2
        |trapv
        |ldi r4, 42
        |halt
        |""".stripMargin)
    cpu.r(4).read shouldBe 42
  }

  "trapv traps when V is set" in {
    val cpu = runCPU(
      """dd 0xFF0
        |dd reset
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd ovhandler
        |dd 0
        |reset
        |  movi r1, 0x7FFF
        |  sli r1, 0xFF
        |  sli r1, 0xFF
        |  sli r1, 0xFF
        |  sli r1, 0xFF
        |  sli r1, 0xFF
        |  sli r1, 0xFF
        |  ldi r2, 1
        |  add r3, r1, r2
        |  trapv
        |  ldi r4, 99
        |  halt
        |ovhandler
        |  ldi r4, 42
        |  halt
        |""".stripMargin)
    cpu.r(4).read shouldBe 42
    cpu.r(4).read should not be 99L
  }

  "trapv handler can resume via rte" in {
    val cpu = runCPU(
      """dd 0xFF0
        |dd reset
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd ovhandler
        |dd 0
        |reset
        |  movi r1, 0x7FFF
        |  sli r1, 0xFF
        |  sli r1, 0xFF
        |  sli r1, 0xFF
        |  sli r1, 0xFF
        |  sli r1, 0xFF
        |  sli r1, 0xFF
        |  ldi r2, 1
        |  add r3, r1, r2
        |  trapv
        |  ldi r4, 77
        |  halt
        |ovhandler
        |  ldi r5, 55
        |  rte
        |""".stripMargin)
    cpu.r(4).read shouldBe 77
    cpu.r(5).read shouldBe 55
  }

  // ===== CHK =====

  "chk does nothing when value in range" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 5
        |ldi r2, 10
        |chk r1, r2
        |ldi r3, 42
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 42
  }

  "chk does nothing when value equals upper bound" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 10
        |ldi r2, 10
        |chk r1, r2
        |ldi r3, 42
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 42
  }

  "chk does nothing when value is zero" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 0
        |ldi r2, 10
        |chk r1, r2
        |ldi r3, 42
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 42
  }

  "chk traps when value is negative" in {
    val cpu = runCPU(
      """dd 0xFF0
        |dd reset
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd chkhandler
        |reset
        |  ldi r1, 10
        |  neg r1, r1
        |  ldi r2, 100
        |  chk r1, r2
        |  ldi r3, 99
        |  halt
        |chkhandler
        |  ldi r3, 42
        |  halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 42
    cpu.r(3).read should not be 99L
  }

  "chk traps when value exceeds upper bound" in {
    val cpu = runCPU(
      """dd 0xFF0
        |dd reset
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd chkhandler
        |reset
        |  ldi r1, 50
        |  ldi r2, 10
        |  chk r1, r2
        |  ldi r3, 99
        |  halt
        |chkhandler
        |  ldi r3, 42
        |  halt
        |""".stripMargin)
    cpu.r(3).read shouldBe 42
  }

  "chk handler can resume via rte" in {
    val cpu = runCPU(
      """dd 0xFF0
        |dd reset
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd chkhandler
        |reset
        |  ldi r1, 50
        |  ldi r2, 10
        |  chk r1, r2
        |  ldi r3, 77
        |  halt
        |chkhandler
        |  ldi r5, 55
        |  rte
        |""".stripMargin)
    cpu.r(3).read shouldBe 77
    cpu.r(5).read shouldBe 55
  }

  // ===== TRACE =====
  // Trace uses 68k-style semantics: trace fires based on T state BEFORE the instruction,
  // not after. This means spsr that sets T does NOT itself trigger trace — the NEXT
  // instruction does. And rte in the handler does NOT trigger trace even though it
  // restores T — the next instruction at the return point does.

  "trace fires after first instruction executed with T set" in {
    val cpu = runCPU(
      """dd 0xFF0
        |dd reset
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd tracehandler
        |dd 0
        |dd 0
        |reset
        |  ldi r1, 0x13
        |  spsr r1
        |  ldi r2, 42
        |  ldi r3, 99
        |  halt
        |tracehandler
        |  ldi r4, 77
        |  halt
        |""".stripMargin)
    // spsr sets T but T was clear before spsr → no trace after spsr
    // ldi r2,42 executes with T set → trace fires AFTER it completes
    // So r2=42, r3 should NOT be 99 (trace handler halts)
    cpu.r(2).read shouldBe 42
    cpu.r(3).read should not be 99L
    cpu.r(4).read shouldBe 77
  }

  "trace exception clears T to prevent infinite loop" in {
    val cpu = runCPU(
      """dd 0xFF0
        |dd reset
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd tracehandler
        |dd 0
        |dd 0
        |reset
        |  ldi r1, 0x13
        |  spsr r1
        |  nop
        |  halt
        |tracehandler
        |  gpsr r5
        |  ldi r3, 10
        |  ldi r4, 20
        |  halt
        |""".stripMargin)
    // spsr sets T. nop executes, trace fires. Handler runs with T cleared.
    cpu.r(3).read shouldBe 10
    cpu.r(4).read shouldBe 20
    (cpu.r(5).read & 16) shouldBe 0
  }

  "trace handler can single-step via rte" in {
    // rte restores saved PSR which has T set. Since T was clear before rte,
    // rte itself doesn't trigger trace. The next instruction at the return
    // point has T set → trace fires after it.
    val cpu = runCPU(
      """dd 0xFF0
        |dd reset
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd tracehandler
        |dd 0
        |dd 0
        |reset
        |  ldi r1, 0x13
        |  spsr r1
        |  ldi r2, 1
        |  ldi r3, 2
        |  ldi r4, 3
        |  halt
        |tracehandler
        |  addi r6, r6, 1
        |  ldi r5, 3
        |  beq r6, r5, .done
        |  rte
        |.done
        |  halt
        |""".stripMargin)
    // Trace fires after: ldi r2 (r6=1), ldi r3 (r6=2), ldi r4 (r6=3, done)
    // halt has T set but changes state to Halt, so trace check doesn't fire
    cpu.r(6).read shouldBe 3
    cpu.r(2).read shouldBe 1
    cpu.r(3).read shouldBe 2
    cpu.r(4).read shouldBe 3
  }

  "trace does not fire when T is clear" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 10
        |ldi r2, 20
        |ldi r3, 30
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe 10
    cpu.r(2).read shouldBe 20
    cpu.r(3).read shouldBe 30
  }

  "non-trace exception during trace mode runs handler without trace" in {
    // When T is set and trap fires, trap changes state to Trap0 (not Run),
    // so trace check doesn't fire. Trap exception entry clears T.
    // The trap handler runs without trace.
    val cpu = runCPU(
      """dd 0xFF0
        |dd reset
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd trap0handler
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd tracehandler
        |dd 0
        |dd 0
        |reset
        |  ldi r1, 0x13
        |  spsr r1
        |  trap 0
        |  halt
        |tracehandler
        |  ldi r4, 99
        |  halt
        |trap0handler
        |  ldi r2, 42
        |  ldi r3, 43
        |  halt
        |""".stripMargin)
    // spsr sets T. T was clear before trap 0 → no, wait: T WAS set before trap 0.
    // trap 0 sets state=Trap0 (not Run). Trace check: state != Run → no trace.
    // Trap0 exception entry clears T. Handler runs normally.
    cpu.r(2).read shouldBe 42
    cpu.r(3).read shouldBe 43
    cpu.r(4).read should not be 99L
  }

  "spsr that sets T does not itself trigger trace" in {
    val cpu = runCPU(
      """dd 0xFF0
        |dd reset
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd tracehandler
        |dd 0
        |dd 0
        |reset
        |  ldi r1, 0x13
        |  spsr r1
        |  ldi r2, 55
        |  halt
        |tracehandler
        |  gpsr r3
        |  halt
        |""".stripMargin)
    // spsr sets T, but T was clear before → no trace after spsr
    // ldi r2, 55 runs (T set before it), then trace fires
    // Saved PC = address of halt, handler halts instead
    cpu.r(2).read shouldBe 55
  }
}
