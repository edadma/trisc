package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** `#address(N)` attribute on module-level var declarations. The var is a handle on a
 *  fixed physical address (MMIO device register). Reads lower to a load from `(N as *T)`,
 *  writes to a store. No storage is emitted. The interpreter models memory via a
 *  JVM-side int array pre-seeded with zeros, so reads of an untouched address see 0 and
 *  writes survive a subsequent read. */
class SyslAddressAttrTests extends SyslTestHelpers {

  // The interpreter performs raw memory reads/writes through literal addresses. It maps
  // addresses into a flat memory space; values round-trip correctly within a single run.

  "address var: write then read returns the written value" in {
    eval("""
      |#address(0x1000)
      |var reg: u32
      |main() -> int
      |    reg = 42
      |    return int(reg)
      |""".stripMargin) shouldBe 42
  }

  "address var: compound assignment does read-modify-write" in {
    eval("""
      |#address(0x2000)
      |var reg: u32
      |main() -> int
      |    reg = 10
      |    reg += 5
      |    reg += 3
      |    return int(reg)
      |""".stripMargin) shouldBe 18
  }

  "address var: multiple mapped registers are independent" in {
    eval("""
      |#address(0x3000)
      |var regA: u32
      |#address(0x3004)
      |var regB: u32
      |main() -> int
      |    regA = 100
      |    regB = 200
      |    return int(regA + regB)
      |""".stripMargin) shouldBe 300
  }

  "address var with u8 register uses byte-wide load/store" in {
    eval("""
      |#address(0x4000)
      |var status: u8
      |main() -> int
      |    status = 0xAB
      |    return int(status)
      |""".stripMargin) shouldBe 0xAB
  }

  "address var may be read in an expression" in {
    eval("""
      |#address(0x5000)
      |var a: u32
      |#address(0x5004)
      |var b: u32
      |main() -> int
      |    a = 7
      |    b = 9
      |    return int(a * b + a)
      |""".stripMargin) shouldBe 70
  }

  "address var hex literal is parsed and used correctly" in {
    eval("""
      |#address(0xDEAD_BEEF)
      |var scratch: u32
      |main() -> int
      |    scratch = 1
      |    return int(scratch)
      |""".stripMargin) shouldBe 1
  }

  // ===== Errors =====

  "address requires an explicit type" in {
    val t = intercept[Exception] {
      eval("""
        |#address(0x1000)
        |var reg = 0
        |main() -> int = 0
        |""".stripMargin)
    }
    t.getMessage should include("explicit type")
  }

  "address cannot be combined with const" in {
    val t = intercept[Exception] {
      eval("""
        |#address(0x1000)
        |const reg: u32 = 0
        |main() -> int = 0
        |""".stripMargin)
    }
    t.getMessage should include("const")
  }

  "address rejects non-integer argument" in {
    val t = intercept[Exception] {
      eval("""
        |#address("not_a_number")
        |var reg: u32
        |main() -> int = 0
        |""".stripMargin)
    }
    t.getMessage should include("integer")
  }
}
