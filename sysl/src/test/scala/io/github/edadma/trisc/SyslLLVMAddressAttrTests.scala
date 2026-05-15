package io.github.edadma.trisc

/** `#address(N)` MMIO attribute lowering on the LLVM backend.
  *
  * The interpreter version of these tests lives in SyslAddressAttrTests, where reads
  * and writes hit a JVM-side flat memory map. On LLVM the literal address is real —
  * we can't safely write to a random hardware address, so these tests verify the IR
  * *shape*: each read/write must lower through `inttoptr` to the declared address with
  * the correct element width.
  *
  * The analyzer rewrites `reg = v` to `TDerefAssignStmt(TCast(TIntLit(addr), PtrType(typ)), v)`
  * and `reg` (read) to `TDeref(TCast(TIntLit(addr), PtrType(typ)), typ)`. LLVM lowers
  * the int→ptr cast via `inttoptr` and the deref via `load` / `store` on the resulting
  * pointer, which is exactly what bare-metal MMIO needs. Closes audit item #8. */
class SyslLLVMAddressAttrTests extends SyslLLVMTestHelpers {

  "address var write lowers to inttoptr + store" in {
    val ir = compileLLVM(
      """#address(0x40001000)
        |var reg: u32
        |
        |main() -> int
        |    reg = 42u32
        |    0
        |""".stripMargin)
    // Literal address as i64 constant
    ir should include("1073745920") // 0x40001000
    // Int-to-pointer cast, then store the value through it
    ir should include("inttoptr")
    ir should include("store i32 42")
  }

  "address var read lowers to inttoptr + load" in {
    val ir = compileLLVM(
      """#address(0x40002000)
        |var reg: u32
        |
        |main() -> int
        |    int(reg)
        |""".stripMargin)
    ir should include("1073750016") // 0x40002000
    ir should include("inttoptr")
    ir should include("load i32")
  }

  "address var with u8 register uses i8 load/store" in {
    val ir = compileLLVM(
      """#address(0x40003000)
        |var status: u8
        |
        |main() -> int
        |    status = 0xABu8
        |    int(status)
        |""".stripMargin)
    ir should include("1073754112") // 0x40003000
    ir should include("inttoptr")
    // u8 register: byte-wide load and store
    ir should include("store i8")
    ir should include("load i8")
  }

  "address var with i64 register uses i64 load/store" in {
    val ir = compileLLVM(
      """#address(0x40004000)
        |var counter: i64
        |
        |main() -> int
        |    counter = i64(0xDEAD_BEEF)
        |    int(counter)
        |""".stripMargin)
    ir should include("1073758208") // 0x40004000
    ir should include("inttoptr")
    ir should include("store i64")
    ir should include("load i64")
  }

  "multiple address vars get distinct addresses in IR" in {
    val ir = compileLLVM(
      """#address(0x50000000)
        |var dev_a: u32
        |#address(0x50000004)
        |var dev_b: u32
        |
        |main() -> int
        |    dev_a = 1u32
        |    dev_b = 2u32
        |    0
        |""".stripMargin)
    ir should include("1342177280") // 0x50000000
    ir should include("1342177284") // 0x50000004
  }

  "compound assign on address var does read-modify-write at the same address" in {
    val ir = compileLLVM(
      """#address(0x60000000)
        |var reg: u32
        |
        |main() -> int
        |    reg = 10u32
        |    reg += 5u32
        |    0
        |""".stripMargin)
    val addr = "1610612736" // 0x60000000
    ir should include(addr)
    // The compound assign should produce TWO references to the address — once for
    // the load (read side of RMW) and once for the store (write side). Pin both
    // sides emit through the same address constant.
    val occurrences = addr.r.findAllMatchIn(ir).length
    occurrences should be >= 2
  }
}
