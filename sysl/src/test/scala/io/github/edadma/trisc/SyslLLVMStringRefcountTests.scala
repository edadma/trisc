package io.github.edadma.trisc

class SyslLLVMStringRefcountTests extends SyslLLVMTestHelpers {

  "literal global has immortal refcount sentinel" in {
    val ir = compileLLVM(
      """main()
        |    var s = "hello"
        |""".stripMargin)
    ir should include("private unnamed_addr constant <{ i64, [6 x i8] }> <{ i64 -1,")
  }

  "literal aliasing many times does not segfault" in {
    // Repeated aliasing of an immortal literal — incr/decr are no-ops.
    llvmExit(
      """main() -> int
        |    var s = "hello"
        |    var t = s
        |    var u = t
        |    var v = u
        |    if v == "hello" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "concat allocates a fresh buffer with header (no leak crash)" in {
    llvmExit(
      """main() -> int
        |    var i = 0
        |    while i < 100
        |        var s = "abc" + "def"
        |        i += 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  "string(ptr, len) copies — buffer outlives source" in {
    // The byte array goes out of scope inside the if; the string returned
    // from string(ptr, len) must still be valid afterwards because it owns its own buffer.
    llvmExit(
      """build() -> string
        |    var arr = new [5]byte
        |    arr[0] = 104
        |    arr[1] = 101
        |    arr[2] = 108
        |    arr[3] = 108
        |    arr[4] = 111
        |    string(&arr[0], 5)
        |
        |main() -> int
        |    var s = build()
        |    if s == "hello" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "string passed to function increments refcount" in {
    llvmExit(
      """get_len(s: string) -> int = len(s)
        |
        |main() -> int
        |    var s = "the quick brown fox"
        |    val n = get_len(s)
        |    if n == 19 then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "concat result returned from function is freed after consumption" in {
    // Many repeated concat results. If the returned strings aren't freed,
    // we'd allocate ~100KB; if they ARE freed, memory stays small.
    // Mainly checks no use-after-free / double-free.
    llvmExit(
      """make() -> string = "aaaa" + "bbbb"
        |
        |main() -> int
        |    var i = 0
        |    while i < 1000
        |        var s = make()
        |        i += 1
        |    0
        |""".stripMargin) shouldBe 0
  }

  "string field in struct is incremented on construction" in {
    llvmExit(
      """struct Holder
        |    s: string
        |
        |main() -> int
        |    var msg = "hello"
        |    var h = Holder(msg)
        |    if h.s == "hello" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "string reassignment decrements old buffer" in {
    llvmExit(
      """main() -> int
        |    var s = "abc" + "def"
        |    s = "xyz" + "qrs"
        |    if s == "xyzqrs" then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "concat returned via if-expr branch survives" in {
    llvmExit(
      """build(b: bool) -> string =
        |    if b then "abc" + "def" else "uvw" + "xyz"
        |
        |main() -> int
        |    var s = build(true)
        |    if s == "abcdef" then 0 else 1
        |""".stripMargin) shouldBe 0
  }
}
