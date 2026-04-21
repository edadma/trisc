package io.github.edadma.trisc

/** Targeted UAF / double-free coverage for the rc work, run under
  * AddressSanitizer. Each scenario is a known-tricky path from the rc plumbing:
  * closures with heap envs, structs/enums holding rc-bearing fields, &[]T slice
  * deinits, substring, *string deref, etc. macOS does not support LSan so plain
  * leaks are not caught; ASan covers UAF and double-free, which is exactly the
  * failure mode of a wrong incr/decr or missed null-out.
  */
class SyslLLVMRCSanitizerTests extends SyslLLVMTestHelpers {

  // ====================================================================
  // Closures + heap env
  // ====================================================================

  "closure capture string — env walk frees buffers (no UAF)" in {
    llvmExitASan(
      """make() -> (int) -> int
        |    val s = "aa" + "bb"
        |    (x: int) -> x + len(s)
        |
        |main() -> int
        |    val f = make()
        |    f(3)
        |    0
        |""".stripMargin) shouldBe 0
  }

  "closure descriptor copy — env shared, no double-free" in {
    llvmExitASan(
      """make() -> (int) -> int
        |    val s = "aa" + "bb"
        |    (x: int) -> x + len(s)
        |
        |main() -> int
        |    val f = make()
        |    val g = f
        |    g(3)
        |    f(5)
        |    0
        |""".stripMargin) shouldBe 0
  }

  "closure descriptor reassignment — old env decr'd, no UAF" in {
    llvmExitASan(
      """make() -> (int) -> int
        |    val s = "aa" + "bb"
        |    (x: int) -> x + len(s)
        |
        |main() -> int
        |    var f = make()
        |    f = make()
        |    f(3)
        |    0
        |""".stripMargin) shouldBe 0
  }

  "passthrough(closure) — borrowed funcparam does not double-decr" in {
    llvmExitASan(
      """passthrough(g: (int) -> int) -> (int) -> int = g
        |
        |make() -> (int) -> int
        |    val s = "aa" + "bb"
        |    (x: int) -> x + len(s)
        |
        |main() -> int
        |    val f = make()
        |    val h = passthrough(f)
        |    h(3)
        |    0
        |""".stripMargin) shouldBe 0
  }

  "if-expr returning closure — caller frees env once" in {
    llvmExitASan(
      """make_a() -> (int) -> int
        |    val s = "aa" + "bb"
        |    (x: int) -> x + len(s)
        |
        |make_b() -> (int) -> int
        |    val s = "cc" + "dd"
        |    (x: int) -> x * len(s)
        |
        |choose(b: bool) -> (int) -> int =
        |    if b then make_a() else make_b()
        |
        |main() -> int
        |    val f = choose(true)
        |    val g = choose(false)
        |    f(2)
        |    g(2)
        |    0
        |""".stripMargin) shouldBe 0
  }

  // ====================================================================
  // Structs / enums holding rc-bearing fields (FuncType + string)
  // ====================================================================

  "struct holding closure field — auto-deinit walks env (no UAF)" in {
    llvmExitASan(
      """struct Holder
        |    cb: (int) -> int
        |    name: string
        |
        |main() -> int
        |    val cap = "aa" + "bb"
        |    val h = new Holder((x: int) -> x + len(cap), "h" + "h")
        |    0
        |""".stripMargin) shouldBe 0
  }

  "enum variant holding closure — auto-deinit walks env (no UAF)" in {
    llvmExitASan(
      """enum Tagged
        |    Some(cb: (int) -> int)
        |    None
        |
        |main() -> int
        |    val cap = "xx" + "yy"
        |    val h = new Some((x: int) -> x + len(cap))
        |    0
        |""".stripMargin) shouldBe 0
  }

  "closure field reassignment — old env freed once, no UAF" in {
    llvmExitASan(
      """struct Holder
        |    cb: (int) -> int
        |
        |make() -> (int) -> int
        |    val cap = "aa" + "bb"
        |    (x: int) -> x + len(cap)
        |
        |main() -> int
        |    var h = Holder(make())
        |    h.cb = make()
        |    h.cb = make()
        |    0
        |""".stripMargin) shouldBe 0
  }

  "value-struct passed by value — caller copy survives, no UAF" in {
    llvmExitASan(
      """struct H
        |    s: string
        |
        |use(h: H) -> int = len(h.s)
        |
        |main() -> int
        |    val h = H("aa" + "bb")
        |    use(h)
        |    use(h)
        |    0
        |""".stripMargin) shouldBe 0
  }

  // ====================================================================
  // Slices, substrings, *string deref
  // ====================================================================

  "&[]string deinit walks elements — no UAF on free" in {
    llvmExitASan(
      """main() -> int
        |    val arr = new [3]string
        |    arr[0] = "aa" + "bb"
        |    arr[1] = "cc" + "dd"
        |    arr[2] = "ee" + "ff"
        |    0
        |""".stripMargin) shouldBe 0
  }

  "substring s[a:b] — own buffer freed once" in {
    llvmExitASan(
      """main() -> int
        |    val s = "abcdefgh"
        |    val t = s[2:5]
        |    val u = t[0:2]
        |    0
        |""".stripMargin) shouldBe 0
  }

  "*string deref + reassign — old buffer freed, no UAF" in {
    llvmExitASan(
      """set(p: *string)
        |    *p = "xx" + "yy"
        |
        |main() -> int
        |    var s = "aa" + "bb"
        |    set(&s)
        |    set(&s)
        |    0
        |""".stripMargin) shouldBe 0
  }

  // ====================================================================
  // Enum-with-string variant tag-dispatched cleanup
  // ====================================================================

  "enum variant string tag-dispatch — only active variant's strings freed" in {
    llvmExitASan(
      """enum E
        |    Two(a: string, b: string)
        |    One(s: string)
        |    None
        |
        |make_two() -> E = Two("aa" + "bb", "cc" + "dd")
        |make_one() -> E = One("ee" + "ff")
        |make_none() -> E = None
        |
        |main() -> int
        |    val e1 = make_two()
        |    val e2 = make_one()
        |    val e3 = make_none()
        |    0
        |""".stripMargin) shouldBe 0
  }

  // ====================================================================
  // Nested aggregate: struct holding enum holding closure
  // ====================================================================

  "struct holds enum holds closure — full-depth deinit walk" in {
    llvmExitASan(
      """enum Inner
        |    Cb(f: (int) -> int)
        |    Empty
        |
        |struct Outer
        |    inner: Inner
        |    label: string
        |
        |main() -> int
        |    val cap = "cap" + "ture"
        |    val o = new Outer(Cb((x: int) -> x + len(cap)), "label" + "1")
        |    0
        |""".stripMargin) shouldBe 0
  }
}
