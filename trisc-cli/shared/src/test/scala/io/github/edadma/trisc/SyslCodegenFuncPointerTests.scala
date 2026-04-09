package io.github.edadma.trisc

class SyslCodegenFuncPointerTests extends SyslCodegenHelpers {

  // ===== Basic function pointer =====

  "assign function to variable and call" in {
    compileAndRun(
      """dbl(x: int) -> int = x * 2
        |main() -> int
        |    f: (int) -> int = dbl
        |    f(21)
        |""".stripMargin) shouldBe 42
  }

  "function pointer with inferred type" in {
    compileAndRun(
      """dbl(x: int) -> int = x * 2
        |main() -> int
        |    f = dbl
        |    f(21)
        |""".stripMargin) shouldBe 42
  }

  // ===== Passing function pointers as arguments =====

  "pass function as argument" in {
    compileAndRun(
      """dbl(x: int) -> int = x * 2
        |apply(f: (int) -> int, x: int) -> int = f(x)
        |main() -> int = apply(dbl, 21)
        |""".stripMargin) shouldBe 42
  }

  "pass different functions" in {
    compileAndRun(
      """dbl(x: int) -> int = x * 2
        |triple(x: int) -> int = x * 3
        |apply(f: (int) -> int, x: int) -> int = f(x)
        |main() -> int = apply(dbl, 10) + apply(triple, 10)
        |""".stripMargin) shouldBe 50
  }

  // ===== Reassignment =====

  "reassign function pointer" in {
    compileAndRun(
      """dbl(x: int) -> int = x * 2
        |triple(x: int) -> int = x * 3
        |main() -> int
        |    f = dbl
        |    a = f(10)
        |    f = triple
        |    b = f(10)
        |    a + b
        |""".stripMargin) shouldBe 50
  }

  // ===== Two-arg function pointer =====

  "two-arg function pointer" in {
    compileAndRun(
      """myAdd(a: int, b: int) -> int = a + b
        |apply2(f: (int, int) -> int, a: int, b: int) -> int = f(a, b)
        |main() -> int = apply2(myAdd, 20, 22)
        |""".stripMargin) shouldBe 42
  }

  // ===== Indirect calls on arbitrary expressions =====

  "call function pointer from array index" in {
    compileAndRun(
      """dbl(x: int) -> int = x * 2
        |triple(x: int) -> int = x * 3
        |main() -> int
        |    var funcs: [2](int) -> int
        |    funcs[0] = dbl
        |    funcs[1] = triple
        |    funcs[0](10) + funcs[1](10)
        |""".stripMargin) shouldBe 50
  }

  "call function returned by another function" in {
    compileAndRun(
      """dbl(x: int) -> int = x * 2
        |getFunc() -> (int) -> int = dbl
        |main() -> int = getFunc()(21)
        |""".stripMargin) shouldBe 42
  }

  "chain: function returning function pointer called immediately" in {
    compileAndRun(
      """add(a: int, b: int) -> int = a + b
        |getOp() -> (int, int) -> int = add
        |main() -> int = getOp()(20, 22)
        |""".stripMargin) shouldBe 42
  }

  "call dereferenced function pointer" in {
    compileAndRun(
      """dbl(x: int) -> int = x * 2
        |main() -> int
        |    f: (int) -> int = dbl
        |    fp = &f
        |    (*fp)(21)
        |""".stripMargin) shouldBe 42
  }

  "indirect call on struct field" in {
    compileAndRun(
      """struct Ops
        |    apply: (int) -> int
        |dbl(x: int) -> int = x * 2
        |main() -> int
        |    var ops: Ops
        |    ops.apply = dbl
        |    ops.apply(21)
        |""".stripMargin) shouldBe 42
  }

  "call function-typed field on indexed struct" in {
    compileAndRun(
      """struct Cmd
        |    handler: (int) -> int
        |dbl(x: int) -> int = x * 2
        |triple(x: int) -> int = x * 3
        |main() -> int
        |    var cmds: [2]Cmd
        |    cmds[0].handler = dbl
        |    cmds[1].handler = triple
        |    cmds[0].handler(10) + cmds[1].handler(10)
        |""".stripMargin) shouldBe 50
  }

  // ===== def auto-call + indirect call =====

  "def zero-arg returns function pointer, call with args" in {
    compileAndRun(
      """add1(x: int) -> int = x + 1
        |def get_add1 = add1
        |main() -> int = get_add1(41)
        |""".stripMargin) shouldBe 42
  }

  "def zero-arg returns function pointer, assign then call" in {
    compileAndRun(
      """dbl(x: int) -> int = x * 2
        |def get_dbl = dbl
        |main() -> int
        |    val f = get_dbl
        |    f(21)
        |""".stripMargin) shouldBe 42
  }

  "def zero-arg returns function pointer, pass to higher-order" in {
    compileAndRun(
      """inc(x: int) -> int = x + 1
        |def get_inc = inc
        |apply(f: (int) -> int, x: int) -> int = f(x)
        |main() -> int = apply(get_inc, 41)
        |""".stripMargin) shouldBe 42
  }

  "select function pointer at runtime" in {
    compileAndRun(
      """dbl(x: int) -> int = x * 2
        |triple(x: int) -> int = x * 3
        |main() -> int
        |    var funcs: [2](int) -> int
        |    funcs[0] = dbl
        |    funcs[1] = triple
        |    i = 1
        |    funcs[i](14)
        |""".stripMargin) shouldBe 42
  }
}
