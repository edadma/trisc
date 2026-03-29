package io.github.edadma.trisc

class SyslCodegenFunctionTests extends SyslCodegenHelpers {

  "function call" in {
    compileAndRun(
      """dbl(x: int) -> int = x * 2
        |
        |main() -> int = dbl(21)
        |""".stripMargin) shouldBe 42
  }

  "function with two args" in {
    compileAndRun(
      """myAdd(a: int, b: int) -> int = a + b
        |main() -> int = myAdd(20, 22)
        |""".stripMargin) shouldBe 42
  }

  "function with three args" in {
    compileAndRun(
      """sum3(a: int, b: int, c: int) -> int = a + b + c
        |main() -> int = sum3(10, 20, 12)
        |""".stripMargin) shouldBe 42
  }

  "function with four args" in {
    compileAndRun(
      """sum4(a: int, b: int, c: int, d: int) -> int = a + b + c + d
        |main() -> int = sum4(10, 11, 12, 9)
        |""".stripMargin) shouldBe 42
  }

  "abs function" in {
    compileAndRun(
      """abs_val(x: int) -> int
        |    if x < 0 then -x else x
        |
        |main() -> int = abs_val(-42)
        |""".stripMargin) shouldBe 42
  }

  "multifile abs function" in {
    val sources = Map(
      "math" ->
        """abs_val(x: int) -> int
          |    if x < 0 then -x else x
          |""".stripMargin,
      "main" ->
        """import "math"
          |
          |main() -> int = abs_val(-42)
          |""".stripMargin,
    )
    val driver = new SyslDriver
    val result = driver.compile(sources)
    val codegen = new SyslTriscCodegen
    val tofs = for unit <- result.units yield
      val asm = codegen.generate(unit.typed)
      assemble(asm, relocatable = true)
    val linked = Linker.link(Runtime.bootTof +: tofs :+ Runtime.ioTof)
    val stdout = new Stdout(Runtime.stdoutAddress)
    val ram = new RAM(0, Runtime.stdoutAddress.toInt)
    val mem = new Memory("Memory", ram, stdout)
    linked.load(mem)
    val cpu = new CPU(mem) { limit = 100000 }
    cpu.reset()
    cpu.run()
    cpu.r(1).read shouldBe 42
  }

  "multifile abs function two-stage link" in {
    val sources = Map(
      "math" ->
        """abs_val(x: int) -> int
          |    if x < 0 then -x else x
          |""".stripMargin,
      "main" ->
        """import "math"
          |
          |main() -> int = abs_val(-42)
          |""".stripMargin,
    )
    val driver = new SyslDriver
    val result = driver.compile(sources)
    val codegen = new SyslTriscCodegen
    val tofs = for unit <- result.units yield
      val asm = codegen.generate(unit.typed)
      assemble(asm, relocatable = true)
    // Stage 1: link user modules into single relocatable TOF
    val partial = Linker.link(tofs, relocatable = true)
    // Stage 2: link with runtime (what trisc run does)
    val linked = Linker.link(Seq(Runtime.bootTof, partial, Runtime.ioTof))
    val stdout = new Stdout(Runtime.stdoutAddress)
    val ram = new RAM(0, Runtime.stdoutAddress.toInt)
    val mem = new Memory("Memory", ram, stdout)
    linked.load(mem)
    val cpu = new CPU(mem) { limit = 100000 }
    cpu.reset()
    cpu.run()
    cpu.r(1).read shouldBe 42
  }

  "pass function pointer as argument" in {
    compileAndRun(
      """myDouble(x: int) -> int = x * 2
        |apply(f: func(int) -> int, x: int) -> int = f(x)
        |main() -> int = apply(myDouble, 21)
        |""".stripMargin) shouldBe 42
  }

  "function pointer call" in {
    compileAndRun(
      """myDouble(x: int) -> int = x * 2
        |main() -> int
        |    f = myDouble
        |    f(21)
        |""".stripMargin) shouldBe 42
  }

  "function pointer reassignment" in {
    compileAndRun(
      """myDouble(x: int) -> int = x * 2
        |myTriple(x: int) -> int = x * 3
        |main() -> int
        |    f = myDouble
        |    a = f(10)
        |    f = myTriple
        |    b = f(10)
        |    a + b
        |""".stripMargin) shouldBe 50
  }

  "i32 function arg preserved" in {
    compileAndRun(
      """dbl(x: int) -> int = x * 2
        |main() -> int = dbl(21)
        |""".stripMargin) shouldBe 42
  }

  "function stores result in narrow local" in {
    compileAndRun(
      """triple(x: int) -> int = x * 3
        |main() -> int
        |    var r: int = triple(14)
        |    r
        |""".stripMargin) shouldBe 42
  }

  "chained function calls" in {
    compileAndRun(
      """inc(x: int) -> int = x + 1
        |dbl(x: int) -> int = x * 2
        |main() -> int
        |    var x: int = 10
        |    x = dbl(inc(x))
        |    x
        |""".stripMargin) shouldBe 22
  }

  // ===== Calling convention =====

  "calling convention: 1 arg in r1" in {
    compileAndRun(
      """identity(x: int) -> int = x
        |main() -> int = identity(42)
        |""".stripMargin) shouldBe 42
  }

  "calling convention: 2 args in r1-r2" in {
    compileAndRun(
      """sub(a: int, b: int) -> int = a - b
        |main() -> int = sub(50, 8)
        |""".stripMargin) shouldBe 42
  }

  "calling convention: 3 args in r1-r3" in {
    compileAndRun(
      """sum3(a: int, b: int, c: int) -> int = a + b + c
        |main() -> int = sum3(10, 20, 12)
        |""".stripMargin) shouldBe 42
  }

  "calling convention: 4 args (3 regs + 1 stack)" in {
    compileAndRun(
      """sum4(a: int, b: int, c: int, d: int) -> int = a + b + c + d
        |main() -> int = sum4(10, 11, 12, 9)
        |""".stripMargin) shouldBe 42
  }

  "calling convention: 5 args (3 regs + 2 stack)" in {
    compileAndRun(
      """sum5(a: int, b: int, c: int, d: int, e: int) -> int = a + b + c + d + e
        |main() -> int = sum5(5, 6, 7, 8, 16)
        |""".stripMargin) shouldBe 42
  }

  "calling convention: 6 args (3 regs + 3 stack)" in {
    compileAndRun(
      """sum6(a: int, b: int, c: int, d: int, e: int, f: int) -> int = a + b + c + d + e + f
        |main() -> int = sum6(1, 2, 3, 4, 5, 27)
        |""".stripMargin) shouldBe 42
  }

  "calling convention: cross-module 2 args" in {
    compileMultiAndRun(Map(
      "lib" ->
        """sub(a: int, b: int) -> int = a - b
          |""".stripMargin,
      "main" ->
        """import "lib"
          |main() -> int = sub(50, 8)
          |""".stripMargin
    )) shouldBe 42
  }

  "calling convention: cross-module 5 args" in {
    compileMultiAndRun(Map(
      "lib" ->
        """sum5(a: int, b: int, c: int, d: int, e: int) -> int = a + b + c + d + e
          |""".stripMargin,
      "main" ->
        """import "lib"
          |main() -> int = sum5(5, 6, 7, 8, 16)
          |""".stripMargin
    )) shouldBe 42
  }

  "calling convention: multiple calls preserve correctness" in {
    compileAndRun(
      """add(a: int, b: int) -> int = a + b
        |main() -> int
        |    val x = add(10, 20)
        |    val y = add(5, 7)
        |    x + y
        |""".stripMargin) shouldBe 42
  }

  // ===== 5-arg tests =====

  "5-arg extern function called 3 times from same function" in {
    compileMultiAndRun(Map(
      "lib" ->
        """var total = 0
          |
          |add5(a: int, b: int, c: int, d: int, e: int)
          |    total = total + a + b + c + d + e
          |
          |get_total() -> int = total
          |""".stripMargin,
      "main" ->
        """import "lib"
          |
          |main() -> int
          |    add5(1, 2, 3, 4, 5)
          |    add5(10, 20, 30, 40, 50)
          |    add5(100, 200, 300, 400, 500)
          |    get_total()
          |""".stripMargin
    )) shouldBe 1665
  }

  "5-arg function with pointer arg called 3 times" in {
    compileMultiAndRun(Map(
      "lib" ->
        """struct Item
          |    value: int
          |    name: *byte
          |    pri: int
          |
          |var items: [8]Item
          |var count = 0
          |
          |add_item(entry: int, usp: int, ssp: int, name: *byte, pri: int)
          |    items[count].value = entry
          |    items[count].name = name
          |    items[count].pri = pri
          |    count = count + 1
          |
          |get_count() -> int = count
          |""".stripMargin,
      "main" ->
        """import "lib"
          |
          |main() -> int
          |    add_item(100, 0x6000, 0x5000, "a", 0)
          |    add_item(200, 0x8000, 0x7000, "b", 1)
          |    add_item(300, 0xA000, 0x9000, "c", 2)
          |    get_count()
          |""".stripMargin
    )) shouldBe 3
  }

  "5-arg with ptr arg called twice from same function" in {
    compileMultiAndRun(Map(
      "lib" ->
        """var count = 0
          |
          |do_thing(a: int, b: int, c: int, name: *byte, pri: int)
          |    count = count + pri + 1
          |
          |get_count() -> int = count
          |""".stripMargin,
      "main" ->
        """import "lib"
          |
          |main() -> int
          |    do_thing(1, 2, 3, 99, 10)
          |    do_thing(4, 5, 6, 88, 20)
          |    get_count()
          |""".stripMargin
    )) shouldBe 32
  }

  "5-arg extern function called 2 times works" in {
    compileMultiAndRun(Map(
      "lib" ->
        """var total = 0
          |
          |add5(a: int, b: int, c: int, d: int, e: int)
          |    total = total + a + b + c + d + e
          |
          |get_total() -> int = total
          |""".stripMargin,
      "main" ->
        """import "lib"
          |
          |main() -> int
          |    add5(1, 2, 3, 4, 5)
          |    add5(10, 20, 30, 40, 50)
          |    get_total()
          |""".stripMargin
    )) shouldBe 165
  }

  "queue enqueue at multiple priorities" in {
    compileAndRun(
      """val EMPTY = -1
        |
        |var head: [4]int
        |var tail: [4]int
        |
        |init()
        |    var i = 0
        |    while i < 4
        |        head[i] = EMPTY
        |        tail[i] = EMPTY
        |        i += 1
        |
        |enqueue(pri: int, idx: int)
        |    if head[pri] == EMPTY
        |        head[pri] = idx
        |        tail[pri] = idx
        |    else
        |        tail[pri] = idx
        |
        |main() -> int
        |    init()
        |    enqueue(0, 10)
        |    enqueue(1, 20)
        |    enqueue(2, 30)
        |    head[0] + head[1] + head[2]
        |""".stripMargin) shouldBe 60
  }

  "array write at index 2+ via function call" in {
    compileAndRun(
      """val EMPTY = -1
        |var head: [4]int
        |var mask = 0
        |
        |init()
        |    var i = 0
        |    while i < 4
        |        head[i] = EMPTY
        |        i += 1
        |
        |enqueue(pri: int, idx: int)
        |    head[pri] = idx
        |    mask = mask | (1 << pri)
        |
        |main() -> int
        |    init()
        |    enqueue(0, 10)
        |    enqueue(1, 20)
        |    enqueue(2, 30)
        |    head[0] + head[1] + head[2]
        |""".stripMargin) shouldBe 60
  }

  "cross-module array write at index 2+ with 5-arg creator" in {
    compileMultiAndRun(Map(
      "kernel" ->
        """val EMPTY = -1
          |var head: [4]int
          |var mask = 0
          |var count = 0
          |
          |init()
          |    var i = 0
          |    while i < 4
          |        head[i] = EMPTY
          |        i += 1
          |
          |enqueue(pri: int, idx: int)
          |    head[pri] = idx
          |    mask = mask | (1 << pri)
          |
          |create(a: int, b: int, c: int, name: *byte, pri: int)
          |    val idx = count
          |    count = count + 1
          |    enqueue(pri, idx)
          |
          |get_head(pri: int) -> int = head[pri]
          |get_mask() -> int = mask
          |""".stripMargin,
      "main" ->
        """import "kernel"
          |
          |main() -> int
          |    init()
          |    create(1, 2, 3, "a", 0)
          |    create(4, 5, 6, "b", 1)
          |    create(7, 8, 9, "c", 2)
          |    get_head(0) * 100 + get_head(1) * 10 + get_head(2)
          |""".stripMargin
    )) shouldBe 12  // head[0]=0, head[1]=1, head[2]=2
  }

  "cross-module array write at index" in {
    compileMultiAndRun(Map(
      "lib" ->
        """var arr: [4]int
          |
          |set_arr(idx: int, val_: int)
          |    arr[idx] = val_
          |
          |get_arr(idx: int) -> int = arr[idx]
          |""".stripMargin,
      "main" ->
        """import "lib"
          |
          |main() -> int
          |    set_arr(0, 10)
          |    set_arr(1, 20)
          |    set_arr(2, 30)
          |    get_arr(0) + get_arr(1) + get_arr(2)
          |""".stripMargin
    )) shouldBe 60
  }

  "cross-module queue enqueue at multiple priorities" in {
    compileMultiAndRun(Map(
      "queue" ->
        """var head: [4]int
          |var tail: [4]int
          |
          |init_q()
          |    var i = 0
          |    while i < 4
          |        head[i] = -1
          |        tail[i] = -1
          |        i += 1
          |
          |enqueue(pri: int, idx: int)
          |    if head[pri] == -1
          |        head[pri] = idx
          |        tail[pri] = idx
          |    else
          |        tail[pri] = idx
          |
          |get_head(pri: int) -> int = head[pri]
          |""".stripMargin,
      "main" ->
        """import "queue"
          |
          |main() -> int
          |    init_q()
          |    enqueue(0, 10)
          |    enqueue(1, 20)
          |    enqueue(2, 30)
          |    get_head(0) + get_head(1) + get_head(2)
          |""".stripMargin
    )) shouldBe 60
  }

  "cross-module full enqueue pattern at priority 2" in {
    compileMultiAndRun(Map(
      "kernel" ->
        """val EMPTY = -1
          |var head: [4]int
          |var tail: [4]int
          |var mask = 0
          |var count = 0
          |
          |struct Task
          |    pri: int
          |    next: int
          |
          |var tasks: [8]Task
          |
          |init()
          |    var i = 0
          |    while i < 4
          |        head[i] = EMPTY
          |        tail[i] = EMPTY
          |        i += 1
          |    mask = 0
          |
          |enqueue(pri: int, idx: int)
          |    tasks[idx].next = EMPTY
          |    if head[pri] == EMPTY
          |        head[pri] = idx
          |        tail[pri] = idx
          |    else
          |        tasks[tail[pri]].next = idx
          |        tail[pri] = idx
          |    mask = mask | (1 << pri)
          |
          |create(a: int, b: int, c: int, name: *byte, pri: int)
          |    val idx = count
          |    tasks[idx].pri = pri
          |    tasks[idx].next = EMPTY
          |    count = count + 1
          |    enqueue(pri, idx)
          |
          |get_head(pri: int) -> int = head[pri]
          |get_mask() -> int = mask
          |""".stripMargin,
      "main" ->
        """import "kernel"
          |
          |main() -> int
          |    init()
          |    create(1, 2, 3, "a", 0)
          |    create(4, 5, 6, "b", 1)
          |    create(7, 8, 9, "c", 2)
          |    get_mask()
          |""".stripMargin
    )) shouldBe 7  // bits 0,1,2 set
  }

  "single-module array write at index" in {
    compileAndRun(
      """var arr: [4]int
        |
        |set_arr(idx: int, val_: int)
        |    arr[idx] = val_
        |
        |main() -> int
        |    set_arr(0, 10)
        |    set_arr(1, 20)
        |    set_arr(2, 30)
        |    arr[0] + arr[1] + arr[2]
        |""".stripMargin) shouldBe 60
  }

  // ===== Additional arg width tests =====

  "two i32 args" in {
    compileAndRun(
      """myAdd(a: int, b: int) -> int = a + b
        |main() -> int = myAdd(20, 22)
        |""".stripMargin) shouldBe 42
  }

  "three i32 args" in {
    compileAndRun(
      """mySum(a: int, b: int, c: int) -> int = a + b + c
        |main() -> int = mySum(10, 20, 12)
        |""".stripMargin) shouldBe 42
  }

  "four i32 args" in {
    compileAndRun(
      """mySum4(a: int, b: int, c: int, d: int) -> int = a + b + c + d
        |main() -> int = mySum4(10, 11, 12, 9)
        |""".stripMargin) shouldBe 42
  }
}
