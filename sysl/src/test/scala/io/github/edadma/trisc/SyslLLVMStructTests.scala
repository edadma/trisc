package io.github.edadma.trisc

class SyslLLVMStructTests extends SyslLLVMTestHelpers {

  // ===== Basic struct construction and field access =====

  "struct construction and field read" in {
    llvmExit(
      """struct Point
        |    x: int
        |    y: int
        |
        |main() -> int
        |    p = Point(3, 4)
        |    p.x + p.y
        |""".stripMargin) shouldBe 7
  }

  "struct field assignment" in {
    llvmExit(
      """struct Point
        |    x: int
        |    y: int
        |
        |main() -> int
        |    var p = Point(0, 0)
        |    p.x = 10
        |    p.y = 32
        |    p.x + p.y
        |""".stripMargin) shouldBe 42
  }

  "struct zero-initialized" in {
    llvmExit(
      """struct Pair
        |    a: int
        |    b: int
        |
        |main() -> int
        |    p = Pair(0, 0)
        |    p.a + p.b
        |""".stripMargin) shouldBe 0
  }

  "struct with mixed types" in {
    llvmOutput(
      """struct Record
        |    name: string
        |    age: int
        |
        |main() -> int
        |    r = Record("Alice", 30)
        |    puts(r.name)
        |    println(r.age)
        |    0
        |""".stripMargin) shouldBe "Alice\n30"
  }

  "struct passed to function" in {
    llvmExit(
      """struct Point
        |    x: int
        |    y: int
        |
        |sum(p: Point) -> int = p.x + p.y
        |
        |main() -> int
        |    p = Point(20, 22)
        |    sum(p)
        |""".stripMargin) shouldBe 42
  }

  "struct with interpolation" in {
    llvmOutput(
      """struct Vec2
        |    x: int
        |    y: int
        |
        |main() -> int
        |    v = Vec2(3, 4)
        |    puts(s"${v.x}, ${v.y}")
        |    0
        |""".stripMargin) shouldBe "3, 4"
  }

  "nested struct field read" in {
    llvmExit(
      """struct Inner
        |    v: int
        |
        |struct Outer
        |    a: Inner
        |    b: int
        |
        |main() -> int
        |    o = Outer(Inner(42), 0)
        |    o.a.v
        |""".stripMargin) shouldBe 42
  }

  // ===== Method self-mutation =====

  "method mutates self field" in {
    llvmOutput(
      """struct Counter
        |    value: int
        |
        |Counter.inc()
        |    self.value = self.value + 1
        |
        |main()
        |    var c = Counter(0)
        |    c.inc()
        |    c.inc()
        |    c.inc()
        |    println(c.value)
        |""".stripMargin) shouldBe "3"
  }

  "method compound assign self field" in {
    llvmOutput(
      """struct Counter
        |    value: int
        |
        |Counter.add(n: int)
        |    self.value += n
        |
        |main()
        |    var c = Counter(10)
        |    c.add(5)
        |    c.add(3)
        |    println(c.value)
        |""".stripMargin) shouldBe "18"
  }

  // ===== Append backref preservation =====

  "append preserves backref - no grow" in {
    llvmExit(
      """make_list() -> []int
        |    val _a = new [16]int
        |    var xs = _a[:0]
        |    xs = append(xs, 10)
        |    xs = append(xs, 20)
        |    xs = append(xs, 30)
        |    xs
        |
        |main() -> int
        |    val xs = make_list()
        |    if len(xs) != 3 then return 0
        |    if xs[0] != 10 then return 0
        |    if xs[1] != 20 then return 0
        |    if xs[2] != 30 then return 0
        |    1
        |""".stripMargin) shouldBe 1
  }

  "append preserves backref - two calls" in {
    llvmExit(
      """make_list(n: int) -> []int
        |    val _a = new [16]int
        |    var xs = _a[:0]
        |    for var i = 0; i < n; i++
        |        xs = append(xs, i * 10)
        |    xs
        |
        |main() -> int
        |    val a = make_list(3)
        |    val b = make_list(2)
        |    if a[0] != 0 then return 0
        |    if a[2] != 20 then return 0
        |    if b[0] != 0 then return 0
        |    if b[1] != 10 then return 0
        |    if len(a) != 3 then return 0
        |    if len(b) != 2 then return 0
        |    1
        |""".stripMargin) shouldBe 1
  }

  "struct with slice field returned from function" in {
    llvmExit(
      """struct Container
        |    items: []int
        |    count: int
        |
        |make_container() -> Container
        |    val _a = new [8]int
        |    var items = _a[:0]
        |    items = append(items, 42)
        |    items = append(items, 99)
        |    Container(items, 2)
        |
        |main() -> int
        |    val c = make_container()
        |    if c.count != 2 then return 0
        |    if c.items[0] != 42 then return 0
        |    if c.items[1] != 99 then return 0
        |    1
        |""".stripMargin) shouldBe 1
  }

  // ===== IR validity: argument types must match declarations =====

  "string iteration byte passed to char param is widened" in {
    // for c in s iterates as bytes (i8), but a char param is i32.
    // The codegen must zext i8 -> i32 at the call site.
    val ir = compileLLVM(
      """extern my_putc(c: char)
        |
        |my_puts(s: string) = for c in s do my_putc(c)
        |
        |main() -> int
        |    my_puts("hi")
        |    0
        |""".stripMargin)
    // Verify: every call to @my_putc must pass i32, not i8
    val calls = ir.linesIterator.filter(_.contains("call")).filter(_.contains("@my_putc")).toList
    for line <- calls do
      assert(!line.contains("i8 %"), s"my_putc called with i8 instead of i32: $line")
  }

  "nested struct-with-slice in slice array" in {
    llvmExit(
      """struct Item
        |    tag: int
        |    data: []int
        |
        |make_data(n: int) -> []int
        |    val _d = new [8]int
        |    var d = _d[:0]
        |    for var i = 0; i < n; i++
        |        d = append(d, i * 100)
        |    d
        |
        |main() -> int
        |    val _items = new [8]Item
        |    var items = _items[:0]
        |    val d1 = make_data(3)
        |    items = append(items, Item(1, d1))
        |    val d2 = make_data(2)
        |    items = append(items, Item(2, d2))
        |    if len(items) != 2 then return 0
        |    if items[0].tag != 1 then return 0
        |    if items[0].data[0] != 0 then return 0
        |    if items[0].data[2] != 200 then return 0
        |    if items[1].tag != 2 then return 0
        |    if items[1].data[1] != 100 then return 0
        |    1
        |""".stripMargin) shouldBe 1
  }

  // ===== Explicit *ptr to pass struct by value; self is the only implicit deref =====

  "explicit *ptr passes struct by value" in {
    llvmExit(
      """struct Point
        |    x: int
        |    y: int
        |
        |sum(p: Point) -> int = p.x + p.y
        |
        |main() -> int
        |    var p = Point(20, 22)
        |    val ptr: *Point = &p
        |    sum(*ptr)
        |""".stripMargin) shouldBe 42
  }

  "self auto-derefs in method calling standalone function" in {
    llvmExit(
      """struct Point
        |    x: int
        |    y: int
        |
        |sum(p: Point) -> int = p.x + p.y
        |
        |Point.total() -> int = sum(self)
        |
        |main() -> int
        |    p = Point(20, 22)
        |    p.total()
        |""".stripMargin) shouldBe 42
  }

  "explicit *ptr produces a copy of pointee" in {
    llvmExit(
      """struct Point
        |    x: int
        |    y: int
        |
        |sum(p: Point) -> int = p.x + p.y
        |
        |main() -> int
        |    var p = Point(20, 22)
        |    val ptr: *Point = &p
        |    sum(*ptr)
        |""".stripMargin) shouldBe 42
  }
}
