package io.github.edadma.trisc

class SyslCodegenInterfaceTests extends SyslCodegenHelpers {

  // Interface boxing heap-allocates a copy of the concrete value, so tests need malloc
  private val allocSource = scala.io.Source.fromFile("posix/stdlib/alloc.sysl").mkString
  private val stringSource = scala.io.Source.fromFile("posix/string/string.sysl").mkString
  private val ctypeSource = scala.io.Source.fromFile("posix/ctype/ctype.sysl").mkString

  private def sbrkModule(heapSize: Int = 16384): String =
    s"""module posix.unistd
       |
       |var _heap: [$heapSize]i8
       |var _brk: *i8 = *i8(0)
       |var _brk_initialized = false
       |
       |sbrk(increment: int) -> *i8
       |    if !_brk_initialized
       |        _brk = &_heap
       |        _brk_initialized = true
       |
       |    if increment == 0 then return _brk
       |
       |    val old_brk = _brk
       |    val new_brk = old_brk + increment
       |
       |    if i64(new_brk) > i64(&_heap + $heapSize) then return *i8(-1)
       |
       |    _brk = new_brk
       |    old_brk
       |""".stripMargin

  private def allocSources(mainSource: String): Map[String, String] =
    Map(
      "posix/unistd/sbrk" -> sbrkModule(),
      "posix/string/string" -> stringSource,
      "posix/ctype/ctype" -> ctypeSource,
      "posix/stdlib/alloc" -> allocSource,
      "main" -> mainSource,
    )

  // ===== Basic interface declaration and satisfaction =====

  "struct satisfies interface implicitly" in {
    compileMultiAndRun(allocSources(
      """import posix.stdlib.*
        |
        |interface Greeter
        |    greet() -> int
        |
        |struct Dog
        |    age: int
        |
        |Dog.greet() -> int = self.age
        |
        |hello(g: Greeter) -> int = g.greet()
        |
        |main() -> int
        |    var d = Dog(42)
        |    hello(d)
        |""".stripMargin)) shouldBe 42
  }

  "interface with parameters" in {
    compileMultiAndRun(allocSources(
      """import posix.stdlib.*
        |
        |interface Adder
        |    add(x: int) -> int
        |
        |struct Offset
        |    base: int
        |
        |Offset.add(x: int) -> int = self.base + x
        |
        |apply_add(a: Adder, x: int) -> int = a.add(x)
        |
        |main() -> int
        |    var o = Offset(10)
        |    apply_add(o, 32)
        |""".stripMargin)) shouldBe 42
  }

  "multiple methods in interface" in {
    compileMultiAndRun(allocSources(
      """import posix.stdlib.*
        |
        |interface Math
        |    dbl(x: int) -> int
        |    neg(x: int) -> int
        |
        |struct Calculator
        |    factor: int
        |
        |Calculator.dbl(x: int) -> int = x * self.factor
        |Calculator.neg(x: int) -> int = -x
        |
        |compute(m: Math) -> int = m.dbl(21) + m.neg(0)
        |
        |main() -> int
        |    var c = Calculator(2)
        |    compute(c)
        |""".stripMargin)) shouldBe 42
  }

  // ===== Interface variable =====

  "assign struct to interface variable" in {
    compileMultiAndRun(allocSources(
      """import posix.stdlib.*
        |
        |interface HasValue
        |    value() -> int
        |
        |struct Box
        |    v: int
        |
        |Box.value() -> int = self.v
        |
        |main() -> int
        |    var b = Box(42)
        |    val h: HasValue = b
        |    h.value()
        |""".stripMargin)) shouldBe 42
  }

  // ===== Multiple types satisfying same interface =====

  "different types satisfy same interface" in {
    compileMultiAndRun(allocSources(
      """import posix.stdlib.*
        |
        |interface Sizer
        |    size() -> int
        |
        |struct Small
        |    x: int
        |
        |struct Big
        |    x: int
        |    y: int
        |
        |Small.size() -> int = 1
        |Big.size() -> int = 2
        |
        |get_size(s: Sizer) -> int = s.size()
        |
        |main() -> int
        |    var a = Small(0)
        |    var b = Big(0, 0)
        |    get_size(a) * 40 + get_size(b)
        |""".stripMargin)) shouldBe 42
  }

  // ===== Interface embedding =====

  "embedded interface" in {
    compileMultiAndRun(allocSources(
      """import posix.stdlib.*
        |
        |interface Reader
        |    read() -> int
        |
        |interface Writer
        |    write(x: int) -> int
        |
        |interface ReadWriter
        |    Reader
        |    Writer
        |
        |struct Buffer
        |    data: int
        |
        |Buffer.read() -> int = self.data
        |Buffer.write(x: int) -> int
        |    self.data = x
        |    x
        |
        |use_rw(rw: ReadWriter, val_: int) -> int
        |    rw.write(val_)
        |    rw.read()
        |
        |main() -> int
        |    var buf = Buffer(0)
        |    use_rw(buf, 42)
        |""".stripMargin)) shouldBe 42
  }

  // ===== Void return =====

  "interface method with void return" in {
    compileMultiAndRun(allocSources(
      """import posix.stdlib.*
        |
        |interface Setter
        |    set(x: int)
        |    get() -> int
        |
        |struct Store
        |    val_: int
        |
        |Store.set(x: int)
        |    self.val_ = x
        |
        |Store.get() -> int = self.val_
        |
        |do_set_and_get(s: Setter, x: int) -> int
        |    s.set(x)
        |    s.get()
        |
        |main() -> int
        |    var st = Store(0)
        |    do_set_and_get(st, 42)
        |""".stripMargin)) shouldBe 42
  }

  // ===== Pointer-to-struct boxing =====

  "box pointer-to-struct into interface" in {
    compileAndRun(
      """interface HasValue
        |    value() -> int
        |
        |struct Box
        |    v: int
        |
        |Box.value() -> int = self.v
        |
        |get_val(h: HasValue) -> int = h.value()
        |
        |main() -> int
        |    var b = Box(42)
        |    get_val(&b)
        |""".stripMargin) shouldBe 42
  }
}
