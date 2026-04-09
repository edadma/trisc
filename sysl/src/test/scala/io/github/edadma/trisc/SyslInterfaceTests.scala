package io.github.edadma.trisc

class SyslInterfaceTests extends SyslTestHelpers {

  // ===== Basic interface declaration and satisfaction =====

  "struct satisfies interface implicitly" in {
    eval(
      """interface Greeter
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
        |""".stripMargin) shouldBe 42
  }

  "interface with parameters" in {
    eval(
      """interface Adder
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
        |""".stripMargin) shouldBe 42
  }

  "multiple methods in interface" in {
    eval(
      """interface Math
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
        |""".stripMargin) shouldBe 42
  }

  // ===== Interface variable =====

  "assign struct to interface variable" in {
    eval(
      """interface HasValue
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
        |""".stripMargin) shouldBe 42
  }

  // ===== Multiple types satisfying same interface =====

  "different types satisfy same interface" in {
    eval(
      """interface Sizer
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
        |""".stripMargin) shouldBe 42
  }

  // ===== Interface embedding =====

  "embedded interface" in {
    eval(
      """interface Reader
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
        |""".stripMargin) shouldBe 42
  }

  // ===== Void return =====

  "interface method with void return" in {
    eval(
      """interface Setter
        |    set(x: int)
        |
        |struct Store
        |    val_: int
        |
        |Store.set(x: int)
        |    self.val_ = x
        |
        |do_set(s: Setter, x: int)
        |    s.set(x)
        |
        |main() -> int
        |    var st = Store(0)
        |    do_set(st, 42)
        |    st.val_
        |""".stripMargin) shouldBe 42
  }

  // ===== Cross-unit (module-mangled methods) =====

  "interface works across compilation units" in {
    evalWithLibs(
      Map(
        "mylib/io/io" ->
          """module mylib.io
            |
            |interface Reader
            |    read() -> int
            |
            |struct ByteReader
            |    value: int
            |
            |ByteReader.read() -> int = self.value
            |
            |use_reader(r: Reader) -> int = r.read()
            |""".stripMargin,
      ),
      """import mylib.io.*
        |main() -> int
        |    var br = ByteReader(42)
        |    use_reader(br)
        |""".stripMargin
    ) shouldBe 42
  }
}
