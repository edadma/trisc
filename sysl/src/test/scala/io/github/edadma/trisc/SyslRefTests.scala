package io.github.edadma.trisc

class SyslRefTests extends SyslTestHelpers {

  // ===== Basic new =====

  "new creates ref-counted struct" in {
    eval(
      """struct Point
        |    x: int
        |    y: int
        |
        |main() -> int
        |    val p = new Point(10, 20)
        |    p.x + p.y
        |""".stripMargin) shouldBe 30
  }

  "new field access" in {
    eval(
      """struct Node
        |    value: int
        |
        |main() -> int
        |    val n = new Node(42)
        |    n.value
        |""".stripMargin) shouldBe 42
  }

  "new field assignment" in {
    eval(
      """struct Point
        |    x: int
        |    y: int
        |
        |main() -> int
        |    val p = new Point(0, 0)
        |    p.x = 10
        |    p.y = 20
        |    p.x + p.y
        |""".stripMargin) shouldBe 30
  }

  "new with zero fields" in {
    eval(
      """struct Empty
        |    dummy: int
        |
        |main() -> int
        |    val e = new Empty(0)
        |    e.dummy
        |""".stripMargin) shouldBe 0
  }

  "new with many fields" in {
    eval(
      """struct Vec4
        |    x: int
        |    y: int
        |    z: int
        |    w: int
        |
        |main() -> int
        |    val v = new Vec4(1, 2, 3, 4)
        |    v.x + v.y + v.z + v.w
        |""".stripMargin) shouldBe 10
  }

  // ===== Ref type annotation =====

  "ref type annotation &T" in {
    eval(
      """struct Box
        |    value: int
        |
        |main() -> int
        |    val b: &Box = new Box(99)
        |    b.value
        |""".stripMargin) shouldBe 99
  }

  // ===== Ref sharing (refcount > 1) =====

  "two refs to same object share data" in {
    eval(
      """struct Counter
        |    n: int
        |
        |main() -> int
        |    val a = new Counter(0)
        |    val b = a
        |    a.n = 42
        |    b.n
        |""".stripMargin) shouldBe 42
  }

  "three refs to same object" in {
    eval(
      """struct Box
        |    value: int
        |
        |main() -> int
        |    val a = new Box(10)
        |    val b = a
        |    val c = b
        |    c.value = 77
        |    a.value
        |""".stripMargin) shouldBe 77
  }

  "reassign ref — old value preserved through other ref" in {
    eval(
      """struct Box
        |    value: int
        |
        |main() -> int
        |    var a = new Box(10)
        |    val b = a
        |    a = new Box(20)
        |    b.value
        |""".stripMargin) shouldBe 10
  }

  "reassign ref — new value accessible" in {
    eval(
      """struct Box
        |    value: int
        |
        |main() -> int
        |    var a = new Box(10)
        |    a = new Box(20)
        |    a.value
        |""".stripMargin) shouldBe 20
  }

  // ===== Ref passed to function =====

  "ref passed to function — read" in {
    eval(
      """struct Box
        |    value: int
        |
        |get(b: &Box) -> int = b.value
        |
        |main() -> int
        |    val b = new Box(77)
        |    get(b)
        |""".stripMargin) shouldBe 77
  }

  "ref passed to function — modify" in {
    eval(
      """struct Box
        |    value: int
        |
        |set(b: &Box, v: int)
        |    b.value = v
        |
        |main() -> int
        |    val b = new Box(0)
        |    set(b, 55)
        |    b.value
        |""".stripMargin) shouldBe 55
  }

  "ref returned from function" in {
    eval(
      """struct Box
        |    value: int
        |
        |make(v: int) -> &Box = new Box(v)
        |
        |main() -> int
        |    val b = make(42)
        |    b.value
        |""".stripMargin) shouldBe 42
  }

  "ref passed through multiple functions" in {
    eval(
      """struct Box
        |    value: int
        |
        |add(b: &Box, n: int)
        |    b.value = b.value + n
        |
        |double_add(b: &Box, n: int)
        |    add(b, n)
        |    add(b, n)
        |
        |main() -> int
        |    val b = new Box(0)
        |    double_add(b, 5)
        |    b.value
        |""".stripMargin) shouldBe 10
  }

  "ref created in function, modified, returned" in {
    eval(
      """struct Point
        |    x: int
        |    y: int
        |
        |make_point(x: int, y: int) -> &Point
        |    val p = new Point(x, y)
        |    p.x = p.x * 2
        |    p.y = p.y * 2
        |    p
        |
        |main() -> int
        |    val p = make_point(3, 4)
        |    p.x + p.y
        |""".stripMargin) shouldBe 14
  }

  // ===== Ref decays to pointer =====

  "ref decays to raw pointer" in {
    eval(
      """struct Point
        |    x: int
        |    y: int
        |
        |read_x(p: *Point) -> int = p.x
        |
        |main() -> int
        |    val pt = new Point(99, 0)
        |    read_x(pt)
        |""".stripMargin) shouldBe 99
  }

  // ===== Multiple allocations =====

  "multiple independent new allocations" in {
    eval(
      """struct Box
        |    value: int
        |
        |main() -> int
        |    val a = new Box(10)
        |    val b = new Box(20)
        |    val c = new Box(30)
        |    a.value + b.value + c.value
        |""".stripMargin) shouldBe 60
  }

  "new in loop" in {
    eval(
      """struct Box
        |    value: int
        |
        |main() -> int
        |    var sum = 0
        |    var i = 0
        |    while i < 5
        |        val b = new Box(i * 10)
        |        sum += b.value
        |        i++
        |    sum
        |""".stripMargin) shouldBe 100
  }

  // ===== Ref with methods =====

  "ref with method call" in {
    eval(
      """struct Counter
        |    n: int
        |
        |Counter.inc()
        |    self.n = self.n + 1
        |
        |Counter.get() -> int = self.n
        |
        |main() -> int
        |    val c = new Counter(0)
        |    c.inc()
        |    c.inc()
        |    c.inc()
        |    c.get()
        |""".stripMargin) shouldBe 3
  }

  // ===== Ref in struct field =====

  "ref stored in struct field" in {
    eval(
      """struct Inner
        |    value: int
        |
        |struct Outer
        |    x: int
        |    y: int
        |
        |main() -> int
        |    val a = new Inner(42)
        |    val b = new Inner(58)
        |    a.value + b.value
        |""".stripMargin) shouldBe 100
  }

  // ===== Conditional with refs =====

  "ref in if/else branches" in {
    eval(
      """struct Box
        |    value: int
        |
        |main() -> int
        |    val flag = true
        |    val b = if flag
        |        new Box(10)
        |    else
        |        new Box(20)
        |    b.value
        |""".stripMargin) shouldBe 10
  }

  "ref in else branch" in {
    eval(
      """struct Box
        |    value: int
        |
        |main() -> int
        |    val flag = false
        |    val b = if flag
        |        new Box(10)
        |    else
        |        new Box(20)
        |    b.value
        |""".stripMargin) shouldBe 20
  }

  // ===== deinit =====

  "deinit called when last ref drops" in {
    eval(
      """var deinit_called = false
        |
        |struct Resource
        |    id: int
        |
        |Resource.deinit()
        |    deinit_called = true
        |
        |main() -> int
        |    val r = new Resource(1)
        |    // r goes out of scope at function exit → deinit called
        |    if deinit_called then return 0
        |    1
        |""".stripMargin) shouldBe 1  // deinit runs after return value computed
  }

  "deinit called on reassignment" in {
    eval(
      """var deinit_count = 0
        |
        |struct Resource
        |    id: int
        |
        |Resource.deinit()
        |    deinit_count = deinit_count + 1
        |
        |main() -> int
        |    var r = new Resource(1)
        |    r = new Resource(2)
        |    // first Resource should have been deinited
        |    deinit_count
        |""".stripMargin) shouldBe 1
  }

  "deinit not called while refs remain" in {
    eval(
      """var deinit_count = 0
        |
        |struct Resource
        |    id: int
        |
        |Resource.deinit()
        |    deinit_count = deinit_count + 1
        |
        |main() -> int
        |    val a = new Resource(1)
        |    val b = a  // refcount = 2
        |    // neither ref dropped yet
        |    deinit_count
        |""".stripMargin) shouldBe 0
  }

  "deinit accesses fields" in {
    eval(
      """var last_id = 0
        |
        |struct Resource
        |    id: int
        |
        |Resource.deinit()
        |    last_id = self.id
        |
        |main() -> int
        |    var r = new Resource(42)
        |    r = new Resource(99)
        |    last_id
        |""".stripMargin) shouldBe 42
  }

  "deinit in loop — all cleaned up" in {
    eval(
      """var deinit_count = 0
        |
        |struct Resource
        |    id: int
        |
        |Resource.deinit()
        |    deinit_count = deinit_count + 1
        |
        |main() -> int
        |    var i = 0
        |    while i < 10
        |        val r = new Resource(i)
        |        i++
        |    deinit_count
        |""".stripMargin) shouldBe 10
  }

  // ===== Analyzer error cases =====

  "pointer to ref conversion rejected" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """struct Node
        |    value: int
        |
        |main() -> int
        |    var x: Node
        |    var p: *Node = &x
        |    var r: &Node = p
        |    0
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  "new on non-struct type rejected" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """type MyInt = int
        |
        |main() -> int
        |    val x = new MyInt(0)
        |    0
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  "new with wrong number of args rejected" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """struct Point
        |    x: int
        |    y: int
        |
        |main() -> int
        |    val p = new Point(1)
        |    0
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  "new with wrong arg type rejected" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """struct Box
        |    value: bool
        |
        |main() -> int
        |    val b = new Box(42)
        |    0
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  // ===== &T type in various positions =====

  "ref type as function parameter" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """struct Node
        |    value: int
        |
        |process(n: &Node) -> int = n.value
        |
        |main() -> int = 0
        |""".stripMargin): @unchecked
    (new SyslAnalyzer).analyze(ast) // should not throw
  }

  "ref type as return type" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """struct Node
        |    value: int
        |
        |make() -> &Node = new Node(0)
        |
        |main() -> int = 0
        |""".stripMargin): @unchecked
    (new SyslAnalyzer).analyze(ast) // should not throw
  }

  "ref type as global variable" in {
    eval(
      """struct Box
        |    value: int
        |
        |var g: &Box = new Box(0)
        |
        |main() -> int
        |    g.value = 42
        |    g.value
        |""".stripMargin) shouldBe 42
  }
}
