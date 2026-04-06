package io.github.edadma.trisc

class SyslCodegenRefEnumTests extends SyslCodegenHelpers {

  private val allocSource = scala.io.Source.fromFile("posix/stdlib/alloc.sysl").mkString
  private val stringSource = scala.io.Source.fromFile("posix/string/string.sysl").mkString

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

  private def refSources(mainSource: String, heapSize: Int = 16384): Map[String, String] =
    Map(
      "posix/unistd/sbrk" -> sbrkModule(heapSize),
      "posix/string/string" -> stringSource,
      "posix/stdlib/alloc" -> allocSource,
      "main" -> mainSource,
    )

  // ===== Basic new on enum variant =====

  "new on single-field variant" in {
    compileMultiAndRun(refSources(
      """import posix.stdlib.malloc
        |import posix.stdlib.free
        |
        |enum Expr
        |    Lit(value: int)
        |    Add(left: &Expr, right: &Expr)
        |
        |main() -> int
        |    val x = new Lit(42)
        |    *x match
        |        Lit(v) -> v
        |        Add(_, _) -> 0
        |""".stripMargin)) shouldBe 42
  }

  "new on two-field variant" in {
    compileMultiAndRun(refSources(
      """import posix.stdlib.malloc
        |import posix.stdlib.free
        |
        |enum Expr
        |    Lit(value: int)
        |    Add(left: &Expr, right: &Expr)
        |
        |main() -> int
        |    val a = new Lit(3)
        |    val b = new Lit(4)
        |    val c = new Add(a, b)
        |    *c match
        |        Lit(v) -> v
        |        Add(_, _) -> 99
        |""".stripMargin)) shouldBe 99
  }

  // ===== Deref and pattern match on &Enum =====

  "eval tree via deref match" in {
    compileMultiAndRun(refSources(
      """import posix.stdlib.malloc
        |import posix.stdlib.free
        |
        |enum Expr
        |    Lit(value: int)
        |    Add(left: &Expr, right: &Expr)
        |
        |eval_expr(e: &Expr) -> int
        |    *e match
        |        Lit(v) -> v
        |        Add(l, r) -> eval_expr(l) + eval_expr(r)
        |
        |main() -> int
        |    val tree = new Add(new Lit(3), new Lit(4))
        |    eval_expr(tree)
        |""".stripMargin)) shouldBe 7
  }

  "nested 3-level tree" in {
    compileMultiAndRun(refSources(
      """import posix.stdlib.malloc
        |import posix.stdlib.free
        |
        |enum Expr
        |    Lit(value: int)
        |    Add(left: &Expr, right: &Expr)
        |
        |eval_expr(e: &Expr) -> int
        |    *e match
        |        Lit(v) -> v
        |        Add(l, r) -> eval_expr(l) + eval_expr(r)
        |
        |main() -> int
        |    val tree = new Add(
        |        new Add(new Lit(1), new Lit(2)),
        |        new Lit(10))
        |    eval_expr(tree)
        |""".stripMargin)) shouldBe 13
  }

  "deeply nested tree (4 levels)" in {
    compileMultiAndRun(refSources(
      """import posix.stdlib.malloc
        |import posix.stdlib.free
        |
        |enum Expr
        |    Lit(value: int)
        |    Add(left: &Expr, right: &Expr)
        |
        |eval_expr(e: &Expr) -> int
        |    *e match
        |        Lit(v) -> v
        |        Add(l, r) -> eval_expr(l) + eval_expr(r)
        |
        |main() -> int
        |    val tree = new Add(
        |        new Add(new Lit(1), new Lit(2)),
        |        new Add(new Lit(3), new Lit(4)))
        |    eval_expr(tree)
        |""".stripMargin)) shouldBe 10
  }

  // ===== Multiple variant types =====

  "three-variant enum with refs" in {
    compileMultiAndRun(refSources(
      """import posix.stdlib.malloc
        |import posix.stdlib.free
        |
        |enum Expr
        |    Lit(value: int)
        |    Neg(inner: &Expr)
        |    Add(left: &Expr, right: &Expr)
        |
        |eval_expr(e: &Expr) -> int
        |    *e match
        |        Lit(v) -> v
        |        Neg(inner) -> 0 - eval_expr(inner)
        |        Add(l, r) -> eval_expr(l) + eval_expr(r)
        |
        |main() -> int
        |    val tree = new Add(new Lit(10), new Neg(new Lit(3)))
        |    eval_expr(tree)
        |""".stripMargin)) shouldBe 7
  }

  // ===== Ref enum as function argument =====

  "pass &Enum to function" in {
    compileMultiAndRun(refSources(
      """import posix.stdlib.malloc
        |import posix.stdlib.free
        |
        |enum Expr
        |    Lit(value: int)
        |    Add(left: &Expr, right: &Expr)
        |
        |eval_expr(e: &Expr) -> int
        |    *e match
        |        Lit(v) -> v
        |        Add(l, r) -> eval_expr(l) + eval_expr(r)
        |
        |main() -> int
        |    eval_expr(new Lit(100))
        |""".stripMargin)) shouldBe 100
  }

  // ===== Recursive struct codegen (verify existing works) =====

  "recursive struct via pointer — linked list" in {
    compileAndRun(
      """struct Node
        |    value: int
        |    next: *Node
        |
        |sum(n: *Node) -> int
        |    if i64(n) == 0 then return 0
        |    n.value + sum(n.next)
        |
        |main() -> int
        |    var a: Node
        |    var b: Node
        |    var c: Node
        |    a.value = 10
        |    b.value = 20
        |    c.value = 30
        |    a.next = &b
        |    b.next = &c
        |    sum(&a)
        |""".stripMargin) shouldBe 60
  }

  "recursive enum via pointer — tree eval" in {
    compileAndRun(
      """enum Expr
        |    Lit(value: int)
        |    Add(left: *Expr, right: *Expr)
        |
        |eval_expr(e: *Expr) -> int
        |    *e match
        |        Lit(v) -> v
        |        Add(l, r) -> eval_expr(l) + eval_expr(r)
        |
        |main() -> int
        |    var a = Lit(3)
        |    var b = Lit(4)
        |    var c = Add(&a, &b)
        |    eval_expr(&c)
        |""".stripMargin) shouldBe 7
  }

  // ===== Mutually recursive structs =====

  "mutually recursive structs" in {
    compileAndRun(
      """struct A
        |    value: int
        |    b: *B
        |
        |struct B
        |    value: int
        |    a: *A
        |
        |main() -> int
        |    var x: A
        |    var y: B
        |    x.value = 10
        |    y.value = 20
        |    x.b = &y
        |    y.a = &x
        |    x.value + x.b.value
        |""".stripMargin) shouldBe 30
  }

  // ===== Assign ref enum to variable =====

  "assign new enum to variable then match" in {
    compileMultiAndRun(refSources(
      """import posix.stdlib.malloc
        |import posix.stdlib.free
        |
        |enum Expr
        |    Lit(value: int)
        |    Add(left: &Expr, right: &Expr)
        |
        |main() -> int
        |    val a = new Lit(10)
        |    val b = new Lit(20)
        |    val c = new Add(a, b)
        |    *c match
        |        Lit(v) -> v
        |        Add(l, r) ->
        |            x = *l match
        |                Lit(v) -> v
        |                Add(_, _) -> 0
        |            y = *r match
        |                Lit(v) -> v
        |                Add(_, _) -> 0
        |            x + y
        |""".stripMargin)) shouldBe 30
  }
}
