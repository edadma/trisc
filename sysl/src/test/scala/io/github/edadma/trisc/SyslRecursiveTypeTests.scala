package io.github.edadma.trisc

class SyslRecursiveTypeTests extends SyslTestHelpers {

  // ===== Recursive structs via pointer =====

  "struct with pointer to self" in {
    eval(
      """struct Node
        |    value: int
        |    next: *Node
        |
        |main() -> int
        |    var a: Node
        |    var b: Node
        |    a.value = 1
        |    b.value = 2
        |    a.next = &b
        |    a.value + a.next.value
        |""".stripMargin) shouldBe 3
  }

  "linked list traversal" in {
    eval(
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
        |    // c.next is zero-initialized (null)
        |    sum(&a)
        |""".stripMargin) shouldBe 60
  }

  // ===== Recursive data enum via pointer =====

  "data enum with pointer to self" in {
    eval(
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

  // ===== Mutual recursion =====

  "mutually recursive structs" in {
    eval(
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
}
