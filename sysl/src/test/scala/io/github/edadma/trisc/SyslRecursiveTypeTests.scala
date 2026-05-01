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

  // ===== Ref-counted recursive enum (new on enum variant) =====

  "new on enum variant creates &Enum" in {
    eval(
      """enum Expr
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
        |""".stripMargin) shouldBe 7
  }

  "nested new enum — 3-level tree" in {
    eval(
      """enum Expr
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
        |""".stripMargin) shouldBe 13
  }

  // ===== Recursive enum with slice-of-self field (regression for sysl@9ee727db) =====

  "enum with slice-of-self field type-checks" in {
    eval(
      """enum Tree
        |    Leaf(value: int)
        |    Node(children: []Tree)
        |
        |main() -> int
        |    val t = Leaf(42)
        |    t match
        |        Leaf(v) -> v
        |        Node(_) -> 0
        |""".stripMargin) shouldBe 42
  }

  "enum slice-of-self variant constructs and pattern-matches" in {
    eval(
      """enum Tree
        |    Leaf(value: int)
        |    Node(children: []Tree)
        |
        |main() -> int
        |    var cs = (new [3]Tree)[:0]
        |    cs = append(cs, Leaf(10))
        |    cs = append(cs, Leaf(20))
        |    val t = Node(cs)
        |    t match
        |        Leaf(_) -> 0
        |        Node(c) -> i32(len(c))
        |""".stripMargin) shouldBe 2
  }

  "enum slice-of-self supports recursive traversal" in {
    eval(
      """enum Tree
        |    Leaf(value: int)
        |    Node(children: []Tree)
        |
        |sum_leaves(t: *Tree) -> int
        |    *t match
        |        Leaf(v) -> v
        |        Node(c) ->
        |            var total = 0
        |            for i in 0..<len(c)
        |                total = total + sum_leaves(&c[i])
        |            total
        |
        |main() -> int
        |    var cs = (new [3]Tree)[:0]
        |    cs = append(cs, Leaf(1))
        |    cs = append(cs, Leaf(2))
        |    cs = append(cs, Leaf(7))
        |    var t = Node(cs)
        |    sum_leaves(&t)
        |""".stripMargin) shouldBe 10
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
