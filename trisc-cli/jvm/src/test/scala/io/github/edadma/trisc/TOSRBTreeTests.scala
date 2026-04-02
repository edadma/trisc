package io.github.edadma.trisc

class TOSRBTreeTests extends TOSTestHelpers {

  "RBTree: basic struct field access" in {
    val (_, output) = runRBTest(
      """import rbtree.*
        |
        |var pool: [4]RBNode
        |var tree: Tree
        |
        |main() -> int
        |    tree_init(&tree, &pool[0], 4)
        |    // Check root value directly
        |    putchar(48 + tree.count)
        |    // test setting and reading root
        |    tree.root = 42
        |    if tree.root == 42
        |        putchar(65)
        |    else
        |        putchar(66)
        |    // test NIL
        |    tree.root = -1
        |    if tree.root == -1
        |        putchar(67)
        |    else
        |        putchar(68)
        |    0
        |""".stripMargin)
    output shouldBe "0AC"
  }

  "RBTree: tree_init sets root to NIL" in {
    val (_, output) = runRBTest(
      """import rbtree.*
        |
        |var pool: [4]RBNode
        |var tree: Tree
        |
        |main() -> int
        |    tree_init(&tree, &pool[0], 4)
        |    putchar(48 + tree.count)
        |    if tree.root == -1
        |        putchar(65)
        |    else
        |        putchar(66)
        |    // also check free_head
        |    if tree.free_head == 0
        |        putchar(67)
        |    else
        |        putchar(68)
        |    0
        |""".stripMargin)
    output shouldBe "0AC"
  }

  "RBTree: struct field compound assign" in {
    val (_, output) = runWithBoot(
      """struct Counter
        |    n: int
        |
        |var c: Counter
        |
        |main() -> int
        |    c.n = 0
        |    var p: *Counter = &c
        |    p.n += 1
        |    p.n += 1
        |    p.n += 1
        |    putchar(48 + p.n)
        |    0
        |""".stripMargin)
    output shouldBe "3"
  }

  "RBTree: count via rb_insert" in {
    val (cpu, output) = runRBTest(
      """import rbtree.*
        |
        |var pool: [16]RBNode
        |var tree: Tree
        |
        |inc(t: *Tree)
        |    t.count += 1
        |
        |main() -> int
        |    tree_init(&tree, &pool[0], 16)
        |    putchar(48 + tree.count)
        |    inc(&tree)
        |    putchar(48 + tree.count)
        |    inc(&tree)
        |    putchar(48 + tree.count)
        |    // Now try actual insert
        |    rb_insert(&tree, 10, 1)
        |    putchar(48 + rb_count(&tree))
        |    rb_insert(&tree, 20, 2)
        |    putchar(48 + rb_count(&tree))
        |    0
        |""".stripMargin)
    info(s"output: '$output' state: ${cpu.state}")
    output shouldBe "01234"
  }

  "RBTree: insert and find_min" in {
    val (_, output) = runRBTest(
      """import rbtree.*
        |
        |var pool: [16]RBNode
        |var tree: Tree
        |
        |main() -> int
        |    tree_init(&tree, &pool[0], 16)
        |    rb_insert(&tree, 30, 3)
        |    rb_insert(&tree, 10, 1)
        |    rb_insert(&tree, 20, 2)
        |    val min = rb_find_min(&tree)
        |    putchar(48 + rb_node_value(&tree, min))
        |    0
        |""".stripMargin)
    output shouldBe "1"
  }

  "RBTree: count tracks insertions" in {
    val (_, output) = runRBTest(
      """import rbtree.*
        |
        |var pool: [16]RBNode
        |var tree: Tree
        |
        |main() -> int
        |    tree_init(&tree, &pool[0], 16)
        |    rb_insert(&tree, 5, 0)
        |    rb_insert(&tree, 3, 0)
        |    rb_insert(&tree, 7, 0)
        |    rb_insert(&tree, 1, 0)
        |    rb_insert(&tree, 4, 0)
        |    putchar(48 + rb_count(&tree))
        |    0
        |""".stripMargin)
    output shouldBe "5"
  }

  "RBTree: empty tree returns NIL for find_min" in {
    val (_, output) = runRBTest(
      """import rbtree.*
        |
        |var pool: [16]RBNode
        |var tree: Tree
        |
        |main() -> int
        |    tree_init(&tree, &pool[0], 16)
        |    val min = rb_find_min(&tree)
        |    if min == -1
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin)
    output shouldBe "Y"
  }

  "RBTree: remove min and find new min" in {
    val (_, output) = runRBTest(
      """import rbtree.*
        |
        |var pool: [16]RBNode
        |var tree: Tree
        |
        |main() -> int
        |    tree_init(&tree, &pool[0], 16)
        |    rb_insert(&tree, 30, 3)
        |    rb_insert(&tree, 10, 1)
        |    rb_insert(&tree, 20, 2)
        |    // Remove the min (key=10, value=1)
        |    val min1 = rb_find_min(&tree)
        |    rb_remove(&tree, min1)
        |    // New min should be key=20, value=2
        |    val min2 = rb_find_min(&tree)
        |    putchar(48 + rb_node_value(&tree, min2))
        |    putchar(48 + rb_count(&tree))
        |    0
        |""".stripMargin)
    // value=2, count=2
    output shouldBe "22"
  }

  "RBTree: insert ascending sequence maintains balance" in {
    val (_, output) = runRBTest(
      """import rbtree.*
        |
        |var pool: [16]RBNode
        |var tree: Tree
        |
        |main() -> int
        |    tree_init(&tree, &pool[0], 16)
        |    // Ascending insertion — worst case for naive BST
        |    var i: i64 = 1
        |    while i <= 7
        |        rb_insert(&tree, i, int(i))
        |        i += 1
        |    // Min should be 1
        |    val min = rb_find_min(&tree)
        |    putchar(48 + rb_node_value(&tree, min))
        |    // Count should be 7
        |    putchar(48 + rb_count(&tree))
        |    0
        |""".stripMargin)
    output shouldBe "17"
  }

  "RBTree: remove all nodes leaves empty tree" in {
    val (_, output) = runRBTest(
      """import rbtree.*
        |
        |var pool: [16]RBNode
        |var tree: Tree
        |
        |main() -> int
        |    tree_init(&tree, &pool[0], 16)
        |    rb_insert(&tree, 50, 5)
        |    rb_insert(&tree, 30, 3)
        |    rb_insert(&tree, 70, 7)
        |    rb_insert(&tree, 20, 2)
        |    // Remove all by repeatedly removing min
        |    var i = 0
        |    while i < 4
        |        val min = rb_find_min(&tree)
        |        rb_remove(&tree, min)
        |        i += 1
        |    putchar(48 + rb_empty(&tree))
        |    putchar(48 + rb_count(&tree))
        |    0
        |""".stripMargin)
    output shouldBe "10"
  }

  "RBTree: insert descending sequence" in {
    val (_, output) = runRBTest(
      """import rbtree.*
        |
        |var pool: [16]RBNode
        |var tree: Tree
        |
        |main() -> int
        |    tree_init(&tree, &pool[0], 16)
        |    var i: i64 = 8
        |    while i >= 1
        |        rb_insert(&tree, i, int(i))
        |        i -= 1
        |    val min = rb_find_min(&tree)
        |    putchar(48 + rb_node_value(&tree, min))
        |    putchar(48 + rb_count(&tree))
        |    0
        |""".stripMargin)
    output shouldBe "18"
  }

  "RBTree: remove middle nodes preserves structure" in {
    val (_, output) = runRBTest(
      """import rbtree.*
        |
        |var pool: [16]RBNode
        |var tree: Tree
        |var nodes: [5]int
        |
        |main() -> int
        |    tree_init(&tree, &pool[0], 16)
        |    nodes[0] = rb_insert(&tree, 10, 1)
        |    nodes[1] = rb_insert(&tree, 20, 2)
        |    nodes[2] = rb_insert(&tree, 30, 3)
        |    nodes[3] = rb_insert(&tree, 40, 4)
        |    nodes[4] = rb_insert(&tree, 50, 5)
        |    // Remove key=30 (middle)
        |    rb_remove(&tree, nodes[2])
        |    // Remove key=10 (min)
        |    rb_remove(&tree, nodes[0])
        |    // New min should be key=20, value=2
        |    val min = rb_find_min(&tree)
        |    putchar(48 + rb_node_value(&tree, min))
        |    putchar(48 + rb_count(&tree))
        |    0
        |""".stripMargin)
    output shouldBe "23"
  }

  "RBTree: node reuse after remove" in {
    val (_, output) = runRBTest(
      """import rbtree.*
        |
        |var pool: [4]RBNode
        |var tree: Tree
        |
        |main() -> int
        |    tree_init(&tree, &pool[0], 4)
        |    // Fill pool
        |    val a = rb_insert(&tree, 10, 1)
        |    val b = rb_insert(&tree, 20, 2)
        |    val c = rb_insert(&tree, 30, 3)
        |    val d = rb_insert(&tree, 40, 4)
        |    // Pool is full — next insert should fail
        |    val e = rb_insert(&tree, 50, 5)
        |    if e == -1
        |        putchar(70)
        |    // Remove one, then insert should succeed
        |    rb_remove(&tree, a)
        |    val f = rb_insert(&tree, 5, 0)
        |    if f != -1
        |        putchar(82)
        |    val min = rb_find_min(&tree)
        |    putchar(48 + rb_node_value(&tree, min))
        |    0
        |""".stripMargin)
    // F=full, R=reuse succeeded, 0=min value
    output shouldBe "FR0"
  }
}
