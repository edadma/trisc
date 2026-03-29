package io.github.edadma.trisc

class SyslCodegenStructTests extends SyslCodegenHelpers {

  "struct field write and read through pointer" in {
    compileAndRun(
      """struct Point
        |    x: int
        |    y: int
        |
        |var p: Point
        |
        |main() -> int
        |    var pp: *Point = &p
        |    pp.x = 10
        |    pp.y = 32
        |    pp.x + pp.y
        |""".stripMargin) shouldBe 42
  }

  "struct field access second field" in {
    compileAndRun(
      """struct Pair
        |    first: int
        |    second: int
        |
        |var p: Pair
        |
        |main() -> int
        |    var pp: *Pair = &p
        |    pp.first = 100
        |    pp.second = 200
        |    pp.second
        |""".stripMargin) shouldBe 200
  }

  "struct array with field access" in {
    compileAndRun(
      """struct Entry
        |    key: int
        |    value: int
        |
        |var entries: [3]Entry
        |
        |main() -> int
        |    var e: *Entry = &entries[0]
        |    e.key = 1
        |    e.value = 10
        |    e = &entries[1]
        |    e.key = 2
        |    e.value = 20
        |    e = &entries[2]
        |    e.key = 3
        |    e.value = 30
        |    var sum = 0
        |    e = &entries[0]
        |    sum = sum + e.value
        |    e = &entries[1]
        |    sum = sum + e.value
        |    e = &entries[2]
        |    sum = sum + e.value
        |    sum
        |""".stripMargin) shouldBe 60
  }

  "struct field compound assign +=" in {
    compileAndRun(
      """struct Counter
        |    value: int
        |
        |var c: Counter
        |
        |main() -> int
        |    var p: *Counter = &c
        |    p.value = 10
        |    p.value += 32
        |    p.value
        |""".stripMargin) shouldBe 42
  }

  "struct field compound assign -=" in {
    compileAndRun(
      """struct Counter
        |    value: int
        |
        |var c: Counter
        |
        |main() -> int
        |    var p: *Counter = &c
        |    p.value = 50
        |    p.value -= 8
        |    p.value
        |""".stripMargin) shouldBe 42
  }

  "struct field compound assign *= on first field" in {
    compileAndRun(
      """struct Pair
        |    a: int
        |    b: int
        |
        |var p: Pair
        |
        |main() -> int
        |    var pp: *Pair = &p
        |    pp.a = 7
        |    pp.a *= 6
        |    pp.a
        |""".stripMargin) shouldBe 42
  }

  "struct field compound assign *= on second field" in {
    compileAndRun(
      """struct Pair
        |    a: int
        |    b: int
        |
        |var p: Pair
        |
        |main() -> int
        |    var pp: *Pair = &p
        |    pp.a = 5
        |    pp.b = 7
        |    pp.b *= 6
        |    pp.b
        |""".stripMargin) shouldBe 42
  }

  "multiple field writes then read all" in {
    compileAndRun(
      """struct TCB
        |    ssp: int
        |    state: int
        |    priority: int
        |
        |var tcb: TCB
        |
        |main() -> int
        |    var p: *TCB = &tcb
        |    p.ssp = 100
        |    p.state = 200
        |    p.priority = 300
        |    p.ssp + p.state + p.priority
        |""".stripMargin) shouldBe 600
  }

  "write field then overwrite same field" in {
    compileAndRun(
      """struct Pair
        |    a: int
        |    b: int
        |
        |var p: Pair
        |
        |main() -> int
        |    var pp: *Pair = &p
        |    pp.a = 99
        |    pp.a = 42
        |    pp.a
        |""".stripMargin) shouldBe 42
  }

  "function returns struct field" in {
    compileAndRun(
      """struct Entry
        |    value: int
        |
        |var e: Entry
        |
        |get_value() -> int
        |    var p: *Entry = &e
        |    p.value
        |
        |main() -> int
        |    var p: *Entry = &e
        |    p.value = 42
        |    get_value()
        |""".stripMargin) shouldBe 42
  }

  "index struct array then read field" in {
    compileAndRun(
      """struct Item
        |    x: int
        |    y: int
        |
        |var items: [4]Item
        |
        |main() -> int
        |    var p: *Item = &items[0]
        |    p.x = 10
        |    p.y = 20
        |    p = &items[1]
        |    p.x = 30
        |    p.y = 40
        |    p = &items[2]
        |    p.x = 50
        |    p.y = 60
        |    var q: *Item = &items[1]
        |    q.x + q.y
        |""".stripMargin) shouldBe 70
  }

  "index struct array with variable index" in {
    compileAndRun(
      """struct Slot
        |    data: int
        |
        |var slots: [4]Slot
        |
        |main() -> int
        |    var i = 0
        |    while i < 4
        |        var p: *Slot = &slots[i]
        |        p.data = (i + 1) * 10
        |        i += 1
        |    var p: *Slot = &slots[2]
        |    p.data
        |""".stripMargin) shouldBe 30
  }

  "index struct array with global index" in {
    compileAndRun(
      """struct Slot
        |    data: int
        |
        |var slots: [4]Slot
        |var idx = 0
        |
        |main() -> int
        |    var p: *Slot = &slots[0]
        |    p.data = 10
        |    p = &slots[1]
        |    p.data = 20
        |    p = &slots[2]
        |    p.data = 30
        |    idx = 2
        |    p = &slots[idx]
        |    p.data
        |""".stripMargin) shouldBe 30
  }

  "scheduler pattern: index, write fields, pick next, return field" in {
    compileAndRun(
      """struct Task
        |    ssp: int
        |    state: int
        |
        |var tasks: [3]Task
        |var current = 0
        |var count = 3
        |var ticks = 0
        |
        |schedule(cur_ssp: int) -> int
        |    ticks += 1
        |    var cur: *Task = &tasks[current]
        |    cur.ssp = cur_ssp
        |    cur.state = 0
        |    var next = current + 1
        |    if next >= count
        |        next = 0
        |    current = next
        |    var nxt: *Task = &tasks[next]
        |    nxt.state = 1
        |    nxt.ssp
        |
        |main() -> int
        |    // Initialize: thread 0 ssp=100, thread 1 ssp=200, thread 2 ssp=300
        |    var p: *Task = &tasks[0]
        |    p.ssp = 100
        |    p = &tasks[1]
        |    p.ssp = 200
        |    p = &tasks[2]
        |    p.ssp = 300
        |
        |    // Simulate: current=0, call schedule(111)
        |    // Should save 111 to tasks[0].ssp, pick next=1, return tasks[1].ssp=200
        |    var r1 = schedule(111)
        |
        |    // Now current=1, call schedule(222)
        |    // Should save 222 to tasks[1].ssp, pick next=2, return tasks[2].ssp=300
        |    var r2 = schedule(222)
        |
        |    // Now current=2, call schedule(333)
        |    // Should save 333 to tasks[2].ssp, pick next=0 (wrap), return tasks[0].ssp=111
        |    var r3 = schedule(333)
        |
        |    // Verify: r1=200, r2=300, r3=111, ticks=3
        |    if r1 != 200 then return 1
        |    if r2 != 300 then return 2
        |    if r3 != 111 then return 3
        |    if ticks != 3 then return 4
        |    0
        |""".stripMargin) shouldBe 0
  }

  "index into struct array via pointer: read field" in {
    compileAndRun(
      """struct Point
        |    x: int
        |    y: int
        |
        |var pts: [4]Point
        |
        |main() -> int
        |    var p: *Point = &pts[0]
        |    p[0].x = 10
        |    p[0].y = 20
        |    p[1].x = 30
        |    p[1].y = 40
        |    p[0].x + p[1].y
        |""".stripMargin) shouldBe 50
  }

  "index into struct array via struct pointer field: read and write" in {
    compileAndRun(
      """struct Node
        |    key: int
        |    value: int
        |
        |struct Container
        |    items: *Node
        |    count: int
        |
        |var nodes: [4]Node
        |var c: Container
        |
        |main() -> int
        |    c.items = &nodes[0]
        |    c.count = 0
        |    c.items[0].key = 100
        |    c.items[0].value = 1
        |    c.count += 1
        |    c.items[1].key = 200
        |    c.items[1].value = 2
        |    c.count += 1
        |    c.items[0].key + c.items[1].value + c.count
        |""".stripMargin) shouldBe 104
  }

  "chained struct pointer field index: compound assign" in {
    compileAndRun(
      """struct Node
        |    value: int
        |
        |struct Tree
        |    pool: *Node
        |    count: int
        |
        |var nodes: [4]Node
        |var tree: Tree
        |
        |main() -> int
        |    tree.pool = &nodes[0]
        |    tree.count = 0
        |    tree.pool[0].value = 10
        |    tree.pool[1].value = 20
        |    tree.pool[2].value = 30
        |    tree.count += 1
        |    tree.count += 1
        |    tree.count += 1
        |    tree.pool[0].value + tree.pool[1].value + tree.pool[2].value + tree.count
        |""".stripMargin) shouldBe 63
  }
}
