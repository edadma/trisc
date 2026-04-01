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

  // ===== Embedded (nested) struct tests =====

  "nested struct read and write" in {
    compileAndRun(
      """struct Inner
        |    value: int
        |
        |struct Outer
        |    inner: Inner
        |    extra: int
        |
        |var o: Outer
        |
        |main() -> int
        |    var p: *Outer = &o
        |    p.inner.value = 42
        |    p.extra = 10
        |    p.inner.value + p.extra
        |""".stripMargin) shouldBe 52
  }

  "nested struct field offset correct" in {
    compileAndRun(
      """struct Point
        |    x: int
        |    y: int
        |
        |struct Line
        |    start: Point
        |    end_: Point
        |
        |var line: Line
        |
        |main() -> int
        |    var p: *Line = &line
        |    p.start.x = 1
        |    p.start.y = 2
        |    p.end_.x = 3
        |    p.end_.y = 4
        |    p.start.x + p.start.y + p.end_.x + p.end_.y
        |""".stripMargin) shouldBe 10
  }

  "triple nested struct" in {
    compileAndRun(
      """struct A
        |    num: int
        |
        |struct B
        |    a: A
        |
        |struct C
        |    b: B
        |    extra: int
        |
        |var c: C
        |
        |main() -> int
        |    var p: *C = &c
        |    p.b.a.num = 99
        |    p.extra = 1
        |    p.b.a.num + p.extra
        |""".stripMargin) shouldBe 100
  }

  "nested struct with array field" in {
    compileAndRun(
      """struct Header
        |    tag: int
        |    size: int
        |
        |struct Packet
        |    hdr: Header
        |    data: [4]int
        |
        |var pkt: Packet
        |
        |main() -> int
        |    var p: *Packet = &pkt
        |    p.hdr.tag = 1
        |    p.hdr.size = 4
        |    p.data[0] = 10
        |    p.data[1] = 20
        |    p.data[2] = 30
        |    p.data[3] = 40
        |    p.hdr.tag + p.hdr.size + p.data[0] + p.data[3]
        |""".stripMargin) shouldBe 55
  }

  "array of nested structs" in {
    compileAndRun(
      """struct Point
        |    x: int
        |    y: int
        |
        |struct Rect
        |    origin: Point
        |    size: Point
        |
        |var rects: [2]Rect
        |
        |main() -> int
        |    rects[0].origin.x = 1
        |    rects[0].origin.y = 2
        |    rects[0].size.x = 10
        |    rects[0].size.y = 20
        |    rects[1].origin.x = 3
        |    rects[1].origin.y = 4
        |    rects[1].size.x = 30
        |    rects[1].size.y = 40
        |    rects[0].origin.x + rects[1].size.y
        |""".stripMargin) shouldBe 41
  }

  "nested struct passed to function via pointer" in {
    compileAndRun(
      """struct Inner
        |    value: int
        |
        |struct Outer
        |    inner: Inner
        |    count: int
        |
        |get_inner_value(o: *Outer) -> int = o.inner.value
        |
        |var obj: Outer
        |
        |main() -> int
        |    var p: *Outer = &obj
        |    p.inner.value = 77
        |    p.count = 3
        |    get_inner_value(p)
        |""".stripMargin) shouldBe 77
  }

  "nested struct local variable" in {
    compileAndRun(
      """struct Inner
        |    x: int
        |    y: int
        |
        |struct Outer
        |    inner: Inner
        |    z: int
        |
        |main() -> int
        |    o: Outer
        |    o.inner.x = 10
        |    o.inner.y = 20
        |    o.z = 12
        |    o.inner.x + o.inner.y + o.z
        |""".stripMargin) shouldBe 42
  }

  // ===== Alignment / padding tests =====

  "mixed width fields: i8 then i32" in {
    compileAndRun(
      """struct S
        |    a: i8
        |    b: i32
        |
        |var s: S
        |
        |main() -> int
        |    var p: *S = &s
        |    p.a = 1
        |    p.b = 100
        |    p.a + p.b
        |""".stripMargin) shouldBe 101
  }

  "mixed width fields: i8 i16 i32 i64" in {
    compileAndRun(
      """struct Mixed
        |    a: i8
        |    b: i16
        |    c: i32
        |    d: i64
        |
        |var m: Mixed
        |
        |main() -> int
        |    var p: *Mixed = &m
        |    p.a = 1
        |    p.b = 2
        |    p.c = 3
        |    p.d = 4
        |    int(p.a + p.b + p.c + p.d)
        |""".stripMargin) shouldBe 10
  }

  "mixed width: i32 then i8 then i32" in {
    compileAndRun(
      """struct S
        |    x: i32
        |    flag: i8
        |    y: i32
        |
        |var s: S
        |
        |main() -> int
        |    var p: *S = &s
        |    p.x = 10
        |    p.flag = 1
        |    p.y = 31
        |    p.x + p.flag + p.y
        |""".stripMargin) shouldBe 42
  }

  "i64 then i8 then i64 alignment" in {
    compileAndRun(
      """struct S
        |    big1: i64
        |    small: i8
        |    big2: i64
        |
        |var s: S
        |
        |main() -> int
        |    var p: *S = &s
        |    p.big1 = 100
        |    p.small = 5
        |    p.big2 = 200
        |    int(p.big1 + p.small + p.big2)
        |""".stripMargin) shouldBe 305
  }

  "bool field alignment" in {
    compileAndRun(
      """struct S
        |    flag: bool
        |    value: int
        |
        |var s: S
        |
        |main() -> int
        |    var p: *S = &s
        |    p.flag = true
        |    p.value = 41
        |    int(p.flag) + p.value
        |""".stripMargin) shouldBe 42
  }

  "multiple i8 fields before i32" in {
    compileAndRun(
      """struct S
        |    a: i8
        |    b: i8
        |    c: i8
        |    d: i32
        |
        |var s: S
        |
        |main() -> int
        |    var p: *S = &s
        |    p.a = 1
        |    p.b = 2
        |    p.c = 3
        |    p.d = 100
        |    p.a + p.b + p.c + p.d
        |""".stripMargin) shouldBe 106
  }

  "u8 u16 u32 unsigned field alignment" in {
    compileAndRun(
      """struct S
        |    a: u8
        |    b: u16
        |    c: u32
        |
        |var s: S
        |
        |main() -> int
        |    var p: *S = &s
        |    p.a = 10
        |    p.b = 20
        |    p.c = 12
        |    int(p.a) + int(p.b) + int(p.c)
        |""".stripMargin) shouldBe 42
  }

  "sizeof matches expected layout" in {
    compileAndRun(
      """struct Mixed
        |    a: i8
        |    b: i16
        |    c: i32
        |    d: i64
        |
        |main() -> int = sizeof(Mixed)
        |""".stripMargin) shouldBe 16
  }

  "sizeof with trailing padding" in {
    compileAndRun(
      """struct S
        |    big: i64
        |    small: i8
        |
        |main() -> int = sizeof(S)
        |""".stripMargin) shouldBe 16  // 8 + 1 + 7 pad to align to 8
  }

  "array of padded structs: second element aligned" in {
    compileAndRun(
      """struct S
        |    big: i64
        |    small: i8
        |
        |var arr: [2]S
        |
        |main() -> int
        |    arr[0].big = 100
        |    arr[0].small = 1
        |    arr[1].big = 200
        |    arr[1].small = 2
        |    int(arr[0].big + arr[1].big) + arr[0].small + arr[1].small
        |""".stripMargin) shouldBe 303
  }

  "array of mixed-width structs" in {
    compileAndRun(
      """struct Entry
        |    flag: i8
        |    value: i32
        |    id: i64
        |
        |var entries: [3]Entry
        |
        |main() -> int
        |    entries[0].flag = 1
        |    entries[0].value = 10
        |    entries[0].id = 100
        |    entries[1].flag = 2
        |    entries[1].value = 20
        |    entries[1].id = 200
        |    entries[2].flag = 3
        |    entries[2].value = 30
        |    entries[2].id = 300
        |    entries[0].flag + entries[1].value + int(entries[2].id)
        |""".stripMargin) shouldBe 321
  }

  "struct with embedded struct and i8 fields" in {
    compileAndRun(
      """struct Flags
        |    a: i8
        |    b: i8
        |
        |struct Config
        |    flags: Flags
        |    count: int
        |
        |var cfg: Config
        |
        |main() -> int
        |    var p: *Config = &cfg
        |    p.flags.a = 1
        |    p.flags.b = 2
        |    p.count = 39
        |    p.flags.a + p.flags.b + p.count
        |""".stripMargin) shouldBe 42
  }
}
