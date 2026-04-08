package io.github.edadma.trisc

class SyslStructArrayTests extends OSKitTestHelpers {

  // === Basic: inline array field read/write ===

  "struct with i8 array field: write and read back" in {
    val (_, output) = runWithBoot(
      """struct Buf
        |    data: [4]byte
        |    len: int
        |
        |var b: Buf
        |
        |main() -> int
        |    b.data[0] = 72
        |    b.data[1] = 105
        |    b.len = 2
        |    var i = 0
        |    while i < b.len
        |        putchar(b.data[i])
        |        i += 1
        |    0
        |""".stripMargin
    )
    output shouldBe "Hi"
  }

  "struct with int array field: write and read back" in {
    val (_, output) = runWithBoot(
      """struct Nums
        |    vals: [4]int
        |    count: int
        |
        |var n: Nums
        |
        |main() -> int
        |    n.vals[0] = 10
        |    n.vals[1] = 20
        |    n.vals[2] = 30
        |    n.count = 3
        |    var sum = 0
        |    var i = 0
        |    while i < n.count
        |        sum += n.vals[i]
        |        i += 1
        |    if sum == 60
        |        putchar(89)
        |    0
        |""".stripMargin
    )
    output shouldBe "Y"
  }

  // === Array field at non-zero offset in struct ===

  "struct with array field after scalar fields" in {
    val (_, output) = runWithBoot(
      """struct Record
        |    id: int
        |    tag: int
        |    name: [4]byte
        |
        |var r: Record
        |
        |main() -> int
        |    r.id = 1
        |    r.tag = 2
        |    r.name[0] = 65
        |    r.name[1] = 66
        |    r.name[2] = 67
        |    r.name[3] = 0
        |    putchar(r.name[0])
        |    putchar(r.name[1])
        |    putchar(r.name[2])
        |    if r.id == 1
        |        putchar(49)
        |    if r.tag == 2
        |        putchar(50)
        |    0
        |""".stripMargin
    )
    output shouldBe "ABC12"
  }

  // === Array of structs with array fields ===

  "array of structs with array field" in {
    val (_, output) = runWithBoot(
      """struct Entry
        |    buf: [4]byte
        |    len: int
        |
        |var entries: [3]Entry
        |
        |main() -> int
        |    entries[0].buf[0] = 65
        |    entries[0].buf[1] = 66
        |    entries[0].len = 2
        |    entries[1].buf[0] = 67
        |    entries[1].buf[1] = 68
        |    entries[1].len = 2
        |    entries[2].buf[0] = 69
        |    entries[2].len = 1
        |    var i = 0
        |    while i < 3
        |        var j = 0
        |        while j < entries[i].len
        |            putchar(entries[i].buf[j])
        |            j += 1
        |        i += 1
        |    0
        |""".stripMargin
    )
    output shouldBe "ABCDE"
  }

  // === Multiple array fields in same struct ===

  "struct with multiple array fields" in {
    val (_, output) = runWithBoot(
      """struct TwoArrays
        |    first: [4]byte
        |    second: [4]byte
        |
        |var t: TwoArrays
        |
        |main() -> int
        |    t.first[0] = 72
        |    t.first[1] = 73
        |    t.second[0] = 74
        |    t.second[1] = 75
        |    putchar(t.first[0])
        |    putchar(t.first[1])
        |    putchar(t.second[0])
        |    putchar(t.second[1])
        |    0
        |""".stripMargin
    )
    output shouldBe "HIJK"
  }

  // === Array field with computed index ===

  "struct array field with computed index" in {
    val (_, output) = runWithBoot(
      """struct Buf
        |    data: [8]byte
        |    head: int
        |    tail: int
        |
        |var b: Buf
        |
        |main() -> int
        |    b.tail = 0
        |    b.head = 0
        |    // Push three characters
        |    b.data[b.tail] = 88
        |    b.tail += 1
        |    b.data[b.tail] = 89
        |    b.tail += 1
        |    b.data[b.tail] = 90
        |    b.tail += 1
        |    // Pop and print them
        |    while b.head < b.tail
        |        putchar(b.data[b.head])
        |        b.head += 1
        |    0
        |""".stripMargin
    )
    output shouldBe "XYZ"
  }

  // === Local struct with array field ===

  "local struct variable with array field" in {
    val (_, output) = runWithBoot(
      """struct Msg
        |    data: [4]byte
        |    len: int
        |
        |main() -> int
        |    var m: Msg
        |    m.data[0] = 79
        |    m.data[1] = 75
        |    m.len = 2
        |    var i = 0
        |    while i < m.len
        |        putchar(m.data[i])
        |        i += 1
        |    0
        |""".stripMargin
    )
    output shouldBe "OK"
  }

  // === Pointer to struct with array field ===

  "pointer to struct: access array field" in {
    val (_, output) = runWithBoot(
      """struct Buf
        |    data: [4]byte
        |    len: int
        |
        |var b: Buf
        |
        |fill(p: *Buf)
        |    p.data[0] = 71
        |    p.data[1] = 79
        |    p.len = 2
        |
        |main() -> int
        |    fill(&b)
        |    var i = 0
        |    while i < b.len
        |        putchar(b.data[i])
        |        i += 1
        |    0
        |""".stripMargin
    )
    output shouldBe "GO"
  }

  // === Struct field that is itself a struct containing an array ===

  "nested struct with array field" in {
    val (_, output) = runWithBoot(
      """struct Inner
        |    items: [4]byte
        |    count: int
        |
        |struct Outer
        |    tag: int
        |    inner: Inner
        |
        |var obj: Outer
        |
        |main() -> int
        |    obj.tag = 1
        |    obj.inner.items[0] = 80
        |    obj.inner.items[1] = 81
        |    obj.inner.count = 2
        |    if obj.tag == 1
        |        putchar(49)
        |    var i = 0
        |    while i < obj.inner.count
        |        putchar(obj.inner.items[i])
        |        i += 1
        |    0
        |""".stripMargin
    )
    output shouldBe "1PQ"
  }

  // === Array field is the only field ===

  "struct with only an array field" in {
    val (_, output) = runWithBoot(
      """struct Wrapper
        |    data: [8]byte
        |
        |var w: Wrapper
        |
        |main() -> int
        |    w.data[0] = 65
        |    w.data[1] = 66
        |    w.data[2] = 67
        |    putchar(w.data[0])
        |    putchar(w.data[1])
        |    putchar(w.data[2])
        |    0
        |""".stripMargin
    )
    output shouldBe "ABC"
  }

  // === Large array field (ring buffer pattern, like the TTY bug) ===

  "ring buffer in struct — the TTY pattern" in {
    val (_, output) = runWithBoot(
      """struct RingBuf
        |    buf: [64]byte
        |    head: int
        |    tail: int
        |    count: int
        |
        |var ring: RingBuf
        |
        |push(ch: byte) -> int
        |    if ring.count >= 64
        |        return 0
        |    ring.buf[ring.tail] = ch
        |    ring.tail = (ring.tail + 1) % 64
        |    ring.count += 1
        |    1
        |
        |pop() -> int
        |    if ring.count == 0
        |        return -1
        |    val ch = ring.buf[ring.head]
        |    ring.head = (ring.head + 1) % 64
        |    ring.count -= 1
        |    int(ch)
        |
        |main() -> int
        |    ring.head = 0
        |    ring.tail = 0
        |    ring.count = 0
        |    push(72)
        |    push(101)
        |    push(108)
        |    push(108)
        |    push(111)
        |    var ch = pop()
        |    while ch >= 0
        |        putchar(ch)
        |        ch = pop()
        |    0
        |""".stripMargin
    )
    output shouldBe "Hello"
  }

  // === Array of structs with array field, indexed by variable ===

  "array of structs indexed by variable, accessing array field" in {
    val (_, output) = runWithBoot(
      """struct Console
        |    in_buf: [8]byte
        |    in_count: int
        |
        |var consoles: [2]Console
        |
        |main() -> int
        |    var id = 0
        |    consoles[id].in_buf[0] = 65
        |    consoles[id].in_buf[1] = 66
        |    consoles[id].in_count = 2
        |    id = 1
        |    consoles[id].in_buf[0] = 67
        |    consoles[id].in_buf[1] = 68
        |    consoles[id].in_count = 2
        |    // Read back from both
        |    id = 0
        |    while id < 2
        |        var j = 0
        |        while j < consoles[id].in_count
        |            putchar(consoles[id].in_buf[j])
        |            j += 1
        |        id += 1
        |    0
        |""".stripMargin
    )
    output shouldBe "ABCD"
  }

  // === Address-of array element inside struct ===

  "struct array field: fill and read in loop" in {
    val (_, output) = runWithBoot(
      """struct Buf
        |    data: [8]byte
        |    len: int
        |
        |var b: Buf
        |
        |main() -> int
        |    var i = 0
        |    while i < 8
        |        b.data[i] = byte(65 + i)
        |        i += 1
        |    b.len = 8
        |    i = 0
        |    while i < b.len
        |        putchar(b.data[i])
        |        i += 1
        |    0
        |""".stripMargin
    )
    output shouldBe "ABCDEFGH"
  }

  // === Struct with i64 array field ===

  "struct with i64 array field" in {
    val (_, output) = runWithBoot(
      """struct BigBuf
        |    vals: [4]i64
        |    count: int
        |
        |var bb: BigBuf
        |
        |main() -> int
        |    bb.vals[0] = 1000000
        |    bb.vals[1] = 2000000
        |    bb.count = 2
        |    if bb.vals[0] + bb.vals[1] == 3000000
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin
    )
    output shouldBe "Y"
  }

  // === Verify no memory corruption (the actual TTY bug) ===

  "struct array field writes don't corrupt low memory" in {
    val (cpu, output) = runWithBoot(
      """struct Console
        |    in_buf: [64]byte
        |    in_head: int
        |    in_tail: int
        |    in_count: int
        |
        |var consoles: [4]Console
        |
        |main() -> int
        |    // Write 20 characters to console 0's buffer
        |    var i = 0
        |    while i < 20
        |        consoles[0].in_buf[i] = byte(65 + i)
        |        i += 1
        |    consoles[0].in_tail = 20
        |    consoles[0].in_count = 20
        |    // Read them back
        |    i = 0
        |    while i < 20
        |        putchar(consoles[0].in_buf[i])
        |        i += 1
        |    0
        |""".stripMargin
    )
    output shouldBe "ABCDEFGHIJKLMNOPQRST"
    cpu.state shouldBe State.Halt
  }

  // === Codegen bug: if/else + struct array field copy ===

  "struct array: copy field between elements with if/else" in {
    val (cpu, output) = runWithBoot(
      """struct Thread
        |    a: int
        |    b: int
        |    uid: int
        |    c: int
        |
        |var threads: [4]Thread
        |var current = 0
        |
        |main() -> int
        |    threads[0].uid = 42
        |    var idx = 1
        |    if current >= 0
        |        threads[idx].uid = threads[current].uid
        |    else
        |        threads[idx].uid = 0
        |    if threads[1].uid == 42
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin
    )
    output shouldBe "Y"
    cpu.state shouldBe State.Halt
  }

  "struct array: copy field between elements without if/else" in {
    val (cpu, output) = runWithBoot(
      """struct Thread
        |    a: int
        |    b: int
        |    uid: int
        |    c: int
        |
        |var threads: [4]Thread
        |var current = 0
        |
        |main() -> int
        |    threads[0].uid = 42
        |    var idx = 1
        |    threads[idx].uid = threads[current].uid
        |    if threads[1].uid == 42
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin
    )
    output shouldBe "Y"
    cpu.state shouldBe State.Halt
  }

  "struct array: copy field with larger struct (16 fields)" in {
    val (cpu, output) = runWithBoot(
      """struct Thread
        |    f0: int
        |    f1: int
        |    f2: int
        |    f3: int
        |    f4: int
        |    f5: int
        |    f6: int
        |    f7: int
        |    f8: int
        |    f9: int
        |    f10: int
        |    f11: int
        |    f12: int
        |    f13: int
        |    uid: int
        |    f15: i64
        |
        |var threads: [8]Thread
        |var current_thread = 0
        |
        |main() -> int
        |    threads[0].uid = 99
        |    var idx = 1
        |    if current_thread >= 0
        |        threads[idx].uid = threads[current_thread].uid
        |    else
        |        threads[idx].uid = 0
        |    if threads[1].uid == 99
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin
    )
    output shouldBe "Y"
    cpu.state shouldBe State.Halt
  }

  "struct array: many field assignments then uid copy (create_thread_pri pattern)" in {
    val (cpu, output) = runWithBoot(
      """struct Thread
        |    ssp: i64
        |    state: int
        |    wake_tick: int
        |    name: i64
        |    priority: int
        |    join_target: int
        |    quantum: int
        |    next: int
        |    ctx_switches: int
        |    cpu_ticks: int
        |    consec_quanta: int
        |    notify_value: int
        |    notify_pending: int
        |    base_priority: int
        |    uid: int
        |    blocked_on: i64
        |
        |var threads: [8]Thread
        |var current_thread = -1
        |var thread_count = 0
        |
        |create_thread(priority: int)
        |    val idx = thread_count
        |    threads[idx].ssp = 0
        |    threads[idx].state = 1
        |    threads[idx].wake_tick = 0
        |    threads[idx].name = 0
        |    threads[idx].priority = priority
        |    threads[idx].join_target = -1
        |    threads[idx].quantum = 10
        |    threads[idx].next = -1
        |    threads[idx].ctx_switches = 0
        |    threads[idx].cpu_ticks = 0
        |    threads[idx].consec_quanta = 0
        |    threads[idx].notify_value = 0
        |    threads[idx].notify_pending = 0
        |    threads[idx].base_priority = priority
        |    threads[idx].blocked_on = 0
        |    if current_thread >= 0
        |        threads[idx].uid = threads[current_thread].uid
        |    else
        |        threads[idx].uid = 0
        |    thread_count += 1
        |
        |main() -> int
        |    // First thread: current_thread = -1, should use else branch
        |    create_thread(0)
        |    current_thread = 0
        |    threads[0].uid = 42
        |    // Second thread: current_thread = 0, should copy uid
        |    create_thread(0)
        |    if threads[1].uid == 42
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin
    )
    output shouldBe "Y"
    cpu.state shouldBe State.Halt
  }
}
