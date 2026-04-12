package io.github.edadma.trisc

class SyslStructTests extends OSKitTestHelpers {

  "Sysl: flat struct field access" in {
    val (_, output) = runWithBoot(
      """struct Pair
        |    a: int
        |    b: int
        |
        |var p: Pair
        |
        |main() -> int
        |    p.a = 42
        |    p.b = 99
        |    if p.a == 42
        |        putchar(65)
        |    if p.b == 99
        |        putchar(66)
        |    0
        |""".stripMargin
    )
    output shouldBe "AB"
  }

  "Sysl: embedded struct dot access" in {
    val (_, output) = runWithBoot(
      """struct Inner
        |    x: int
        |    y: int
        |
        |struct Outer
        |    tag: int
        |    inner: Inner
        |
        |var obj: Outer
        |
        |main() -> int
        |    obj.tag = 1
        |    obj.inner.x = 42
        |    obj.inner.y = 99
        |    if obj.tag == 1
        |        putchar(65)
        |    if obj.inner.x == 42
        |        putchar(66)
        |    if obj.inner.y == 99
        |        putchar(67)
        |    0
        |""".stripMargin
    )
    output shouldBe "ABC"
  }

  "Sysl: address-of embedded struct passed to function" in {
    val (_, output) = runWithBoot(
      """struct Inner
        |    head: int
        |    tail: int
        |
        |struct Outer
        |    owner: int
        |    q: Inner
        |
        |var obj: Outer
        |
        |set_inner(q: *Inner)
        |    q.head = 10
        |    q.tail = 20
        |
        |main() -> int
        |    obj.owner = 5
        |    set_inner(&obj.q)
        |    if obj.owner == 5
        |        putchar(65)
        |    if obj.q.head == 10
        |        putchar(66)
        |    if obj.q.tail == 20
        |        putchar(67)
        |    0
        |""".stripMargin
    )
    output shouldBe "ABC"
  }
}
