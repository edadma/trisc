package io.github.edadma.trisc

class SyslCodegenByteSwapTests extends SyslCodegenHelpers {

  "byte-level write to int memory preserves value" in {
    compileAndRun(
      """main() -> int
        |    var x = 42
        |    val p = *byte(i64(&x))
        |    val b0 = p[0]
        |    val b1 = p[1]
        |    val b2 = p[2]
        |    val b3 = p[3]
        |    p[0] = b0
        |    p[1] = b1
        |    p[2] = b2
        |    p[3] = b3
        |    x
        |""".stripMargin) shouldBe 42
  }

  "byte-level swap of two ints" in {
    compileAndRun(
      """main() -> int
        |    var a = 10
        |    var b = 20
        |    val pa = *byte(i64(&a))
        |    val pb = *byte(i64(&b))
        |    for var i = 0; i < sizeof(int); i++
        |        val t = pa[i]
        |        pa[i] = pb[i]
        |        pb[i] = t
        |    a * 100 + b
        |""".stripMargin) shouldBe 2010
  }

  "byte swap via function on array elements" in {
    compileAndRun(
      """swap(a: *byte, b: *byte, n: int)
        |    for var i = 0; i < n; i++
        |        val t = a[i]
        |        a[i] = b[i]
        |        b[i] = t
        |
        |main() -> int
        |    var arr: [3]int
        |    arr[0] = 10
        |    arr[1] = 20
        |    arr[2] = 30
        |    val base = *byte(i64(&arr[0]))
        |    val size = sizeof(int)
        |    swap(base, base + size, size)
        |    arr[0] * 100 + arr[1]
        |""".stripMargin) shouldBe 2010
  }

  "byte swap via function pointer on array elements" in {
    compileAndRun(
      """do_swap(a: *byte, b: *byte, n: int)
        |    for var i = 0; i < n; i++
        |        val t = a[i]
        |        a[i] = b[i]
        |        b[i] = t
        |
        |call_swap(base: *byte, i: int, j: int, size: int, swapper: func(*byte, *byte, int))
        |    swapper(base + i * size, base + j * size, size)
        |
        |main() -> int
        |    var arr: [3]int
        |    arr[0] = 10
        |    arr[1] = 20
        |    arr[2] = 30
        |    val base = *byte(i64(&arr[0]))
        |    call_swap(base, 0, 1, sizeof(int), do_swap)
        |    arr[0] * 100 + arr[1]
        |""".stripMargin) shouldBe 2010
  }

  "insertion sort via byte-level swap and comparator" in {
    compileAndRun(
      """_swap(a: *byte, b: *byte, n: int)
        |    for var i = 0; i < n; i++
        |        val t = a[i]
        |        a[i] = b[i]
        |        b[i] = t
        |
        |cmp_int(a: *byte, b: *byte) -> int
        |    val x = *(*int(i64(a)))
        |    val y = *(*int(i64(b)))
        |    if x < y then -1
        |    elif x > y then 1
        |    else 0
        |
        |main() -> int
        |    var arr: [3]int
        |    arr[0] = 30
        |    arr[1] = 10
        |    arr[2] = 20
        |    val base = *byte(i64(&arr[0]))
        |    val size = sizeof(int)
        |    for var i = 1; i < 3; i++
        |        var j = i
        |        while j > 0 && cmp_int(base + (j - 1) * size, base + j * size) > 0
        |            _swap(base + (j - 1) * size, base + j * size, size)
        |            j--
        |    arr[0] * 10000 + arr[1] * 100 + arr[2]
        |""".stripMargin) shouldBe 102030
  }
}
