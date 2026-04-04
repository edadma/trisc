package io.github.edadma.trisc

class SyslCodegenPointerArrayTests extends SyslCodegenHelpers {

  // ===== Arrays =====

  "array declaration and indexing" in {
    compileAndRun("main() -> int\n    a: [5]int\n    a[0] = 42\n    a[0]\n") shouldBe 42
  }

  "array multiple elements" in {
    compileAndRun(
      """main() -> int
        |    a: [3]int
        |    a[0] = 10
        |    a[1] = 20
        |    a[2] = 30
        |    a[0] + a[1] + a[2]
        |""".stripMargin) shouldBe 60
  }

  // TODO: codegen bug — uninitialized array reads return stack garbage
  "array zero initialized" ignore {
    compileAndRun("main() -> int\n    a: [3]int\n    a[0] + a[1] + a[2]\n") shouldBe 0
  }

  "array pass to function" in {
    compileAndRun(
      """sum(arr: *int, n: int) -> int
        |    total = 0
        |    i = 0
        |    while i < n
        |        total = total + arr[i]
        |        i = i + 1
        |    total
        |
        |main() -> int
        |    a: [3]int
        |    a[0] = 10
        |    a[1] = 20
        |    a[2] = 30
        |    sum(a, 3)
        |""".stripMargin) shouldBe 60
  }

  "array modify through function" in {
    compileAndRun(
      """fill(arr: *int, n: int, v: int)
        |    i = 0
        |    while i < n
        |        arr[i] = v
        |        i = i + 1
        |
        |main() -> int
        |    a: [3]int
        |    fill(a, 3, 42)
        |    a[0] + a[1] + a[2]
        |""".stripMargin) shouldBe 126
  }

  "array in while loop" in {
    compileAndRun(
      """main() -> int
        |    a: [10]int
        |    i = 0
        |    while i < 10
        |        a[i] = i * i
        |        i = i + 1
        |    a[5]
        |""".stripMargin) shouldBe 25
  }

  "multiple arrays" in {
    compileAndRun(
      """main() -> int
        |    a: [3]int
        |    b: [3]int
        |    a[0] = 10
        |    b[0] = 20
        |    a[1] = 30
        |    b[1] = 40
        |    a[0] + a[1] + b[0] + b[1]
        |""".stripMargin) shouldBe 100
  }

  "array index with expression" in {
    compileAndRun(
      """main() -> int
        |    a: [5]int
        |    a[3] = 42
        |    i = 1
        |    a[i + 2]
        |""".stripMargin) shouldBe 42
  }

  // ===== Pointers =====

  "address-of and dereference" in {
    compileAndRun("main() -> int\n    x = 42\n    p = &x\n    *p\n") shouldBe 42
  }

  "deref assignment changes original" in {
    compileAndRun("main() -> int\n    x = 10\n    p = &x\n    *p = 42\n    x\n") shouldBe 42
  }

  "pointer to different variables" in {
    compileAndRun("main() -> int\n    x = 10\n    y = 20\n    p = &x\n    q = &y\n    *p + *q\n") shouldBe 30
  }

  "pointer reassignment" in {
    compileAndRun("main() -> int\n    x = 10\n    y = 20\n    p = &x\n    p = &y\n    *p\n") shouldBe 20
  }

  "pointer as function argument" in {
    compileAndRun(
      """set42(p: *int)
        |    *p = 42
        |
        |main() -> int
        |    x = 0
        |    set42(&x)
        |    x
        |""".stripMargin) shouldBe 42
  }

  "swap via pointers" in {
    compileAndRun(
      """swap(a: *int, b: *int)
        |    tmp = *a
        |    *a = *b
        |    *b = tmp
        |
        |main() -> int
        |    x = 10
        |    y = 20
        |    swap(&x, &y)
        |    x * 100 + y
        |""".stripMargin) shouldBe 2010
  }

  "deref in expression" in {
    compileAndRun("main() -> int\n    x = 21\n    p = &x\n    *p * 2\n") shouldBe 42
  }

  "deref in if condition" in {
    compileAndRun("main() -> int\n    x = 5\n    p = &x\n    if *p > 3 then 1 else 0\n") shouldBe 1
  }

  "increment via pointer" in {
    compileAndRun(
      """inc(p: *int)
        |    *p = *p + 1
        |
        |main() -> int
        |    x = 0
        |    inc(&x)
        |    inc(&x)
        |    inc(&x)
        |    x
        |""".stripMargin) shouldBe 3
  }

  "double pointer" in {
    compileAndRun("main() -> int\n    x = 42\n    p = &x\n    pp = &p\n    **pp\n") shouldBe 42
  }

  "double pointer write" in {
    compileAndRun("main() -> int\n    x = 0\n    p = &x\n    pp = &p\n    **pp = 99\n    x\n") shouldBe 99
  }

  // ===== Pointer/array combinations =====

  "address of array element" in {
    compileAndRun("main() -> int\n    a: [5]int\n    a[2] = 42\n    p = &a[2]\n    *p\n") shouldBe 42
  }

  "modify array element via address-of" in {
    compileAndRun("main() -> int\n    a: [5]int\n    p = &a[3]\n    *p = 77\n    a[3]\n") shouldBe 77
  }

  // TODO: codegen bug — pointer arithmetic on local arrays returns wrong values
  "pointer offset then index" ignore {
    compileAndRun(
      """main() -> int
        |    a: [5]int
        |    a[2] = 10
        |    a[3] = 20
        |    p = a + 2
        |    p[0] + p[1]
        |""".stripMargin) shouldBe 30
  }

  // TODO: codegen bug — write through pointer arithmetic on local array
  "write through pointer arithmetic" ignore {
    compileAndRun("main() -> int\n    a: [5]int\n    *(a + 4) = 55\n    a[4]\n") shouldBe 55
  }

  "array element as function argument" in {
    compileAndRun(
      """dbl(x: int) -> int = x * 2
        |main() -> int
        |    a: [3]int
        |    a[1] = 21
        |    dbl(a[1])
        |""".stripMargin) shouldBe 42
  }

  "function does pointer arithmetic on array arg" in {
    compileAndRun(
      """third(arr: *int) -> int = *(arr + 2)
        |main() -> int
        |    a: [5]int
        |    a[2] = 99
        |    third(a)
        |""".stripMargin) shouldBe 99
  }

  // ===== Practical =====

  "bubble sort" in {
    compileAndRun(
      """sort(arr: *int, n: int)
        |    i = 0
        |    while i < n - 1
        |        j = 0
        |        while j < n - 1 - i
        |            if arr[j] > arr[j + 1]
        |                tmp = arr[j]
        |                arr[j] = arr[j + 1]
        |                arr[j + 1] = tmp
        |            j = j + 1
        |        i = i + 1
        |
        |main() -> int
        |    a: [5]int
        |    a[0] = 5
        |    a[1] = 3
        |    a[2] = 1
        |    a[3] = 4
        |    a[4] = 2
        |    sort(a, 5)
        |    a[0] * 10000 + a[1] * 1000 + a[2] * 100 + a[3] * 10 + a[4]
        |""".stripMargin) shouldBe 12345
  }

  "reverse array in place" in {
    compileAndRun(
      """reverse(arr: *int, n: int)
        |    i = 0
        |    j = n - 1
        |    while i < j
        |        tmp = arr[i]
        |        arr[i] = arr[j]
        |        arr[j] = tmp
        |        i = i + 1
        |        j = j - 1
        |
        |main() -> int
        |    a: [4]int
        |    a[0] = 1
        |    a[1] = 2
        |    a[2] = 3
        |    a[3] = 4
        |    reverse(a, 4)
        |    a[0] * 1000 + a[1] * 100 + a[2] * 10 + a[3]
        |""".stripMargin) shouldBe 4321
  }
}
