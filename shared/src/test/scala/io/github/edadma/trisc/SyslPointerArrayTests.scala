package io.github.edadma.trisc

class SyslPointerArrayTests extends SyslTestHelpers {

  // ===== Arrays =====

  "array declaration and indexing" in {
    eval(
      """main() -> int
        |    a: [5]int
        |    a[0] = 42
        |    a[0]
        |""".stripMargin) shouldBe 42
  }

  "array multiple elements" in {
    eval(
      """main() -> int
        |    a: [3]int
        |    a[0] = 10
        |    a[1] = 20
        |    a[2] = 30
        |    a[0] + a[1] + a[2]
        |""".stripMargin) shouldBe 60
  }

  "array zero initialized" in {
    eval(
      """main() -> int
        |    a: [3]int
        |    a[0] + a[1] + a[2]
        |""".stripMargin) shouldBe 0
  }

  "array decays to pointer" in {
    eval(
      """main() -> int
        |    a: [5]int
        |    a[0] = 42
        |    p = a
        |    *p
        |""".stripMargin) shouldBe 42
  }

  "pointer from array with indexing" in {
    eval(
      """main() -> int
        |    a: [5]int
        |    a[2] = 99
        |    p = a
        |    p[2]
        |""".stripMargin) shouldBe 99
  }

  "pointer arithmetic on array" in {
    eval(
      """main() -> int
        |    a: [5]int
        |    a[3] = 77
        |    p = a + 3
        |    *p
        |""".stripMargin) shouldBe 77
  }

  "pointer subtraction on array" in {
    eval(
      """main() -> int
        |    a: [5]int
        |    a[1] = 88
        |    p = a + 3
        |    q = p - 2
        |    *q
        |""".stripMargin) shouldBe 88
  }

  "array pass to function" in {
    eval(
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
    eval(
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

  "p[0] same as *p for scalar pointer" in {
    eval(
      """main() -> int
        |    x = 42
        |    p = &x
        |    p[0]
        |""".stripMargin) shouldBe 42
  }

  "array in while loop" in {
    eval(
      """main() -> int
        |    a: [10]int
        |    i = 0
        |    while i < 10
        |        a[i] = i * i
        |        i = i + 1
        |    a[5]
        |""".stripMargin) shouldBe 25
  }

  "array with pointer arithmetic in expression" in {
    eval(
      """main() -> int
        |    a: [5]int
        |    a[0] = 1
        |    a[1] = 2
        |    a[2] = 3
        |    *(a + 0) + *(a + 1) + *(a + 2)
        |""".stripMargin) shouldBe 6
  }

  "nested array indexing via pointer" in {
    eval(
      """main() -> int
        |    a: [3]int
        |    a[0] = 100
        |    a[1] = 200
        |    a[2] = 300
        |    p = a
        |    p[0] + p[1] + p[2]
        |""".stripMargin) shouldBe 600
  }

  // ===== Pointers =====

  "address-of and dereference basic" in {
    eval(
      """main() -> int
        |    x = 42
        |    p = &x
        |    *p
        |""".stripMargin) shouldBe 42
  }

  "deref assignment changes original variable" in {
    eval(
      """main() -> int
        |    x = 10
        |    p = &x
        |    *p = 42
        |    x
        |""".stripMargin) shouldBe 42
  }

  "pointer to different variables" in {
    eval(
      """main() -> int
        |    x = 10
        |    y = 20
        |    p = &x
        |    q = &y
        |    *p + *q
        |""".stripMargin) shouldBe 30
  }

  "pointer reassignment" in {
    eval(
      """main() -> int
        |    x = 10
        |    y = 20
        |    p = &x
        |    p = &y
        |    *p
        |""".stripMargin) shouldBe 20
  }

  "pointer as function argument (pass by pointer)" in {
    eval(
      """set_to_42(p: *int)
        |    *p = 42
        |
        |main() -> int
        |    x = 0
        |    set_to_42(&x)
        |    x
        |""".stripMargin) shouldBe 42
  }

  "swap via pointers" in {
    eval(
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

  "pointer to global variable" in {
    eval(
      """g = 0
        |
        |set_global(p: *int)
        |    *p = 99
        |
        |main() -> int
        |    set_global(&g)
        |    g
        |""".stripMargin) shouldBe 99
  }

  "deref in expression" in {
    eval(
      """main() -> int
        |    x = 21
        |    p = &x
        |    *p * 2
        |""".stripMargin) shouldBe 42
  }

  "deref in if condition" in {
    eval(
      """main() -> int
        |    x = 5
        |    p = &x
        |    if *p > 3 then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "deref in function call argument" in {
    eval(
      """double(n: int) -> int = n * 2
        |
        |main() -> int
        |    x = 21
        |    p = &x
        |    double(*p)
        |""".stripMargin) shouldBe 42
  }

  "increment via pointer" in {
    eval(
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
    eval(
      """main() -> int
        |    x = 42
        |    p = &x
        |    pp = &p
        |    **pp
        |""".stripMargin) shouldBe 42
  }

  "double pointer write" in {
    eval(
      """main() -> int
        |    x = 0
        |    p = &x
        |    pp = &p
        |    **pp = 99
        |    x
        |""".stripMargin) shouldBe 99
  }

  "double pointer redirect" in {
    eval(
      """main() -> int
        |    x = 10
        |    y = 20
        |    p = &x
        |    pp = &p
        |    *pp = &y
        |    *p
        |""".stripMargin) shouldBe 20
  }

  "pointer in while loop" in {
    eval(
      """main() -> int
        |    x = 0
        |    p = &x
        |    i = 0
        |    while i < 5
        |        *p = *p + i
        |        i = i + 1
        |    x
        |""".stripMargin) shouldBe 10
  }

  // ===== Pointer/array combination tests =====

  "address of array element" in {
    eval(
      """main() -> int
        |    a: [5]int
        |    a[2] = 42
        |    p = &a[2]
        |    *p
        |""".stripMargin) shouldBe 42
  }

  "modify array element via address-of" in {
    eval(
      """main() -> int
        |    a: [5]int
        |    p = &a[3]
        |    *p = 77
        |    a[3]
        |""".stripMargin) shouldBe 77
  }

  "pointer offset then index" in {
    eval(
      """main() -> int
        |    a: [5]int
        |    a[2] = 10
        |    a[3] = 20
        |    p = a + 2
        |    p[0] + p[1]
        |""".stripMargin) shouldBe 30
  }

  "write through pointer arithmetic" in {
    eval(
      """main() -> int
        |    a: [5]int
        |    *(a + 4) = 55
        |    a[4]
        |""".stripMargin) shouldBe 55
  }

  "array element as function argument" in {
    eval(
      """double(x: int) -> int = x * 2
        |
        |main() -> int
        |    a: [3]int
        |    a[1] = 21
        |    double(a[1])
        |""".stripMargin) shouldBe 42
  }

  "function does pointer arithmetic on array arg" in {
    eval(
      """third(arr: *int) -> int = *(arr + 2)
        |
        |main() -> int
        |    a: [5]int
        |    a[2] = 99
        |    third(a)
        |""".stripMargin) shouldBe 99
  }

  "multiple arrays" in {
    eval(
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
    eval(
      """main() -> int
        |    a: [5]int
        |    a[3] = 42
        |    i = 1
        |    a[i + 2]
        |""".stripMargin) shouldBe 42
  }

  "deref of address-of array element" in {
    eval(
      """main() -> int
        |    a: [3]int
        |    a[1] = 88
        |    *(&a[1])
        |""".stripMargin) shouldBe 88
  }

  "array decay and pointer are interchangeable" in {
    eval(
      """main() -> int
        |    a: [3]int
        |    a[0] = 1
        |    a[1] = 2
        |    a[2] = 3
        |    p = a
        |    q = &a[0]
        |    *p + *q + p[1] + q[2]
        |""".stripMargin) shouldBe 7
  }

  "bubble sort" in {
    eval(
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
    eval(
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

  "pointer walks array" in {
    eval(
      """main() -> int
        |    a: [5]int
        |    i = 0
        |    while i < 5
        |        a[i] = (i + 1) * 10
        |        i = i + 1
        |    sum = 0
        |    p = a
        |    i = 0
        |    while i < 5
        |        sum = sum + *p
        |        p = p + 1
        |        i = i + 1
        |    sum
        |""".stripMargin) shouldBe 150
  }
}
