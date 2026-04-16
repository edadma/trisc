package io.github.edadma.trisc

class SyslLLVMGapTests extends SyslLLVMTestHelpers {

  // ===== Generics =====

  "generic function" in {
    llvmExit(
      """identity[T](x: T) -> T = x
        |
        |main() -> int = identity(42)
        |""".stripMargin) shouldBe 42
  }

  "generic function two instantiations" in {
    llvmExit(
      """add[T](a: T, b: T) -> T = a + b
        |
        |main() -> int
        |    add(20, 22)
        |""".stripMargin) shouldBe 42
  }

  "generic struct" in {
    llvmExit(
      """struct Pair[T]
        |    first: T
        |    second: T
        |
        |main() -> int
        |    p = Pair(20, 22)
        |    p.first + p.second
        |""".stripMargin) shouldBe 42
  }

  // ===== Traits/impl =====

  "trait impl" in {
    llvmExit(
      """trait Doubler[T]
        |    dbl(x: T) -> T
        |
        |impl Doubler[int]
        |    dbl(x: int) -> int = x * 2
        |
        |main() -> int = Doubler.dbl(21)
        |""".stripMargin) shouldBe 42
  }

  // ===== Methods (dot-call) =====

  "method call on struct" in {
    llvmExit(
      """struct Point
        |    x: int
        |    y: int
        |
        |Point_sum(self: *Point) -> int = self.x + self.y
        |
        |main() -> int
        |    p = Point(20, 22)
        |    p.sum()
        |""".stripMargin) shouldBe 42
  }

  // ===== String indexing =====

  "string index" in {
    llvmExit(
      """main() -> int
        |    s = "ABC"
        |    s[0]
        |""".stripMargin) shouldBe 65  // 'A' = 65
  }

  // ===== Do-while =====

  "do-while loop" in {
    llvmExit(
      """main() -> int
        |    var x = 0
        |    do
        |        x = x + 1
        |    while x < 10
        |    x
        |""".stripMargin) shouldBe 10
  }

  // ===== Unary ops =====

  "unary negation" in {
    llvmExit(
      """main() -> int
        |    x = 42
        |    -(-x)
        |""".stripMargin) shouldBe 42
  }

  "boolean not" in {
    llvmExit(
      """main() -> int
        |    if !false
        |        42
        |    else
        |        0
        |""".stripMargin) shouldBe 42
  }

  // ===== Bitwise ops =====

  "bitwise and" in {
    llvmExit(
      """main() -> int
        |    0xFF & 0x2A
        |""".stripMargin) shouldBe 42
  }

  "bitwise or" in {
    llvmExit(
      """main() -> int
        |    0x20 | 0x0A
        |""".stripMargin) shouldBe 42
  }

  "bitwise xor" in {
    llvmExit(
      """main() -> int
        |    0x3F ^ 0x15
        |""".stripMargin) shouldBe 42
  }

  "left shift" in {
    llvmExit(
      """main() -> int
        |    21 << 1
        |""".stripMargin) shouldBe 42
  }

  "right shift" in {
    llvmExit(
      """main() -> int
        |    84 >> 1
        |""".stripMargin) shouldBe 42
  }

  // ===== Logical ops =====

  "logical and short-circuit" in {
    llvmExit(
      """main() -> int
        |    if true && true
        |        42
        |    else
        |        0
        |""".stripMargin) shouldBe 42
  }

  "logical or short-circuit" in {
    llvmExit(
      """main() -> int
        |    if false || true
        |        42
        |    else
        |        0
        |""".stripMargin) shouldBe 42
  }

  // ===== Range patterns in match =====

  "match range pattern" in {
    llvmExit(
      """main() -> int
        |    x = 5
        |    x match
        |        1..3 -> 10
        |        4..6 -> 42
        |        else -> 0
        |""".stripMargin) shouldBe 42
  }

  // ===== Nested function calls =====

  "nested calls" in {
    llvmExit(
      """add(a: int, b: int) -> int = a + b
        |mul(a: int, b: int) -> int = a * b
        |
        |main() -> int = add(mul(6, 7), 0)
        |""".stripMargin) shouldBe 42
  }

  // ===== Multiple return paths =====

  "early return" in {
    llvmExit(
      """check(x: int) -> int
        |    if x > 10
        |        return 42
        |    0
        |
        |main() -> int = check(20)
        |""".stripMargin) shouldBe 42
  }

  // ===== Tuple type disambiguation =====

  "two different 2-tuples in same function" in {
    llvmExit(
      """main() -> int
        |    val a = (42, true)
        |    val b = (10, 20)
        |    if a.1 then a.0 + b.0 + b.1 else 0
        |""".stripMargin) shouldBe 72
  }

  "function returning different tuple types" in {
    llvmExit(
      """pair_int() -> (int, int) = (10, 32)
        |pair_bool() -> (int, bool) = (42, true)
        |
        |main() -> int
        |    val a, b = pair_int()
        |    val c, d = pair_bool()
        |    if d then a + b + c else 0
        |""".stripMargin) shouldBe 84
  }

  "tuple with string field" in {
    llvmOutput(
      """make() -> (string, int) = ("hello", 5)
        |
        |main()
        |    val s, n = make()
        |    puts(s)
        |""".stripMargin) shouldBe "hello"
  }

  "cross-function tuple type disambiguation" in {
    llvmExit(
      """make_pair() -> (int, bool) = (42, true)
        |make_nums() -> (int, int) = (10, 20)
        |
        |main() -> int
        |    val x, ok = make_pair()
        |    val a, b = make_nums()
        |    if ok then x + a + b else 0
        |""".stripMargin) shouldBe 72
  }

  // ===== Dynamic new + slice element access =====

  "new [n]int store and load" in {
    llvmExit(
      """main() -> int
        |    val a = new [3]int
        |    a[0] = 100
        |    a[1] = 200
        |    a[2] = 300
        |    a[0] + a[1] + a[2]
        |""".stripMargin) shouldBe 600 % 256  // exit code is modulo 256
  }

  "generic reverse from another module" in {
    val (exit, _) = runLLVMMulti(Map(
      "mylib/util/util" ->
        """module mylib.util
          |
          |reverse[T](s: []T) -> []T
          |    val n = len(s)
          |    val buf = new [n]T
          |    for i in 0..<n
          |        buf[i] = s[n - 1 - i]
          |    buf[:]
          |""".stripMargin,
      "main" ->
        """import mylib.util.*
          |
          |main() -> int
          |    val a = new [3]int
          |    a[0] = 1; a[1] = 2; a[2] = 3
          |    val b = reverse(a[:])
          |    b[0] * 100 + b[1] * 10 + b[2]
          |""".stripMargin
    ))
    (exit % 256) shouldBe (321 % 256)
  }

  "new [n]int via slice reverse manual" in {
    llvmExit(
      """main() -> int
        |    val a = new [3]int
        |    a[0] = 1; a[1] = 2; a[2] = 3
        |    val b = new [3]int
        |    val s = a[:]
        |    val n = len(s)
        |    for i in 0..<n
        |        b[i] = s[n - 1 - i]
        |    b[0] * 100 + b[1] * 10 + b[2]
        |""".stripMargin) shouldBe (321 % 256)
  }

  "slice of slices basic" in {
    llvmExit(
      """make_slice(v: int) -> []byte
        |    val b = new [3]byte
        |    b[0] = v; b[1] = v + 1; b[2] = v + 2
        |    b[:]
        |
        |main() -> int
        |    val parts = new [2][]byte
        |    parts[0] = make_slice(10)
        |    parts[1] = make_slice(20)
        |    val s = parts[:]
        |    len(s[0]) + len(s[1])
        |""".stripMargin) shouldBe 6
  }

  "early return with aggregate cleans up locals" in {
    llvmExitWithStd(
      """import std.bytes.*
        |
        |my_join(parts: [][]byte, sep: []byte) -> []byte
        |    val n = len(parts)
        |    if n == 0
        |        val empty = new [0]byte
        |        return empty[:]
        |    var total = 0
        |    var i = 0
        |    while i < n
        |        total = total + len(parts[i])
        |        i++
        |    total = total + len(sep) * (n - 1)
        |    val buf = new [total]byte
        |    var pos = 0
        |    i = 0
        |    while i < n
        |        if i > 0
        |            for j in 0..<len(sep)
        |                buf[pos] = sep[j]
        |                pos++
        |        val plen = len(parts[i])
        |        for j in 0..<plen
        |            buf[pos] = parts[i][j]
        |            pos++
        |        i++
        |    buf[:]
        |
        |main() -> int
        |    val parts = split(as_bytes("a,b,c"), as_bytes(","))
        |    val joined = my_join(parts, as_bytes("-"))
        |    len(joined)
        |""".stripMargin) shouldBe 5
  }

  // ===== String Interpolation =====

  "basic string interpolation" in {
    llvmOutput(
      """main()
        |    val name = "world"
        |    puts(s"hello $name")
        |""".stripMargin) shouldBe "hello world"
  }

  "interpolation with int" in {
    llvmOutput(
      """main()
        |    val x = 42
        |    puts(s"answer is $x")
        |""".stripMargin) shouldBe "answer is 42"
  }

  "interpolation with expression" in {
    llvmOutput(
      """main()
        |    val a = 10
        |    val b = 20
        |    puts(s"sum is ${a + b}")
        |""".stripMargin) shouldBe "sum is 30"
  }

  "interpolation with bool" in {
    llvmOutput(
      """main()
        |    val ok = true
        |    puts(s"result: $ok")
        |""".stripMargin) shouldBe "result: true"
  }

  "interpolation with multiple values" in {
    llvmOutput(
      """main()
        |    val x = 1
        |    val y = 2
        |    val z = 3
        |    puts(s"$x + $y = $z")
        |""".stripMargin) shouldBe "1 + 2 = 3"
  }

  "interpolation with dollar escape" in {
    llvmOutput(
      """main()
        |    puts(s"price: $$5")
        |""".stripMargin) shouldBe "price: $5"
  }

  // ===== Formatted Strings =====

  "formatted string hex" in {
    llvmOutput(
      "main()\n    val x = 255\n    puts(f\"hex: ${x}%x\")\n") shouldBe "hex: ff"
  }

  "formatted string zero-padded" in {
    llvmOutput(
      "main()\n    val x = 42\n    puts(f\"padded: ${x}%05d\")\n") shouldBe "padded: 00042"
  }

  "formatted string width right-aligned" in {
    llvmOutput(
      "main()\n    val x = 42\n    puts(f\"[${x}%5d]\")\n") shouldBe "[   42]"
  }

  "formatted string width left-aligned" in {
    llvmOutput(
      "main()\n    val x = 42\n    puts(f\"[${x}%-5d]\")\n") shouldBe "[42   ]"
  }

  "formatted string uppercase hex" in {
    llvmOutput(
      "main()\n    val x = 255\n    puts(f\"${x}%X\")\n") shouldBe "FF"
  }

  "formatted string with sign" in {
    llvmOutput(
      "main()\n    val x = 42\n    puts(f\"${x}%+d\")\n") shouldBe "+42"
  }

  // ===== Dynamic array size =====

  "dynamic new array size" in {
    llvmExit(
      """main() -> int
        |    val n = 3
        |    val a = new [n]int
        |    a[0] = 10
        |    a[1] = 20
        |    a[2] = 30
        |    a[0] + a[1] + a[2]
        |""".stripMargin) shouldBe 60
  }

  "repeat-like nested loop with dynamic array" in {
    llvmExit(
      """main() -> int
        |    val s = new [2]byte
        |    s[0] = byte(65)
        |    s[1] = byte(66)
        |    val slen = 2
        |    val n = 3
        |    val buf = new [slen * n]byte
        |    for k in 0..<n
        |        for j in 0..<slen
        |            buf[k * slen + j] = s[j]
        |    int(buf[0]) + int(buf[5])
        |""".stripMargin) shouldBe 131
  }

  "string from byte slice" in {
    llvmOutput(
      """main()
        |    val a = new [5]byte
        |    a[0] = byte(72)
        |    a[1] = byte(69)
        |    a[2] = byte(76)
        |    a[3] = byte(76)
        |    a[4] = byte(79)
        |    val s = a[:]
        |    puts(string(s))
        |""".stripMargin) shouldBe "HELLO"
  }

  "string from returned byte slice" in {
    llvmOutput(
      """make() -> []byte
        |    val a = new [5]byte
        |    a[0] = byte(72)
        |    a[1] = byte(69)
        |    a[2] = byte(76)
        |    a[3] = byte(76)
        |    a[4] = byte(79)
        |    a[:]
        |
        |main()
        |    val result = make()
        |    puts(string(result))
        |""".stripMargin) shouldBe "HELLO"
  }

  "repeat function return len" in {
    llvmExit(
      """repeat(s: []byte, n: int) -> []byte
        |    if n <= 0
        |        val empty = new [0]byte
        |        return empty[:]
        |    val slen = len(s)
        |    val buf = new [slen * n]byte
        |    for k in 0..<n
        |        for j in 0..<slen
        |            buf[k * slen + j] = s[j]
        |    buf[:]
        |
        |main() -> int
        |    val src = new [2]byte
        |    src[0] = byte(97)
        |    src[1] = byte(98)
        |    val result = repeat(src[:], 3)
        |    len(result)
        |""".stripMargin) shouldBe 6
  }

  "repeat no early return" in {
    llvmOutput(
      """repeat(s: []byte, n: int) -> []byte
        |    val slen = len(s)
        |    val buf = new [slen * n]byte
        |    for k in 0..<n
        |        for j in 0..<slen
        |            buf[k * slen + j] = s[j]
        |    buf[:]
        |
        |main()
        |    val src = new [2]byte
        |    src[0] = byte(97)
        |    src[1] = byte(98)
        |    val result = repeat(src[:], 3)
        |    puts(string(result))
        |""".stripMargin) shouldBe "ababab"
  }

  "repeat with early return debug" in {
    llvmExit(
      """repeat(s: []byte, n: int) -> []byte
        |    if n <= 0
        |        val empty = new [0]byte
        |        return empty[:]
        |    val slen = len(s)
        |    val buf = new [slen * n]byte
        |    for k in 0..<n
        |        for j in 0..<slen
        |            buf[k * slen + j] = s[j]
        |    buf[:]
        |
        |main() -> int
        |    val src = new [2]byte
        |    src[0] = byte(97)
        |    src[1] = byte(98)
        |    val result = repeat(src[:], 3)
        |    int(result[0])
        |""".stripMargin) shouldBe 97
  }

  "repeat with early return" in {
    llvmOutput(
      """repeat(s: []byte, n: int) -> []byte
        |    if n <= 0
        |        val empty = new [0]byte
        |        return empty[:]
        |    val slen = len(s)
        |    val buf = new [slen * n]byte
        |    for k in 0..<n
        |        for j in 0..<slen
        |            buf[k * slen + j] = s[j]
        |    buf[:]
        |
        |main()
        |    val src = new [2]byte
        |    src[0] = byte(97)
        |    src[1] = byte(98)
        |    val result = repeat(src[:], 3)
        |    puts(string(result))
        |""".stripMargin) shouldBe "ababab"
  }

  "dynamic new array with multiply" in {
    llvmExit(
      """main() -> int
        |    val slen = 2
        |    val n = 3
        |    val buf = new [slen * n]byte
        |    buf[0] = byte(65)
        |    buf[5] = byte(70)
        |    int(buf[0]) + int(buf[5])
        |""".stripMargin) shouldBe 135
  }

  // ===== Generic function returning slice =====

  "generic clone function" in {
    llvmExit(
      """clone[T](s: []T) -> []T
        |    val n = len(s)
        |    val buf = new [n]T
        |    for i in 0..<n
        |        buf[i] = s[i]
        |    buf[:]
        |
        |main() -> int
        |    val a = new [3]int
        |    a[0] = 10
        |    a[1] = 20
        |    a[2] = 30
        |    val b = clone(a[:])
        |    b[0] + b[1] + b[2]
        |""".stripMargin) shouldBe 60
  }

  "array index with subtraction expression" in {
    llvmExit(
      """main() -> int
        |    val a = new [3]int
        |    a[0] = 10
        |    a[1] = 20
        |    a[2] = 30
        |    val n = 3
        |    a[n - 1]
        |""".stripMargin) shouldBe 30
  }

  "array index with n - 1 - i" in {
    llvmExit(
      """main() -> int
        |    val a = new [3]int
        |    a[0] = 10
        |    a[1] = 20
        |    a[2] = 30
        |    val n = 3
        |    val i = 0
        |    a[n - 1 - i]
        |""".stripMargin) shouldBe 30
  }

  "write one ref array element from another" in {
    llvmExit(
      """main() -> int
        |    val a = new [3]int
        |    a[0] = 10
        |    a[1] = 20
        |    a[2] = 30
        |    val buf = new [3]int
        |    buf[0] = a[2]
        |    buf[0]
        |""".stripMargin) shouldBe 30
  }

  "read three ref array elements" in {
    llvmExit(
      """main() -> int
        |    val a = new [3]int
        |    a[0] = 10
        |    a[1] = 20
        |    a[2] = 30
        |    a[0] + a[1] + a[2]
        |""".stripMargin) shouldBe 60
  }

  "write ref array with compound read index" in {
    llvmOutput(
      """main()
        |    val a = new [3]int
        |    a[0] = 10
        |    a[1] = 20
        |    a[2] = 30
        |    val buf = new [3]int
        |    buf[0] = a[2]
        |    buf[1] = a[1]
        |    buf[2] = a[0]
        |    println(buf[0] * 100 + buf[1] * 10 + buf[2])
        |""".stripMargin) shouldBe "3210"
  }

  "write ref array in loop with compound index" in {
    llvmOutput(
      """main()
        |    val a = new [3]int
        |    a[0] = 1
        |    a[1] = 2
        |    a[2] = 3
        |    val n = 3
        |    val buf = new [3]int
        |    for i in 0..<n
        |        buf[i] = a[n - 1 - i]
        |    println(buf[0] * 100 + buf[1] * 10 + buf[2])
        |""".stripMargin) shouldBe "321"
  }

  "generic reverse function" in {
    llvmOutput(
      """reverse[T](s: []T) -> []T
        |    val n = len(s)
        |    val buf = new [n]T
        |    for i in 0..<n
        |        buf[i] = s[n - 1 - i]
        |    buf[:]
        |
        |main()
        |    val a = new [3]int
        |    a[0] = 1
        |    a[1] = 2
        |    a[2] = 3
        |    val b = reverse(a[:])
        |    println(b[0] * 100 + b[1] * 10 + b[2])
        |""".stripMargin) shouldBe "321"
  }

  "generic concat function" in {
    llvmOutput(
      """concat[T](a: []T, b: []T) -> []T
        |    val na = len(a)
        |    val nb = len(b)
        |    val buf = new [na + nb]T
        |    for i in 0..<na
        |        buf[i] = a[i]
        |    for j in 0..<nb
        |        buf[na + j] = b[j]
        |    buf[:]
        |
        |main()
        |    val a = new [2]int
        |    a[0] = 1
        |    a[1] = 2
        |    val b = new [3]int
        |    b[0] = 3
        |    b[1] = 4
        |    b[2] = 5
        |    val c = concat(a[:], b[:])
        |    println(len(c) * 100 + c[0] + c[4])
        |""".stripMargin) shouldBe "506"
  }

  // ===== Bug: val initializer values wrong for non-sequential constants =====

  "val with gaps single file" in {
    llvmOutput(
      """val A = 0
        |val B = 2
        |val C = 3
        |val D = 5
        |val E = 9
        |
        |main()
        |    println(A)
        |    println(B)
        |    println(C)
        |    println(D)
        |    println(E)
        |""".stripMargin) shouldBe "0\n2\n3\n5\n9"
  }

  "val with gaps cross module" in {
    llvmOutputMulti(Map(
      "mylib/consts/consts" ->
        """module mylib.consts
          |
          |val A = 0
          |val B = 2
          |val C = 3
          |val D = 5
          |val E = 9
          |""".stripMargin,
      "main" ->
        """import mylib.consts.*
          |
          |main()
          |    println(A)
          |    println(B)
          |    println(C)
          |    println(D)
          |    println(E)
          |""".stripMargin
    )) shouldBe "0\n2\n3\n5\n9"
  }

  "val with gaps IR check" in {
    val ir = compileLLVM(
      """val A = 0
        |val B = 2
        |val C = 3
        |val D = 5
        |val E = 9
        |
        |main() -> int
        |    A + B + C + D + E
        |""".stripMargin)
    ir should include("@A = global i32 0")
    ir should include("@B = global i32 2")
    ir should include("@C = global i32 3")
    ir should include("@D = global i32 5")
    ir should include("@E = global i32 9")
  }

  "val with gaps cross module IR check" in {
    val ir = compileLLVMMulti(Map(
      "mylib/consts/consts" ->
        """module mylib.consts
          |
          |val A = 0
          |val B = 2
          |val C = 3
          |val D = 5
          |val E = 9
          |""".stripMargin,
      "main" ->
        """import mylib.consts.*
          |
          |main() -> int
          |    A + B + C + D + E
          |""".stripMargin
    ))
    ir should include("@mylib_consts__A = global i32 0")
    ir should include("@mylib_consts__B = global i32 2")
    ir should include("@mylib_consts__C = global i32 3")
    ir should include("@mylib_consts__D = global i32 5")
    ir should include("@mylib_consts__E = global i32 9")
  }

  // ===== Bug: void function calls before final return expression are dropped =====

  "void call before return expression" in {
    llvmOutput(
      """foo() -> int
        |    puts("hello")
        |    0
        |
        |main()
        |    foo()
        |""".stripMargin) shouldBe "hello"
  }

  "multiple void calls before return expression" in {
    llvmOutput(
      """foo() -> int
        |    puts("A")
        |    puts("B")
        |    puts("C")
        |    0
        |
        |main()
        |    foo()
        |""".stripMargin) shouldBe "A\nB\nC"
  }

  "user-defined void call before return" in {
    llvmOutput(
      """greet()
        |    puts("hi")
        |
        |foo() -> int
        |    greet()
        |    42
        |
        |main()
        |    println(foo())
        |""".stripMargin) shouldBe "hi\n42"
  }

  "void call before return IR check" in {
    val ir = compileLLVM(
      """extern uart_putc_raw(c: int)
        |
        |foo() -> int
        |    uart_putc_raw(68)
        |    0
        |""".stripMargin)
    ir should include("call void @uart_putc_raw")
  }

  "void call in expr body dropped" in {
    // ExprBody form: foo() -> int = bar(); 0
    // The semicolon produces a sequence expr — check if void call survives
    val ir = compileLLVM(
      """side_effect(x: int)
        |    puts("side")
        |
        |foo() -> int
        |    side_effect(1)
        |    0
        |
        |main() -> int
        |    foo()
        |""".stripMargin)
    // The call to side_effect should appear in foo's body
    ir should include("call void @side_effect")
  }

  // ===== Bug 3: TAddrOfIndex on *byte parameter =====

  "addr of index on *byte parameter" in {
    // Taking &buf[i] where buf is a *byte parameter should compile and work
    llvmExit(
      """get_at(buf: *byte, i: int) -> int
        |    val p: *byte = &buf[i]
        |    int(*p)
        |
        |main() -> int
        |    var data: [4]byte
        |    data[0] = 10
        |    data[1] = 20
        |    data[2] = 42
        |    data[3] = 99
        |    get_at(&data[0], 2)
        |""".stripMargin) shouldBe 42
  }

  // ===== Bug 2: Void call after if-return dropped =====

  "void call between if-return and trailing expression" in {
    // void call after 'if ... return' but before final expression was being dropped
    llvmOutput(
      """foo(flag: int) -> int
        |    if flag > 0
        |        return 1
        |    puts("side-effect")
        |    0
        |
        |main() -> int
        |    foo(0)
        |""".stripMargin) should include("side-effect")
  }

  // ===== Bug 1: Multi-unit val initializer values =====

  "multi-unit val constants preserve declared values" in {
    // Val declarations with non-sequential values should keep their declared values
    val ir = compileLLVMMulti(Map(
      "defs/defs" ->
        """module defs
          |
          |val STATE_READY      = 0
          |val STATE_RUNNING    = 2
          |val STATE_TERMINATED = 3
          |val STATE_BLOCKED    = 5
          |val STATE_SUSPENDED  = 9
          |""".stripMargin,
      "main" ->
        """import defs.*
          |
          |main() -> int
          |    STATE_RUNNING + STATE_BLOCKED + STATE_SUSPENDED
          |""".stripMargin
    ))
    // Check the val globals have correct initializer values
    ir should include("@defs__STATE_RUNNING = global i32 2")
    ir should include("@defs__STATE_BLOCKED = global i32 5")
    ir should include("@defs__STATE_SUSPENDED = global i32 9")
  }

  // ===== Bug: i64 literal cast truncates to i32 =====

  // TODO: un-ignore when i64 cast truncation is fixed. The fix is straightforward
  // but changes RS binary layout which triggers an unrelated boot crash.
  "i64 cast of large literal preserves value" ignore {
    // i64(0x20000FFFFFE0007F) was generating sext i32 <truncated> to i64
    val ir = compileLLVM(
      """main() -> int
        |    var x: i64 = i64(0x20000FFFFFE0007F)
        |    int(x >> 32)
        |""".stripMargin)
    // Should NOT contain sext i32 ... to i64 for this literal
    ir should not include ("sext i32 2305860601397641343")
    // Should contain the literal directly as i64
    ir should include("2305860601397641343")
  }

  "i64 cast of small literal" in {
    // Small values that fit in i32 should still work
    llvmExit(
      """main() -> int
        |    var x: i64 = i64(42)
        |    int(x)
        |""".stripMargin) shouldBe 42
  }
}
