package io.github.edadma.trisc

class SyslSliceStringTests extends SyslTestHelpers {

  // ===== String type basics =====

  "string variable inferred type" in {
    output(
      """main() -> int
        |    s = "hello"
        |    puts(s)
        |    0
        |""".stripMargin) shouldBe "hello"
  }

  "string variable explicit type" in {
    output(
      """main() -> int
        |    s: string = "world"
        |    puts(s)
        |    0
        |""".stripMargin) shouldBe "world"
  }

  "empty string" in {
    eval("main() -> int = len(\"\")\n") shouldBe 0
  }

  "empty string indexing fails" in {
    assertThrows[RuntimeException] {
      eval(
        """main() -> int
          |    s = ""
          |    s[0]
          |""".stripMargin)
    }
  }

  // Note: single-char strings like "x" are parsed as char literals, not strings.
  // Use multi-char strings to test string len.

  "string len of two chars" in {
    eval("main() -> int = len(\"ab\")\n") shouldBe 2
  }

  "string len of multi-byte UTF-8" in {
    eval("main() -> int = len(\"café\")\n") shouldBe 5  // é = 2 UTF-8 bytes, total 5
  }

  "string len of CJK" in {
    eval("main() -> int = len(\"世界\")\n") shouldBe 6  // 世 = 3 UTF-8 bytes × 2
  }

  // ===== String indexing =====

  "string first byte" in {
    eval("main() -> int = \"Hello\"[0]\n") shouldBe 72  // 'H'
  }

  "string last byte" in {
    eval("main() -> int = \"Hello\"[4]\n") shouldBe 111  // 'o'
  }

  "string index out of bounds high" in {
    assertThrows[RuntimeException] {
      eval("main() -> int = \"Hello\"[5]\n")
    }
  }

  "string index out of bounds negative" in {
    assertThrows[RuntimeException] {
      eval("main() -> int = \"Hello\"[-1]\n")
    }
  }

  "string index with variable" in {
    eval(
      """main() -> int
        |    s = "ABC"
        |    i = 2
        |    s[i]
        |""".stripMargin) shouldBe 67  // 'C'
  }

  "string index in loop" in {
    output(
      """main() -> int
        |    s = "Hi!"
        |    i = 0
        |    while i < len(s)
        |        putchar(s[i])
        |        i += 1
        |    0
        |""".stripMargin) shouldBe "Hi!"
  }

  // ===== String as parameter =====

  "string parameter" in {
    eval(
      """first(s: string) -> int = s[0]
        |main() -> int = first("ZZ")
        |""".stripMargin) shouldBe 90  // 'Z' = 90
  }

  "string parameter len" in {
    eval(
      """length(s: string) -> int = len(s)
        |main() -> int = length("Hello")
        |""".stripMargin) shouldBe 5
  }

  "string parameter multiple calls" in {
    eval(
      """length(s: string) -> int = len(s)
        |main() -> int = length("ab") + length("cde")
        |""".stripMargin) shouldBe 5
  }

  // ===== puts and puti builtins =====

  "puts empty string" in {
    output(
      """main() -> int
        |    puts("")
        |    0
        |""".stripMargin) shouldBe ""
  }

  "puts with variable" in {
    output(
      """main() -> int
        |    s = "test"
        |    puts(s)
        |    0
        |""".stripMargin) shouldBe "test"
  }

  "puti zero" in {
    output(
      """main() -> int
        |    puti(0)
        |    0
        |""".stripMargin) shouldBe "0"
  }

  "puti negative" in {
    output(
      """main() -> int
        |    puti(-42)
        |    0
        |""".stripMargin) shouldBe "-42"
  }

  "puts then puti" in {
    output(
      """main() -> int
        |    puts("x=")
        |    puti(10)
        |    0
        |""".stripMargin) shouldBe "x=10"
  }

  // ===== len() on different types =====

  "len on fixed array" in {
    eval(
      """main() -> int
        |    a: [5]int
        |    len(a)
        |""".stripMargin) shouldBe 5
  }

  "len on string variable" in {
    eval(
      """main() -> int
        |    s = "abc"
        |    len(s)
        |""".stripMargin) shouldBe 3
  }

  // ===== cap() =====

  "cap on fixed array" in {
    eval(
      """main() -> int
        |    a: [7]int
        |    cap(a)
        |""".stripMargin) shouldBe 7
  }

  // ===== Analyzer errors =====

  "len with no args rejected" in {
    assertThrows[RuntimeException] {
      eval("main() -> int = len()\n")
    }
  }

  "len with two args rejected" in {
    assertThrows[RuntimeException] {
      eval("main() -> int = len(\"a\", \"b\")\n")
    }
  }

  "len on int rejected" in {
    assertThrows[RuntimeException] {
      eval(
        """main() -> int
          |    x = 42
          |    len(x)
          |""".stripMargin)
    }
  }

  "cap on string rejected" in {
    assertThrows[RuntimeException] {
      eval(
        """main() -> int
          |    s = "hello"
          |    cap(s)
          |""".stripMargin)
    }
  }

  "cap on int rejected" in {
    assertThrows[RuntimeException] {
      eval(
        """main() -> int
          |    x = 42
          |    cap(x)
          |""".stripMargin)
    }
  }

  "deref string rejected" in {
    assertThrows[RuntimeException] {
      eval(
        """main() -> int
          |    s = "hello"
          |    *s
          |""".stripMargin)
    }
  }

  "pointer arithmetic on string rejected" in {
    assertThrows[RuntimeException] {
      eval(
        """main() -> int
          |    s = "hello"
          |    s + 1
          |""".stripMargin)
    }
  }

  "index bool rejected" in {
    assertThrows[RuntimeException] {
      eval(
        """main() -> int
          |    b = true
          |    b[0]
          |""".stripMargin)
    }
  }

  // ===== sizeof =====

  "sizeof string" in {
    eval("main() -> int = sizeof(string)\n") shouldBe 16
  }

  // ===== Type parsing =====

  "parse string type in function signature" in {
    eval(
      """id(s: string) -> string = s
        |main() -> int
        |    t = id("ok")
        |    len(t)
        |""".stripMargin) shouldBe 2
  }

  "parse string return type" in {
    output(
      """greeting() -> string = "hi"
        |main() -> int
        |    puts(greeting())
        |    0
        |""".stripMargin) shouldBe "hi"
  }

  // ===== String immutability (no index assignment) =====

  "string index assignment rejected" in {
    // Strings are immutable — cannot assign to s[i]
    // The analyzer should reject this because StringType indexing returns I8
    // but IndexAssignStmt needs a mutable target
    // For now this may or may not be caught — test documents the intent
    assertThrows[RuntimeException] {
      eval(
        """main() -> int
          |    s = "hello"
          |    s[0] = 72
          |    0
          |""".stripMargin)
    }
  }
}
