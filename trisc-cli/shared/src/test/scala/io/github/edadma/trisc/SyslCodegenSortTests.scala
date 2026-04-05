package io.github.edadma.trisc

class SyslCodegenSortTests extends SyslCodegenHelpers {

  private val stdlibSource = scala.io.Source.fromFile("posix/stdlib/stdlib.sysl").mkString

  private def sortSources(mainSource: String): Map[String, String] =
    Map(
      "posix/stdlib/stdlib" -> stdlibSource,
      "main" -> mainSource,
    )

  private val cmpInt =
    """cmp_int(a: *byte, b: *byte) -> int
      |    val x = *(*int(i64(a)))
      |    val y = *(*int(i64(b)))
      |    if x < y then -1
      |    elif x > y then 1
      |    else 0
      |""".stripMargin

  // ===== qsort =====

  "qsort sorts 5 ints ascending" in {
    compileMultiAndRun(sortSources(
      s"""import posix.stdlib.*
         |
         |$cmpInt
         |main() -> int
         |    var arr: [5]int
         |    arr[0] = 5
         |    arr[1] = 3
         |    arr[2] = 1
         |    arr[3] = 4
         |    arr[4] = 2
         |    qsort(*byte(i64(&arr[0])), 5, sizeof(int), cmp_int)
         |    arr[0] * 10000 + arr[1] * 1000 + arr[2] * 100 + arr[3] * 10 + arr[4]
         |""".stripMargin)) shouldBe 12345
  }

  "qsort sorts descending with reversed comparator" in {
    compileMultiAndRun(sortSources(
      """import posix.stdlib.*
        |
        |cmp_int_desc(a: *byte, b: *byte) -> int
        |    val x = *(*int(i64(a)))
        |    val y = *(*int(i64(b)))
        |    if x > y then -1
        |    elif x < y then 1
        |    else 0
        |
        |main() -> int
        |    var arr: [5]int
        |    arr[0] = 5
        |    arr[1] = 3
        |    arr[2] = 1
        |    arr[3] = 4
        |    arr[4] = 2
        |    qsort(*byte(i64(&arr[0])), 5, sizeof(int), cmp_int_desc)
        |    arr[0] * 10000 + arr[1] * 1000 + arr[2] * 100 + arr[3] * 10 + arr[4]
        |""".stripMargin)) shouldBe 54321
  }

  "qsort already sorted" in {
    compileMultiAndRun(sortSources(
      s"""import posix.stdlib.*
         |
         |$cmpInt
         |main() -> int
         |    var arr: [4]int
         |    arr[0] = 1
         |    arr[1] = 2
         |    arr[2] = 3
         |    arr[3] = 4
         |    qsort(*byte(i64(&arr[0])), 4, sizeof(int), cmp_int)
         |    arr[0] * 1000 + arr[1] * 100 + arr[2] * 10 + arr[3]
         |""".stripMargin)) shouldBe 1234
  }

  "qsort single element" in {
    compileMultiAndRun(sortSources(
      s"""import posix.stdlib.*
         |
         |$cmpInt
         |main() -> int
         |    var arr: [1]int
         |    arr[0] = 42
         |    qsort(*byte(i64(&arr[0])), 1, sizeof(int), cmp_int)
         |    arr[0]
         |""".stripMargin)) shouldBe 42
  }

  "qsort empty array" in {
    compileMultiAndRun(sortSources(
      s"""import posix.stdlib.*
         |
         |$cmpInt
         |main() -> int
         |    var arr: [1]int
         |    arr[0] = 99
         |    qsort(*byte(i64(&arr[0])), 0, sizeof(int), cmp_int)
         |    arr[0]
         |""".stripMargin)) shouldBe 99
  }

  "qsort with duplicates" in {
    compileMultiAndRun(sortSources(
      s"""import posix.stdlib.*
         |
         |$cmpInt
         |main() -> int
         |    var arr: [6]int
         |    arr[0] = 3
         |    arr[1] = 1
         |    arr[2] = 3
         |    arr[3] = 2
         |    arr[4] = 1
         |    arr[5] = 2
         |    qsort(*byte(i64(&arr[0])), 6, sizeof(int), cmp_int)
         |    arr[0] * 100000 + arr[1] * 10000 + arr[2] * 1000 + arr[3] * 100 + arr[4] * 10 + arr[5]
         |""".stripMargin)) shouldBe 112233
  }

  "qsort reverse sorted" in {
    compileMultiAndRun(sortSources(
      s"""import posix.stdlib.*
         |
         |$cmpInt
         |main() -> int
         |    var arr: [5]int
         |    arr[0] = 5
         |    arr[1] = 4
         |    arr[2] = 3
         |    arr[3] = 2
         |    arr[4] = 1
         |    qsort(*byte(i64(&arr[0])), 5, sizeof(int), cmp_int)
         |    arr[0] * 10000 + arr[1] * 1000 + arr[2] * 100 + arr[3] * 10 + arr[4]
         |""".stripMargin)) shouldBe 12345
  }

  "qsort two elements" in {
    compileMultiAndRun(sortSources(
      s"""import posix.stdlib.*
         |
         |$cmpInt
         |main() -> int
         |    var arr: [2]int
         |    arr[0] = 20
         |    arr[1] = 10
         |    qsort(*byte(i64(&arr[0])), 2, sizeof(int), cmp_int)
         |    arr[0] * 100 + arr[1]
         |""".stripMargin)) shouldBe 1020
  }

  // ===== bsearch =====

  "bsearch finds element" in {
    compileMultiAndRun(sortSources(
      s"""import posix.stdlib.*
         |
         |$cmpInt
         |main() -> int
         |    var arr: [5]int
         |    arr[0] = 10
         |    arr[1] = 20
         |    arr[2] = 30
         |    arr[3] = 40
         |    arr[4] = 50
         |    var key = 30
         |    val result = bsearch(*byte(i64(&key)), *byte(i64(&arr[0])), 5, sizeof(int), cmp_int)
         |    if result != *byte(0) then *(*int(i64(result)))
         |    else -1
         |""".stripMargin)) shouldBe 30
  }

  "bsearch finds first element" in {
    compileMultiAndRun(sortSources(
      s"""import posix.stdlib.*
         |
         |$cmpInt
         |main() -> int
         |    var arr: [3]int
         |    arr[0] = 10
         |    arr[1] = 20
         |    arr[2] = 30
         |    var key = 10
         |    val result = bsearch(*byte(i64(&key)), *byte(i64(&arr[0])), 3, sizeof(int), cmp_int)
         |    if result != *byte(0) then *(*int(i64(result)))
         |    else -1
         |""".stripMargin)) shouldBe 10
  }

  "bsearch finds last element" in {
    compileMultiAndRun(sortSources(
      s"""import posix.stdlib.*
         |
         |$cmpInt
         |main() -> int
         |    var arr: [3]int
         |    arr[0] = 10
         |    arr[1] = 20
         |    arr[2] = 30
         |    var key = 30
         |    val result = bsearch(*byte(i64(&key)), *byte(i64(&arr[0])), 3, sizeof(int), cmp_int)
         |    if result != *byte(0) then *(*int(i64(result)))
         |    else -1
         |""".stripMargin)) shouldBe 30
  }

  "bsearch returns null when not found" in {
    compileMultiAndRun(sortSources(
      s"""import posix.stdlib.*
         |
         |$cmpInt
         |main() -> int
         |    var arr: [3]int
         |    arr[0] = 10
         |    arr[1] = 20
         |    arr[2] = 30
         |    var key = 25
         |    val result = bsearch(*byte(i64(&key)), *byte(i64(&arr[0])), 3, sizeof(int), cmp_int)
         |    if result == *byte(0) then 1
         |    else 0
         |""".stripMargin)) shouldBe 1
  }

  "bsearch empty array returns null" in {
    compileMultiAndRun(sortSources(
      s"""import posix.stdlib.*
         |
         |$cmpInt
         |main() -> int
         |    var arr: [1]int
         |    arr[0] = 10
         |    var key = 10
         |    val result = bsearch(*byte(i64(&key)), *byte(i64(&arr[0])), 0, sizeof(int), cmp_int)
         |    if result == *byte(0) then 1
         |    else 0
         |""".stripMargin)) shouldBe 1
  }
}
