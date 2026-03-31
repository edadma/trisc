package io.github.edadma.trisc

class TFSHelperTests extends TFSTestHelpers {

  val prefill: String =
    """/dev/tty0 char 0 0
      |/etc/motd file "Hello TOS"
      |""".stripMargin

  // ===== Constants =====

  "INODE_SIZE is 32" in {
    val (_, output) = runTFS(
      """import "tfs"
        |main() -> int
        |    if INODE_SIZE == 32
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  "DIR_ENTRY_SIZE is 16" in {
    val (_, output) = runTFS(
      """import "tfs"
        |main() -> int
        |    if DIR_ENTRY_SIZE == 16
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  "NUM_DIRECT is 6" in {
    val (_, output) = runTFS(
      """import "tfs"
        |main() -> int
        |    if NUM_DIRECT == 6
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  // ===== read_i16 / read_i32 / write_i16 / write_i32 =====

  "read_i16 big-endian" in {
    val (_, output) = runTFS(
      """import "tfs"
        |main() -> int
        |    var buf: [4]i8
        |    buf[0] = 0x01
        |    buf[1] = 0x02
        |    val v = read_i16(&buf)
        |    if v == 0x0102
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  "read_i32 big-endian" in {
    val (_, output) = runTFS(
      """import "tfs"
        |main() -> int
        |    var buf: [4]i8
        |    buf[0] = 0x54
        |    buf[1] = 0x46
        |    buf[2] = 0x53
        |    buf[3] = 0x00
        |    val v = read_i32(&buf)
        |    if v == 0x54465300
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  "write_i16 then read_i16 round-trip" in {
    val (_, output) = runTFS(
      """import "tfs"
        |main() -> int
        |    var buf: [4]i8
        |    write_i16(&buf, 0x1234)
        |    val v = read_i16(&buf)
        |    if v == 0x1234
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  "write_i32 then read_i32 round-trip" in {
    val (_, output) = runTFS(
      """import "tfs"
        |main() -> int
        |    var buf: [4]i8
        |    write_i32(&buf, 0x12345678)
        |    val v = read_i32(&buf)
        |    if v == 0x12345678
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  // ===== streq_n =====

  "streq_n matching strings" in {
    val (_, output) = runTFS(
      s"""import "tfs"
         |main() -> int
         |${syslBytes("a", "hello")}
         |${syslBytes("b", "hello")}
         |    if streq_n(&a, &b, 14) == 1
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  "streq_n different strings" in {
    val (_, output) = runTFS(
      s"""import "tfs"
         |main() -> int
         |${syslBytes("a", "hello")}
         |${syslBytes("b", "world")}
         |    if streq_n(&a, &b, 14) == 0
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  // ===== strlen =====

  "strlen empty" in {
    val (_, output) = runTFS(
      s"""import "tfs"
         |main() -> int
         |${syslBytes("s", "")}
         |    val n = strlen(&s[0])
         |    if n == 0
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  "strlen nonempty" in {
    val (_, output) = runTFS(
      s"""import "tfs"
         |main() -> int
         |${syslBytes("s", "hello")}
         |    val n = strlen(&s[0])
         |    if n == 5
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  // ===== tfs_init =====

  "tfs_init populates all superblock fields" in {
    val (_, output) = runTFS(
      """import "tfs"
        |main() -> int
        |    tfs_init()
        |    if sb_block_size == 512
        |        putchar(65)
        |    if sb_total_inodes > 0
        |        putchar(66)
        |    if sb_inode_table > 0
        |        putchar(67)
        |    if sb_first_data > 0
        |        putchar(68)
        |    if sb_free_blocks > 0
        |        putchar(69)
        |    if sb_free_inodes > 0
        |        putchar(70)
        |    0
        |""".stripMargin, prefill = prefill)
    output shouldBe "ABCDEF"
  }
}
