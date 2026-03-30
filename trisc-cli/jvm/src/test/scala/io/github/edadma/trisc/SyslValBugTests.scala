package io.github.edadma.trisc

class SyslValBugTests extends TFSTestHelpers {

  // Test whether module-level val constants work correctly in loop bounds
  // across module boundaries

  "module val in loop bound (same module)" in {
    val (_, output) = runTFS(
      """val LIMIT = 5
        |
        |copy(src: *i8, dst: *i8)
        |    var i = 0
        |    while i < LIMIT
        |        dst[i] = src[i]
        |        i += 1
        |
        |main() -> int
        |    var a: [8]i8
        |    var b: [8]i8
        |    a[0] = 72
        |    a[1] = 101
        |    a[2] = 108
        |    a[3] = 108
        |    a[4] = 111
        |    copy(&a, &b)
        |    var i = 0
        |    val p: *i8 = &b
        |    while i < 5
        |        putchar(p[i])
        |        i += 1
        |    0
        |""".stripMargin, prefill = "")
    output shouldBe "Hello"
  }

  "module val in loop bound (cross-module)" in {
    val (_, output) = runTFS(Map(
      "lib" ->
        """val LIMIT = 5
          |
          |copy(src: *i8, dst: *i8)
          |    var i = 0
          |    while i < LIMIT
          |        dst[i] = src[i]
          |        i += 1
          |""".stripMargin,
      "main" ->
        """import "lib"
          |
          |main() -> int
          |    var a: [8]i8
          |    var b: [8]i8
          |    a[0] = 72
          |    a[1] = 101
          |    a[2] = 108
          |    a[3] = 108
          |    a[4] = 111
          |    copy(&a, &b)
          |    var i = 0
          |    val p: *i8 = &b
          |    while i < 5
          |        putchar(p[i])
          |        i += 1
          |    0
          |""".stripMargin,
    ), prefill = "", maxCycles = 100000)
    output shouldBe "Hello"
  }

  "module val = 32 used in loop bound (cross-module)" in {
    val (_, output) = runTFS(Map(
      "lib" ->
        """val SZ = 32
          |
          |mycopy(src: int, dst: int)
          |    val s: *i8 = src
          |    val d: *i8 = dst
          |    var i = 0
          |    while i < SZ
          |        d[i] = s[i]
          |        i += 1
          |""".stripMargin,
      "main" ->
        """import "lib"
          |
          |main() -> int
          |    var a: [32]i8
          |    var b: [32]i8
          |    a[0] = 65
          |    a[6] = 66
          |    a[31] = 67
          |    mycopy(&a, &b)
          |    val p: *i8 = &b
          |    putchar(p[0])
          |    putchar(p[6])
          |    putchar(p[31])
          |    0
          |""".stripMargin,
    ), prefill = "", maxCycles = 100000)
    output shouldBe "ABC"
  }

  "tfs_read_inode copies all 32 bytes" in {
    val (_, output) = runTFS(
      """import "tfs"
        |
        |main() -> int
        |    tfs_init()
        |    var ibuf: [32]i8
        |    tfs_read_inode(1, &ibuf)
        |    // Check byte-by-byte: are any bytes nonzero?
        |    val p: *i8 = &ibuf
        |    var nonzero = 0
        |    var i = 0
        |    while i < 32
        |        if p[i] != 0
        |            nonzero += 1
        |        i += 1
        |    if nonzero > 0
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin, prefill = "/dev/tty0 char 0 0")
    output shouldBe "Y"
  }

  "inode_block and inode_off for root inode" in {
    val (_, output) = runTFS(
      """import "tfs"
        |
        |main() -> int
        |    tfs_init()
        |    val blk = inode_block(1)
        |    val off = inode_off(1)
        |    // With 512-byte blocks, 16 inodes/block, inode 1 is in first inode block at offset 32
        |    // sb_inode_table should be around 4 (boot + super + inode_bm + block_bm)
        |    if blk == sb_inode_table
        |        putchar(66)
        |    else
        |        putchar(78)
        |    if off == 32
        |        putchar(79)
        |    else
        |        putchar(78)
        |    // Print raw values for debugging
        |    putchar(48 + blk)
        |    putchar(48 + off / 10)
        |    putchar(48 + off % 10)
        |    0
        |""".stripMargin, prefill = "/dev/tty0 char 0 0")
    output shouldBe "BO432"
  }

  "rd_read inode block has nonzero data" in {
    val (_, output) = runTFS(
      """import "tfs"
        |
        |main() -> int
        |    tfs_init()
        |    val blk = inode_block(1)
        |    rd_read(blk, &blkbuf)
        |    val off = inode_off(1)
        |    val p: *i8 = &blkbuf + off
        |    // Check first 2 bytes (mode)
        |    val b0 = p[0] & 0xFF
        |    val b1 = p[1] & 0xFF
        |    if b0 != 0
        |        putchar(65)
        |    if b1 != 0
        |        putchar(66)
        |    0
        |""".stripMargin, prefill = "/dev/tty0 char 0 0")
    output shouldBe "AB"
  }

  "rd_read block 4 with literal" in {
    val (_, output) = runTFS(
      """import "tfs"
        |
        |main() -> int
        |    tfs_init()
        |    rd_read(4, &blkbuf)
        |    val p: *i8 = &blkbuf + 32
        |    val b0 = p[0] & 0xFF
        |    if b0 != 0
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin, prefill = "/dev/tty0 char 0 0", maxCycles = 5000)
    output shouldBe "Y"
  }

  "tfs_create and tfs_write round-trip" in {
    val (_, output) = runTFS(
      s"""import "tfs"
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "hello")}
         |    val ino = tfs_create(1, &name[0], 1, 0x1A4)
         |    if ino == -1
         |        putchar(69)
         |        return 1
         |    var data: [6]i8
         |    data[0] = 72
         |    data[1] = 101
         |    data[2] = 108
         |    data[3] = 108
         |    data[4] = 111
         |    tfs_write(ino, &data[0], 0, 5)
         |    var buf: [32]i8
         |    val n = tfs_read(ino, &buf[0], 0, 5)
         |    val bp: *i8 = &buf[0]
         |    var i = 0
         |    while i < n
         |        putchar(bp[i])
         |        i += 1
         |    0
         |""".stripMargin,
      prefill = "/dev/tty0 char 0 0",
      maxCycles = 50000,
    )
    output shouldBe "Hello"
  }

  "tfs_create minimal with trace" in {
    val (cpu, output) = runTFS(
      s"""import "tfs"
         |
         |main() -> int
         |    tfs_init()
         |    putchar(49)
         |${syslBytes("name", "x")}
         |    putchar(50)
         |    val ino = tfs_create(1, &name[0], 1, 0x1A4)
         |    putchar(51)
         |    if ino > 0
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin,
      prefill = "/dev/tty0 char 0 0",
      maxCycles = 5000,
    )
    output shouldBe "123Y"
  }

  "sb_inode_table value" in {
    val (_, output) = runTFS(
      """import "tfs"
        |
        |main() -> int
        |    tfs_init()
        |    // Print sb_inode_table
        |    putchar(48 + sb_inode_table)
        |    // Print sb_block_size
        |    putchar(48 + sb_block_size / 100)
        |    0
        |""".stripMargin, prefill = "/dev/tty0 char 0 0")
    // inode_table should be 4, block_size/100 = 5 (512)
    output shouldBe "45"
  }

  "rd_read block 4 then add offset crashes" in {
    val (_, output) = runTFS(
      """import "tfs"
        |
        |main() -> int
        |    tfs_init()
        |    val blk = 4
        |    rd_read(blk, &blkbuf)
        |    putchar(65)
        |    val off = 32
        |    putchar(66)
        |    val p: *i8 = &blkbuf + off
        |    putchar(67)
        |    val b0 = p[0] & 0xFF
        |    putchar(68)
        |    0
        |""".stripMargin, prefill = "/dev/tty0 char 0 0")
    output shouldBe "ABCD"
  }

  "tfs_read_inode first 2 bytes are mode" in {
    val (_, output) = runTFS(
      """import "tfs"
        |
        |main() -> int
        |    tfs_init()
        |    var ibuf: [32]i8
        |    tfs_read_inode(1, &ibuf)
        |    // Read mode directly as bytes
        |    val p: *i8 = &ibuf
        |    val b0 = p[0] & 0xFF
        |    val b1 = p[1] & 0xFF
        |    val mode = (b0 << 8) | b1
        |    // Root inode: type=dir(2), perm=0x1ED -> mode = 0x21ED
        |    if (mode >> 12) == 2
        |        putchar(89)
        |    else
        |        putchar(78)
        |        putchar(48 + (b0 >> 4))
        |        putchar(48 + (b0 & 0xF))
        |        putchar(48 + (b1 >> 4))
        |        putchar(48 + (b1 & 0xF))
        |    0
        |""".stripMargin, prefill = "/dev/tty0 char 0 0")
    output shouldBe "Y"
  }
}
