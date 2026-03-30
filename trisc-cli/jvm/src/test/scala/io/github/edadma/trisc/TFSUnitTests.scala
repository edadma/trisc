package io.github.edadma.trisc

class TFSUnitTests extends TFSTestHelpers {

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

  // ===== Inode field accessors =====

  "ino_mode / ino_type / ino_perm on zeroed buffer" in {
    val (_, output) = runTFS(
      """import "tfs"
        |main() -> int
        |    var ibuf: [32]i8
        |    var i = 0
        |    val p: *i8 = &ibuf
        |    while i < 32
        |        p[i] = 0
        |        i += 1
        |    write_i16(&ibuf, 0x21ED)
        |    if ino_type(&ibuf) == 2
        |        putchar(84)
        |    if ino_perm(&ibuf) == 0x1ED
        |        putchar(80)
        |    if ino_mode(&ibuf) == 0x21ED
        |        putchar(77)
        |    0
        |""".stripMargin, prefill = prefill)
    output shouldBe "TPM"
  }

  "ino_set_size / ino_size round-trip" in {
    val (_, output) = runTFS(
      """import "tfs"
        |main() -> int
        |    var ibuf: [32]i8
        |    var i = 0
        |    val p: *i8 = &ibuf
        |    while i < 32
        |        p[i] = 0
        |        i += 1
        |    ino_set_size(&ibuf, 80)
        |    val sz = ino_size(&ibuf)
        |    if sz == 80
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  "ino_set_nlinks / ino_nlinks round-trip" in {
    val (_, output) = runTFS(
      """import "tfs"
        |main() -> int
        |    var ibuf: [32]i8
        |    var i = 0
        |    val p: *i8 = &ibuf
        |    while i < 32
        |        p[i] = 0
        |        i += 1
        |    ino_set_nlinks(&ibuf, 3)
        |    if ino_nlinks(&ibuf) == 3
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  "ino_set_direct / ino_direct round-trip" in {
    val (_, output) = runTFS(
      """import "tfs"
        |main() -> int
        |    var ibuf: [32]i8
        |    var i = 0
        |    val p: *i8 = &ibuf
        |    while i < 32
        |        p[i] = 0
        |        i += 1
        |    ino_set_direct(&ibuf, 0, 42)
        |    ino_set_direct(&ibuf, 5, 99)
        |    if ino_direct(&ibuf, 0) == 42
        |        putchar(65)
        |    if ino_direct(&ibuf, 5) == 99
        |        putchar(66)
        |    if ino_direct(&ibuf, 1) == 0
        |        putchar(67)
        |    0
        |""".stripMargin, prefill = prefill)
    output shouldBe "ABC"
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

  // ===== tfs_read_inode =====

  "tfs_read_inode reads root inode correctly" in {
    val (_, output) = runTFS(
      """import "tfs"
        |main() -> int
        |    tfs_init()
        |    var ibuf: [32]i8
        |    tfs_read_inode(1, &ibuf)
        |    // Root should be a directory
        |    if ino_type(&ibuf) == 2
        |        putchar(84)
        |    // Root nlinks >= 2 (. and ..)
        |    if ino_nlinks(&ibuf) >= 2
        |        putchar(76)
        |    // Root size > 0
        |    if ino_size(&ibuf) > 0
        |        putchar(83)
        |    // Root direct[0] > 0 (has data block)
        |    if ino_direct(&ibuf, 0) > 0
        |        putchar(68)
        |    0
        |""".stripMargin, prefill = prefill)
    output shouldBe "TLSD"
  }

  "tfs_read_inode reads device node" in {
    val (_, output) = runTFS(
      """import "tfs"
        |main() -> int
        |    tfs_init()
        |    // inode 3 should be tty0 (root=1, dev=2, tty0=3)
        |    var ibuf: [32]i8
        |    tfs_read_inode(3, &ibuf)
        |    if ino_type(&ibuf) == 3
        |        putchar(67)
        |    if ino_nlinks(&ibuf) == 1
        |        putchar(76)
        |    // major=0 minor=0 stored in direct[0]
        |    if ino_direct(&ibuf, 0) == 0
        |        putchar(68)
        |    0
        |""".stripMargin, prefill = prefill)
    output shouldBe "CLD"
  }

  // ===== tfs_write_inode round-trip =====

  "tfs_write_inode preserves data" in {
    val (_, output) = runTFS(
      """import "tfs"
        |main() -> int
        |    tfs_init()
        |    var ibuf: [32]i8
        |    tfs_read_inode(1, &ibuf)
        |    val sz1 = ino_size(&ibuf)
        |    tfs_write_inode(1, &ibuf)
        |    // Re-read and verify
        |    var ibuf2: [32]i8
        |    tfs_read_inode(1, &ibuf2)
        |    val sz2 = ino_size(&ibuf2)
        |    if sz1 == sz2
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  // ===== bitmap_test =====

  "bitmap_test returns 1 for allocated inode" in {
    val (_, output) = runTFS(
      """import "tfs"
        |main() -> int
        |    tfs_init()
        |    // Inode 0 (reserved) and 1 (root) should be allocated
        |    if bitmap_test(sb_inode_bitmap, 0) == 1
        |        putchar(65)
        |    if bitmap_test(sb_inode_bitmap, 1) == 1
        |        putchar(66)
        |    0
        |""".stripMargin, prefill = prefill)
    output shouldBe "AB"
  }

  "bitmap_test returns 0 for free inode" in {
    val (_, output) = runTFS(
      """import "tfs"
        |main() -> int
        |    tfs_init()
        |    // High inode numbers should be free
        |    if bitmap_test(sb_inode_bitmap, 100) == 0
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  // ===== bitmap_set / bitmap_clear =====

  "bitmap_set then test" in {
    val (_, output) = runTFS(
      """import "tfs"
        |main() -> int
        |    tfs_init()
        |    // Verify bit 50 is free, set it, verify it's set
        |    if bitmap_test(sb_inode_bitmap, 50) == 0
        |        putchar(65)
        |    bitmap_set(sb_inode_bitmap, 50)
        |    if bitmap_test(sb_inode_bitmap, 50) == 1
        |        putchar(66)
        |    0
        |""".stripMargin, prefill = prefill)
    output shouldBe "AB"
  }

  "bitmap_clear then test" in {
    val (_, output) = runTFS(
      """import "tfs"
        |main() -> int
        |    tfs_init()
        |    bitmap_set(sb_inode_bitmap, 50)
        |    bitmap_clear(sb_inode_bitmap, 50)
        |    if bitmap_test(sb_inode_bitmap, 50) == 0
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  // ===== alloc_inode =====

  "alloc_inode returns valid inode" in {
    val (_, output) = runTFS(
      """import "tfs"
        |main() -> int
        |    tfs_init()
        |    val ino = alloc_inode()
        |    if ino >= 2
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  "alloc_inode marks bit in bitmap" in {
    val (_, output) = runTFS(
      """import "tfs"
        |main() -> int
        |    tfs_init()
        |    val ino = alloc_inode()
        |    if bitmap_test(sb_inode_bitmap, ino) == 1
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  "alloc_inode twice returns different inodes" in {
    val (_, output) = runTFS(
      """import "tfs"
        |main() -> int
        |    tfs_init()
        |    val a = alloc_inode()
        |    val b = alloc_inode()
        |    if a != b
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  // ===== alloc_block =====

  "alloc_block returns valid block" in {
    val (_, output) = runTFS(
      """import "tfs"
        |main() -> int
        |    tfs_init()
        |    val blk = alloc_block()
        |    if blk >= sb_first_data
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

  // ===== tfs_dir_lookup =====

  "tfs_dir_lookup finds . in root" in {
    val (_, output) = runTFS(
      s"""import "tfs"
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", ".")}
         |    val ino = tfs_dir_lookup(1, &name)
         |    if ino == 1
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  "tfs_dir_lookup finds .. in root" in {
    val (_, output) = runTFS(
      s"""import "tfs"
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "..")}
         |    val ino = tfs_dir_lookup(1, &name)
         |    if ino == 1
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  "tfs_dir_lookup finds dev in root" in {
    val (_, output) = runTFS(
      s"""import "tfs"
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "dev")}
         |    val ino = tfs_dir_lookup(1, &name)
         |    if ino > 0
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  "tfs_dir_lookup returns -1 for missing name" in {
    val (_, output) = runTFS(
      s"""import "tfs"
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "nope")}
         |    val ino = tfs_dir_lookup(1, &name)
         |    if ino == -1
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  // ===== tfs_lookup =====

  "tfs_lookup root" in {
    val (_, output) = runTFS(
      s"""import "tfs"
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/")}
         |    if tfs_lookup(&p) == 1
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  "tfs_lookup /dev/tty0" in {
    val (_, output) = runTFS(
      s"""import "tfs"
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/dev/tty0")}
         |    val ino = tfs_lookup(&p)
         |    if ino > 0
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  "tfs_lookup nonexistent returns -1" in {
    val (_, output) = runTFS(
      s"""import "tfs"
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/nonexistent")}
         |    if tfs_lookup(&p) == -1
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  // ===== tfs_stat =====

  "tfs_stat on root inode" in {
    val (_, output) = runTFS(
      """import "tfs"
        |main() -> int
        |    tfs_init()
        |    var stat: [7]int
        |    tfs_stat(1, &stat)
        |    if (stat[0] >> 12) == 2
        |        putchar(68)
        |    if stat[1] >= 2
        |        putchar(76)
        |    if stat[4] > 0
        |        putchar(83)
        |    0
        |""".stripMargin, prefill = prefill)
    output shouldBe "DLS"
  }

  // ===== tfs_read =====

  "tfs_read file content" in {
    val (_, output) = runTFS(
      s"""import "tfs"
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/etc/motd")}
         |    val ino = tfs_lookup(&p)
         |    var buf: [32]i8
         |    val n = tfs_read(ino, &buf, 0, 9)
         |    val bp: *i8 = &buf
         |    var i = 0
         |    while i < n
         |        putchar(bp[i])
         |        i += 1
         |    0
         |""".stripMargin, prefill = prefill)
    output shouldBe "Hello TOS"
  }

  "tfs_read offset into file" in {
    val (_, output) = runTFS(
      s"""import "tfs"
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/etc/motd")}
         |    val ino = tfs_lookup(&p)
         |    var buf: [16]i8
         |    val n = tfs_read(ino, &buf, 6, 3)
         |    val bp: *i8 = &buf
         |    var i = 0
         |    while i < n
         |        putchar(bp[i])
         |        i += 1
         |    0
         |""".stripMargin, prefill = prefill)
    output shouldBe "TOS"
  }

  "tfs_read past EOF returns 0" in {
    val (_, output) = runTFS(
      s"""import "tfs"
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/etc/motd")}
         |    val ino = tfs_lookup(&p)
         |    var buf: [16]i8
         |    val n = tfs_read(ino, &buf, 999, 10)
         |    if n == 0
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  "tfs_read clamps to file size" in {
    val (_, output) = runTFS(
      s"""import "tfs"
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/etc/motd")}
         |    val ino = tfs_lookup(&p)
         |    var buf: [64]i8
         |    val n = tfs_read(ino, &buf, 0, 100)
         |    if n == 9
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }
}
