package io.github.edadma.trisc

class TFSInodeTests extends TFSTestHelpers {

  val prefill: String =
    """/dev/tty0 char 0 0
      |/etc/motd file "Hello TOS"
      |""".stripMargin

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

  "ino_uid / ino_gid on buffer" in {
    val (_, output) = runTFS(
      """import "tfs"
        |main() -> int
        |    var ibuf: [32]i8
        |    val p: *i8 = &ibuf[0]
        |    var i = 0
        |    while i < 32
        |        p[i] = 0
        |        i += 1
        |    p[3] = 7
        |    p[4] = 12
        |    if ino_uid(&ibuf[0]) == 7
        |        putchar(85)
        |    if ino_gid(&ibuf[0]) == 12
        |        putchar(71)
        |    0
        |""".stripMargin, prefill = prefill)
    output shouldBe "UG"
  }

  "ino_mtime / ino_ctime from disk" in {
    val (_, output) = runTFS(
      """import "tfs"
        |main() -> int
        |    tfs_init()
        |    var ibuf: [32]i8
        |    tfs_read_inode(1, &ibuf[0])
        |    // mtime and ctime should be 0 (from format with now=0)
        |    if ino_mtime(&ibuf[0]) == 0
        |        putchar(77)
        |    if ino_ctime(&ibuf[0]) == 0
        |        putchar(67)
        |    0
        |""".stripMargin, prefill = prefill)
    output shouldBe "MC"
  }

  "ino_set_mode / ino_mode round-trip" in {
    val (_, output) = runTFS(
      """import "tfs"
        |main() -> int
        |    var ibuf: [32]i8
        |    val p: *i8 = &ibuf[0]
        |    var i = 0
        |    while i < 32
        |        p[i] = 0
        |        i += 1
        |    ino_set_mode(&ibuf[0], 0x21ED)
        |    if ino_mode(&ibuf[0]) == 0x21ED
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  "ino_set_indirect / ino_indirect round-trip" in {
    val (_, output) = runTFS(
      """import "tfs"
        |main() -> int
        |    var ibuf: [32]i8
        |    val p: *i8 = &ibuf[0]
        |    var i = 0
        |    while i < 32
        |        p[i] = 0
        |        i += 1
        |    ino_set_indirect(&ibuf[0], 55)
        |    if ino_indirect(&ibuf[0]) == 55
        |        putchar(89)
        |    if ino_indirect(&ibuf[0]) != 0
        |        putchar(78)
        |    0
        |""".stripMargin, prefill = prefill)
    output shouldBe "YN"
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

  // ===== get_file_block =====

  "get_file_block returns direct pointer" in {
    val (_, output) = runTFS(
      """import "tfs"
        |main() -> int
        |    tfs_init()
        |    var ibuf: [32]i8
        |    tfs_read_inode(1, &ibuf[0])
        |    val blk = get_file_block(&ibuf[0], 0)
        |    if blk > 0
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  "get_file_block returns 0 for unallocated" in {
    val (_, output) = runTFS(
      """import "tfs"
        |main() -> int
        |    var ibuf: [32]i8
        |    val p: *i8 = &ibuf[0]
        |    var i = 0
        |    while i < 32
        |        p[i] = 0
        |        i += 1
        |    val blk = get_file_block(&ibuf[0], 0)
        |    if blk == 0
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }
}
