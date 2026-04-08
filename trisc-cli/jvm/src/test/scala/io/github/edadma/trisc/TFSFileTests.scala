package io.github.edadma.trisc

class TFSFileTests extends TFSTestHelpers {

  val prefill: String =
    """/dev/tty0 char 0 0
      |/etc/motd file "Hello TOS"
      |""".stripMargin

  // ===== tfs_stat =====

  "tfs_stat on root inode" in {
    val (_, output) = runTFS(
      """import oskit.fs.*
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
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/etc/motd")}
         |    val ino = tfs_lookup(&p, p_len)
         |    var buf: [32]byte
         |    val n = tfs_read(ino, &buf, 0, 9)
         |    val bp: *byte = &buf
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
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/etc/motd")}
         |    val ino = tfs_lookup(&p, p_len)
         |    var buf: [16]byte
         |    val n = tfs_read(ino, &buf, 6, 3)
         |    val bp: *byte = &buf
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
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/etc/motd")}
         |    val ino = tfs_lookup(&p, p_len)
         |    var buf: [16]byte
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
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/etc/motd")}
         |    val ino = tfs_lookup(&p, p_len)
         |    var buf: [64]byte
         |    val n = tfs_read(ino, &buf, 0, 100)
         |    if n == 9
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  // ===== tfs_write =====

  "tfs_write to new file allocates block" in {
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "f")}
         |    val ino = tfs_create(1, &name[0], name_len, 1, 0x1A4)
         |    var data: [4]byte
         |    data[0] = 65
         |    data[1] = 66
         |    tfs_write(ino, &data[0], 0, 2)
         |    var buf: [4]byte
         |    tfs_read(ino, &buf[0], 0, 2)
         |    val bp: *byte = &buf[0]
         |    putchar(bp[0])
         |    putchar(bp[1])
         |    0
         |""".stripMargin, prefill = prefill)
    output shouldBe "AB"
  }

  "tfs_write updates size" in {
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "f")}
         |    val ino = tfs_create(1, &name[0], name_len, 1, 0x1A4)
         |    var data: [4]byte
         |    data[0] = 65
         |    tfs_write(ino, &data[0], 0, 3)
         |    var stat: [7]int
         |    tfs_stat(ino, &stat[0])
         |    if stat[4] == 3
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  "tfs_write to existing file (has block)" in {
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("path", "/etc/motd")}
         |    val ino = tfs_lookup(&path[0], path_len)
         |    var data: [4]byte
         |    data[0] = 88
         |    data[1] = 89
         |    tfs_write(ino, &data[0], 0, 2)
         |    // Read back
         |    var buf: [16]byte
         |    val n = tfs_read(ino, &buf[0], 0, 9)
         |    val bp: *byte = &buf[0]
         |    var i = 0
         |    while i < n
         |        putchar(bp[i])
         |        i += 1
         |    0
         |""".stripMargin,
      prefill = "/dev/tty0 char 0 0\n/etc/motd file \"Hello TOS\"",
    )
    output shouldBe "XYllo TOS"
  }

  "tfs_write to pre-allocated block" in {
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "f")}
         |    val ino = tfs_create(1, &name[0], name_len, 1, 0x1A4)
         |    // Pre-allocate block so tfs_write doesn't need to
         |    var ibuf: [32]byte
         |    tfs_read_inode(ino, &ibuf[0])
         |    val blk = alloc_block()
         |    ino_set_direct(&ibuf[0], 0, blk)
         |    tfs_write_inode(ino, &ibuf[0])
         |    // Now write — block already exists
         |    var data: [4]byte
         |    data[0] = 72
         |    data[1] = 105
         |    tfs_write(ino, &data[0], 0, 2)
         |    // Read back
         |    var buf: [4]byte
         |    tfs_read(ino, &buf[0], 0, 2)
         |    val bp: *byte = &buf[0]
         |    putchar(bp[0])
         |    putchar(bp[1])
         |    0
         |""".stripMargin,
      prefill = "/dev/tty0 char 0 0",
    )
    output shouldBe "Hi"
  }

  // ===== tfs_create =====

  "tfs_create regular file" in {
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "x")}
         |    val ino = tfs_create(1, &name[0], name_len, 1, 0x1A4)
         |    if ino > 0
         |        putchar(65)
         |    var stat: [7]int
         |    tfs_stat(ino, &stat[0])
         |    if (stat[0] >> 12) == 1
         |        putchar(66)
         |    if (stat[0] & 0x1FF) == 0x1A4
         |        putchar(67)
         |    if stat[4] == 0
         |        putchar(68)
         |    0
         |""".stripMargin, prefill = prefill)
    output shouldBe "ABCD"
  }

  "tfs_create directory" in {
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "d")}
         |    val ino = tfs_create(1, &name[0], name_len, 2, 0x1ED)
         |    if ino > 0
         |        putchar(65)
         |    var stat: [7]int
         |    tfs_stat(ino, &stat[0])
         |    if (stat[0] >> 12) == 2
         |        putchar(66)
         |    if stat[1] == 2
         |        putchar(67)
         |    0
         |""".stripMargin, prefill = prefill)
    output shouldBe "ABC"
  }

  // ===== tfs_truncate =====

  "tfs_truncate no-op when new_size >= old_size" in {
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "f")}
         |    val ino = tfs_create(1, &name[0], name_len, 1, 0x1A4)
         |    var data: [4]byte
         |    data[0] = 65
         |    tfs_write(ino, &data[0], 0, 3)
         |    tfs_truncate(ino, 10)
         |    var stat: [7]int
         |    tfs_stat(ino, &stat[0])
         |    if stat[4] == 3
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  // ===== tfs_unlink =====

  "tfs_unlink frees inode and blocks" in {
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |    val fb1 = tfs_freeblocks()
         |    val fi1 = tfs_freeinodes()
         |${syslBytes("name", "tmp")}
         |    val ino = tfs_create(1, &name[0], name_len, 1, 0x1A4)
         |    var data: [4]byte
         |    data[0] = 65
         |    tfs_write(ino, &data[0], 0, 1)
         |    tfs_unlink(1, &name[0], name_len)
         |    val fb2 = tfs_freeblocks()
         |    val fi2 = tfs_freeinodes()
         |    // Blocks and inodes restored
         |    if fb2 == fb1
         |        putchar(66)
         |    if fi2 == fi1
         |        putchar(73)
         |    0
         |""".stripMargin, prefill = prefill)
    output shouldBe "BI"
  }

  "tfs_truncate shrinks file size" in {
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "f")}
         |    val ino = tfs_create(1, &name[0], name_len, 1, 0x1A4)
         |    var data: [8]byte
         |    data[0] = 65
         |    data[1] = 66
         |    data[2] = 67
         |    data[3] = 68
         |    data[4] = 69
         |    tfs_write(ino, &data[0], 0, 5)
         |    tfs_truncate(ino, 2)
         |    var stat: [7]int
         |    tfs_stat(ino, &stat[0])
         |    if stat[4] == 2
         |        putchar(83)
         |    var buf: [8]byte
         |    val n = tfs_read(ino, &buf[0], 0, 10)
         |    if n == 2
         |        putchar(78)
         |    0
         |""".stripMargin, prefill = prefill)
    output shouldBe "SN"
  }
}
