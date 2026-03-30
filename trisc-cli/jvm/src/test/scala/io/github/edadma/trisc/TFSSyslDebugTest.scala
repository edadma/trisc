package io.github.edadma.trisc

class TFSSyslDebugTest extends TFSTestHelpers {

  "harness basic output works" in {
    val (cpu, output) = runTFS(
      """main() -> int
        |    putchar(65)
        |    0
        |""".stripMargin,
      prefill = "/dev/tty0 char 0 0",
    )
    output shouldBe "A"
    cpu.state shouldBe State.Halt
  }

  "ramdisk register readable" in {
    val (cpu, output) = runTFS(
      """main() -> int
        |    // Read ramdisk STATUS register (should be 1 = ready)
        |    val p: *i8 = 0x100030
        |    val status = *p & 0xFF
        |    if status == 1
        |        putchar(82)
        |    else
        |        putchar(48 + status)
        |    0
        |""".stripMargin,
      prefill = "/dev/tty0 char 0 0",
    )
    output shouldBe "R"
  }

  "ramdisk DMA read superblock magic" in {
    val (cpu, output) = runTFS(
      """main() -> int
        |    // Write LBA = 1 (superblock)
        |    val lba: *i8 = 0x100032
        |    *(lba + 0) = 0
        |    *(lba + 1) = 0
        |    *(lba + 2) = 0
        |    *(lba + 3) = 1
        |    // Write ADDR = 0x80000 (a safe RAM address)
        |    val addr: *i8 = 0x100036
        |    *(addr + 0) = 0
        |    *(addr + 1) = 8
        |    *(addr + 2) = 0
        |    *(addr + 3) = 0
        |    // Write COUNT = 1
        |    val cnt: *i8 = 0x10003A
        |    *(cnt + 0) = 0
        |    *(cnt + 1) = 1
        |    // Issue read command
        |    val cmd: *i8 = 0x100031
        |    *cmd = 1
        |    // Check magic at 0x80000
        |    val m: *i8 = 0x80000
        |    val b0 = m[0] & 0xFF
        |    val b1 = m[1] & 0xFF
        |    val b2 = m[2] & 0xFF
        |    val b3 = m[3] & 0xFF
        |    // TFS magic = 0x54 0x46 0x53 0x00
        |    if b0 == 0x54
        |        putchar(84)
        |    if b1 == 0x46
        |        putchar(70)
        |    if b2 == 0x53
        |        putchar(83)
        |    if b3 == 0
        |        putchar(48)
        |    0
        |""".stripMargin,
      prefill = "/dev/tty0 char 0 0",
    )
    output shouldBe "TFS0"
  }

  "tfs rd_read function works" in {
    val (_, output) = runTFS(
      """import "tfs"
        |
        |main() -> int
        |    rd_read(1, 0x80000)
        |    val m: *i8 = 0x80000
        |    if (m[0] & 0xFF) == 0x54
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin,
      prefill = "/dev/tty0 char 0 0",
    )
    output shouldBe "Y"
  }

  "tfs blkbuf address is in RAM" in {
    val (_, output) = runTFS(
      """import "tfs"
        |
        |main() -> int
        |    val addr: int = &blkbuf
        |    if addr < 0x100000
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin,
      prefill = "/dev/tty0 char 0 0",
    )
    output shouldBe "Y"
  }

  "tfs RD_BASE has correct value" in {
    val (_, output) = runTFS(
      """import "tfs"
        |
        |main() -> int
        |    if RD_BASE == 0x100030
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin,
      prefill = "/dev/tty0 char 0 0",
    )
    output shouldBe "Y"
  }

  "tfs_lookup root with byte array" in {
    val (_, output) = runTFS(
      """import "tfs"
        |
        |main() -> int
        |    tfs_init()
        |    var path: [2]i8
        |    path[0] = 47
        |    path[1] = 0
        |    val ino = tfs_lookup(&path)
        |    if ino == -1
        |        putchar(77)
        |    else if ino == 0
        |        putchar(48)
        |    else if ino == 1
        |        putchar(49)
        |    else
        |        putchar(63)
        |    0
        |""".stripMargin,
      prefill = "/dev/tty0 char 0 0",
    )
    output shouldBe "1"
  }

  "tfs_lookup root does not enter loop" in {
    val (_, output) = runTFS(
      """import "tfs"
        |
        |main() -> int
        |    val path = "/"
        |    val p: *i8 = path
        |    putchar(48 + (p[0] & 0xFF) / 10)
        |    putchar(48 + (p[1] & 0xFF))
        |    // p[0] should be 47, p[1] should be 0
        |    0
        |""".stripMargin,
      prefill = "/dev/tty0 char 0 0",
    )
    // 47 = '4','7' and 0 = '0'
    output shouldBe "40"
  }

  "tfs_write_inode works" in {
    val (_, output) = runTFS(
      """import "tfs"
        |
        |main() -> int
        |    tfs_init()
        |    var ibuf: [32]i8
        |    tfs_read_inode(1, &ibuf)
        |    tfs_write_inode(1, &ibuf)
        |    putchar(89)
        |    0
        |""".stripMargin,
      prefill = "/dev/tty0 char 0 0",
    )
    output shouldBe "Y"
  }

  "tfs_read_inode + ino_size" in {
    val (_, output) = runTFS(
      """import "tfs"
        |
        |main() -> int
        |    tfs_init()
        |    var ibuf: [32]i8
        |    tfs_read_inode(1, &ibuf)
        |    val sz = ino_size(&ibuf)
        |    // root dir size should be 80 (5 entries * 16 bytes)
        |    if sz == 80
        |        putchar(89)
        |    else
        |        putchar(78)
        |        // print raw size
        |        putchar(48 + (sz / 100) % 10)
        |        putchar(48 + (sz / 10) % 10)
        |        putchar(48 + sz % 10)
        |    0
        |""".stripMargin,
      prefill = "/dev/tty0 char 0 0",
    )
    output shouldBe "Y"
  }

  "dir_add_entry reads dir correctly" in {
    val (_, output) = runTFS(
      s"""import "tfs"
         |
         |main() -> int
         |    tfs_init()
         |    // Manually do what dir_add_entry does, step by step
         |    var ibuf: [32]i8
         |    tfs_read_inode(1, &ibuf)
         |    val data_blk = ino_direct(&ibuf, 0)
         |    val entries = ino_size(&ibuf) / 16
         |    // Print entries count
         |    putchar(48 + entries)
         |    // Read dir data
         |    rd_read(data_blk, &dirbuf)
         |    // Check first entry is . (inode 1)
         |    val e0 = read_i16(&dirbuf)
         |    if e0 == 1
         |        putchar(68)
         |    putchar(10)
         |    0
         |""".stripMargin,
      prefill = "/dev/tty0 char 0 0",
    )
    // 5 entries (., .., dev, etc, tmp) + 'D' for dot entry
    output shouldBe "5D\n"
  }

  "tfs_create file only" in {
    val (_, output) = runTFS(
      s"""import "tfs"
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "test")}
         |    val ino = tfs_create(1, &name, 1, 0x1A4)
         |    if ino > 0
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin,
      prefill = "/dev/tty0 char 0 0",
    )
    output shouldBe "Y"
  }

  "alloc_inode works" in {
    val (_, output) = runTFS(
      """import "tfs"
        |
        |main() -> int
        |    tfs_init()
        |    val ino = alloc_inode()
        |    if ino > 0
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin,
      prefill = "/dev/tty0 char 0 0",
    )
    output shouldBe "Y"
  }

  "tfs rd_read into blkbuf works" in {
    val (_, output) = runTFS(
      """import "tfs"
        |
        |main() -> int
        |    val buf: int = &blkbuf
        |    rd_read(1, buf)
        |    val m: *i8 = buf
        |    val b0 = m[0] & 0xFF
        |    if b0 == 0x54
        |        putchar(89)
        |    else
        |        putchar(48 + (b0 / 100) % 10)
        |        putchar(48 + (b0 / 10) % 10)
        |        putchar(48 + b0 % 10)
        |    0
        |""".stripMargin,
      prefill = "/dev/tty0 char 0 0",
    )
    output shouldBe "Y"
  }
}
