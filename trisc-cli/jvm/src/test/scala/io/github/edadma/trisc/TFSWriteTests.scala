package io.github.edadma.trisc

class TFSWriteTests extends TFSTestHelpers {

  "tfs_create file only" in {
    val (_, output) = runTFS(
      s"""import "tfs"
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "x")}
         |    val ino = tfs_create(1, &name[0], 1, 0x1A4)
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

  "alloc_block after create" in {
    val (cpu, output) = runTFS(
      s"""import "tfs"
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "f")}
         |    val ino = tfs_create(1, &name[0], 1, 0x1A4)
         |    putchar(49)
         |    val blk = alloc_block()
         |    putchar(50)
         |    if blk > 0
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin,
      prefill = "/dev/tty0 char 0 0",
      maxCycles = 500000,
    )
    output shouldBe "12Y"
  }

  "tfs_write to existing file (has block)" in {
    val (cpu, output) = runTFS(
      s"""import "tfs"
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("path", "/etc/motd")}
         |    val ino = tfs_lookup(&path[0])
         |    putchar(49)
         |    var data: [4]i8
         |    data[0] = 88
         |    data[1] = 89
         |    tfs_write(ino, &data[0], 0, 2)
         |    putchar(50)
         |    // Read back
         |    var buf: [16]i8
         |    val n = tfs_read(ino, &buf[0], 0, 9)
         |    val bp: *i8 = &buf[0]
         |    var i = 0
         |    while i < n
         |        putchar(bp[i])
         |        i += 1
         |    0
         |""".stripMargin,
      prefill = "/dev/tty0 char 0 0\n/etc/motd file \"Hello TOS\"",
      maxCycles = 500000,
    )
    // Overwrite first 2 bytes: "XYllo TOS"
    output shouldBe "12XYllo TOS"
  }

  "tfs_write to new file - debug" in {
    val (cpu, output) = runTFS(
      s"""import "tfs"
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "f")}
         |    val ino = tfs_create(1, &name[0], 1, 0x1A4)
         |    putchar(49)
         |    // Check sb_block_size before write
         |    if sb_block_size == 512
         |        putchar(83)
         |    // Manual: alloc a block, set direct[0], write data
         |    var ibuf: [32]i8
         |    tfs_read_inode(ino, &ibuf[0])
         |    putchar(50)
         |    val blk = alloc_block()
         |    putchar(51)
         |    ino_set_direct(&ibuf[0], 0, blk)
         |    ino_set_size(&ibuf[0], 2)
         |    tfs_write_inode(ino, &ibuf[0])
         |    putchar(52)
         |    // Write data to block
         |    rd_read(blk, &blkbuf[0])
         |    val dp: *i8 = &blkbuf[0]
         |    dp[0] = 72
         |    dp[1] = 105
         |    rd_write(blk, &blkbuf[0])
         |    putchar(53)
         |    // Read back
         |    var buf: [8]i8
         |    tfs_read(ino, &buf[0], 0, 2)
         |    val bp: *i8 = &buf[0]
         |    putchar(bp[0])
         |    putchar(bp[1])
         |    0
         |""".stripMargin,
      prefill = "/dev/tty0 char 0 0",
      maxCycles = 500000,
    )
    output shouldBe "1S2345Hi"
  }
}
