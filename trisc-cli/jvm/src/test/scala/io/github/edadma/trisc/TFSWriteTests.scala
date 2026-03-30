package io.github.edadma.trisc

class TFSWriteTests extends TFSTestHelpers {

  "tfs_write to existing file (has block)" in {
    val (_, output) = runTFS(
      s"""import "tfs"
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("path", "/etc/motd")}
         |    val ino = tfs_lookup(&path[0])
         |    var data: [4]i8
         |    data[0] = 88
         |    data[1] = 89
         |    tfs_write(ino, &data[0], 0, 2)
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
    )
    output shouldBe "XYllo TOS"
  }

  "tfs_write to pre-allocated block" in {
    val (_, output) = runTFS(
      s"""import "tfs"
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "f")}
         |    val ino = tfs_create(1, &name[0], 1, 0x1A4)
         |    // Pre-allocate block so tfs_write doesn't need to
         |    var ibuf: [32]i8
         |    tfs_read_inode(ino, &ibuf[0])
         |    val blk = alloc_block()
         |    ino_set_direct(&ibuf[0], 0, blk)
         |    tfs_write_inode(ino, &ibuf[0])
         |    // Now write — block already exists
         |    var data: [4]i8
         |    data[0] = 72
         |    data[1] = 105
         |    tfs_write(ino, &data[0], 0, 2)
         |    // Read back
         |    var buf: [4]i8
         |    tfs_read(ino, &buf[0], 0, 2)
         |    val bp: *i8 = &buf[0]
         |    putchar(bp[0])
         |    putchar(bp[1])
         |    0
         |""".stripMargin,
      prefill = "/dev/tty0 char 0 0",
    )
    output shouldBe "Hi"
  }
}
