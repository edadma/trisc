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

  "tfs_write 5 bytes" in {
    val (cpu, output) = runTFS(
      s"""import "tfs"
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "f")}
         |    val ino = tfs_create(1, &name[0], 1, 0x1A4)
         |    putchar(49)
         |    var data: [6]i8
         |    data[0] = 72
         |    data[1] = 105
         |    tfs_write(ino, &data[0], 0, 2)
         |    putchar(50)
         |    0
         |""".stripMargin,
      prefill = "/dev/tty0 char 0 0",
      maxCycles = 500000,
    )
    output shouldBe "12"
  }
}
