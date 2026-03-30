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
