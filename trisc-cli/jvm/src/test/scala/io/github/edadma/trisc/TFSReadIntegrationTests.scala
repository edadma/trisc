package io.github.edadma.trisc

class TFSReadIntegrationTests extends TFSTestHelpers {

  val defaultPrefill: String =
    """/dev/tty0 char 0 0
      |/dev/null char 0 1
      |/etc/motd file "Hello TOS"
      |/tmp dir
      |""".stripMargin

  // ===== Init =====

  "tfs_init reads superblock" in {
    val (cpu, output) = runTFS(
      """import tfs.*
        |
        |main() -> int
        |    tfs_init()
        |    if sb_block_size == 512
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin,
      prefill = defaultPrefill,
    )
    output shouldBe "Y"
    cpu.state shouldBe State.Halt
  }

  // ===== Lookup =====

  "tfs_lookup finds root" in {
    val (_, output) = runTFS(
      s"""import tfs.*
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("path", "/")}
         |    val ino = tfs_lookup(&path)
         |    if ino == 1
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin,
      prefill = defaultPrefill,
    )
    output shouldBe "Y"
  }

  "tfs_lookup finds /dev" in {
    val (_, output) = runTFS(
      s"""import tfs.*
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("path", "/dev")}
         |    val ino = tfs_lookup(&path)
         |    if ino > 0
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin,
      prefill = defaultPrefill,
    )
    output shouldBe "Y"
  }

  "tfs_lookup finds /dev/tty0" in {
    val (_, output) = runTFS(
      s"""import tfs.*
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("path", "/dev/tty0")}
         |    val ino = tfs_lookup(&path)
         |    if ino > 0
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin,
      prefill = defaultPrefill,
    )
    output shouldBe "Y"
  }

  "tfs_lookup finds /etc/motd" in {
    val (_, output) = runTFS(
      s"""import tfs.*
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("path", "/etc/motd")}
         |    val ino = tfs_lookup(&path)
         |    if ino > 0
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin,
      prefill = defaultPrefill,
    )
    output shouldBe "Y"
  }

  "tfs_lookup returns -1 for nonexistent" in {
    val (_, output) = runTFS(
      s"""import tfs.*
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("path", "/nope")}
         |    val ino = tfs_lookup(&path)
         |    if ino == -1
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin,
      prefill = defaultPrefill,
    )
    output shouldBe "Y"
  }

  // ===== Stat =====

  "tfs_stat returns file mode and size" in {
    val (_, output) = runTFS(
      s"""import tfs.*
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("path", "/etc/motd")}
         |    val ino = tfs_lookup(&path)
         |    var stat: [7]int
         |    tfs_stat(ino, &stat)
         |    if (stat[0] >> 12) == 1
         |        putchar(70)
         |    if stat[4] == 9
         |        putchar(83)
         |    0
         |""".stripMargin,
      prefill = defaultPrefill,
    )
    output shouldBe "FS"
  }

  "tfs_stat returns dir mode" in {
    val (_, output) = runTFS(
      s"""import tfs.*
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("path", "/dev")}
         |    val ino = tfs_lookup(&path)
         |    var stat: [7]int
         |    tfs_stat(ino, &stat)
         |    if (stat[0] >> 12) == 2
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin,
      prefill = defaultPrefill,
    )
    output shouldBe "Y"
  }

  "tfs_stat returns char device mode" in {
    val (_, output) = runTFS(
      s"""import tfs.*
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("path", "/dev/tty0")}
         |    val ino = tfs_lookup(&path)
         |    var stat: [7]int
         |    tfs_stat(ino, &stat)
         |    if (stat[0] >> 12) == 3
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin,
      prefill = defaultPrefill,
    )
    output shouldBe "Y"
  }

  // ===== Read =====

  "tfs_read reads file content" in {
    val (_, output) = runTFS(
      s"""import tfs.*
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("path", "/etc/motd")}
         |    val ino = tfs_lookup(&path)
         |    var buf: [32]i8
         |    val n = tfs_read(ino, &buf, 0, 9)
         |    val p: *i8 = &buf
         |    var i = 0
         |    while i < n
         |        putchar(p[i])
         |        i += 1
         |    0
         |""".stripMargin,
      prefill = defaultPrefill,
    )
    output shouldBe "Hello TOS"
  }

  "tfs_read with offset" in {
    val (_, output) = runTFS(
      s"""import tfs.*
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("path", "/etc/motd")}
         |    val ino = tfs_lookup(&path)
         |    var buf: [32]i8
         |    val n = tfs_read(ino, &buf, 6, 3)
         |    val p: *i8 = &buf
         |    var i = 0
         |    while i < n
         |        putchar(p[i])
         |        i += 1
         |    0
         |""".stripMargin,
      prefill = defaultPrefill,
    )
    output shouldBe "TOS"
  }

  "tfs_read clamps to file size" in {
    val (_, output) = runTFS(
      s"""import tfs.*
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("path", "/etc/motd")}
         |    val ino = tfs_lookup(&path)
         |    var buf: [64]i8
         |    val n = tfs_read(ino, &buf, 0, 100)
         |    if n == 9
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin,
      prefill = defaultPrefill,
    )
    output shouldBe "Y"
  }

  "tfs_read past end returns 0" in {
    val (_, output) = runTFS(
      s"""import tfs.*
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("path", "/etc/motd")}
         |    val ino = tfs_lookup(&path)
         |    var buf: [32]i8
         |    val n = tfs_read(ino, &buf, 100, 10)
         |    if n == 0
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin,
      prefill = defaultPrefill,
    )
    output shouldBe "Y"
  }
}
