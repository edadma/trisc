package io.github.edadma.trisc

class TFSSyslTests extends TFSTestHelpers {

  val defaultPrefill: String =
    """/dev/tty0 char 0 0
      |/dev/null char 0 1
      |/etc/motd file "Hello TOS"
      |/tmp dir
      |""".stripMargin

  // ===== Init & superblock =====

  "tfs_init reads superblock" in {
    val (cpu, output) = runTFS(
      """import "tfs"
        |
        |main() -> int
        |    tfs_init()
        |    // If init worked, sb_block_size should be 512
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
      """import "tfs"
        |
        |main() -> int
        |    tfs_init()
        |    val ino = tfs_lookup("/")
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
      """import "tfs"
        |
        |main() -> int
        |    tfs_init()
        |    val ino = tfs_lookup("/dev")
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
      """import "tfs"
        |
        |main() -> int
        |    tfs_init()
        |    val ino = tfs_lookup("/dev/tty0")
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
      """import "tfs"
        |
        |main() -> int
        |    tfs_init()
        |    val ino = tfs_lookup("/etc/motd")
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

  "tfs_lookup returns -1 for nonexistent file" in {
    val (_, output) = runTFS(
      """import "tfs"
        |
        |main() -> int
        |    tfs_init()
        |    val ino = tfs_lookup("/nope")
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

  "tfs_lookup returns -1 for nonexistent nested path" in {
    val (_, output) = runTFS(
      """import "tfs"
        |
        |main() -> int
        |    tfs_init()
        |    val ino = tfs_lookup("/dev/missing")
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
      """import "tfs"
        |
        |main() -> int
        |    tfs_init()
        |    val ino = tfs_lookup("/etc/motd")
        |    var stat: [7]int
        |    tfs_stat(ino, &stat)
        |    val mode = stat[0]
        |    val size = stat[4]
        |    // mode should have regular file type (0x1xxx)
        |    if (mode >> 12) == 1
        |        putchar(70)
        |    // size should be 9 ("Hello TOS")
        |    if size == 9
        |        putchar(83)
        |    0
        |""".stripMargin,
      prefill = defaultPrefill,
    )
    output shouldBe "FS"
  }

  "tfs_stat returns dir mode" in {
    val (_, output) = runTFS(
      """import "tfs"
        |
        |main() -> int
        |    tfs_init()
        |    val ino = tfs_lookup("/dev")
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
      """import "tfs"
        |
        |main() -> int
        |    tfs_init()
        |    val ino = tfs_lookup("/dev/tty0")
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
      """import "tfs"
        |
        |main() -> int
        |    tfs_init()
        |    val ino = tfs_lookup("/etc/motd")
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
      """import "tfs"
        |
        |main() -> int
        |    tfs_init()
        |    val ino = tfs_lookup("/etc/motd")
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
      """import "tfs"
        |
        |main() -> int
        |    tfs_init()
        |    val ino = tfs_lookup("/etc/motd")
        |    var buf: [64]i8
        |    val n = tfs_read(ino, &buf, 0, 100)
        |    // Should return 9 (file size), not 100
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
      """import "tfs"
        |
        |main() -> int
        |    tfs_init()
        |    val ino = tfs_lookup("/etc/motd")
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

  // ===== Create & Write =====

  "tfs_create creates a file and tfs_write writes to it" in {
    val (_, output) = runTFS(
      """import "tfs"
        |
        |main() -> int
        |    tfs_init()
        |    val root = 1
        |    val ino = tfs_create(root, "hello", 1, 0x1A4)
        |    if ino == -1
        |        putchar(69)
        |        return 1
        |    var data: [6]i8
        |    val p: *i8 = &data
        |    p[0] = 72
        |    p[1] = 101
        |    p[2] = 108
        |    p[3] = 108
        |    p[4] = 111
        |    p[5] = 0
        |    val written = tfs_write(ino, &data, 0, 5)
        |    // Read it back
        |    var buf: [32]i8
        |    val n = tfs_read(ino, &buf, 0, 5)
        |    val bp: *i8 = &buf
        |    var i = 0
        |    while i < n
        |        putchar(bp[i])
        |        i += 1
        |    0
        |""".stripMargin,
      prefill = defaultPrefill,
    )
    output shouldBe "Hello"
  }

  "created file is findable via lookup" in {
    val (_, output) = runTFS(
      """import "tfs"
        |
        |main() -> int
        |    tfs_init()
        |    val root = 1
        |    tfs_create(root, "newfile", 1, 0x1A4)
        |    val ino = tfs_lookup("/newfile")
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

  // ===== Mkdir =====

  "tfs_create makes directory with . and .." in {
    val (_, output) = runTFS(
      """import "tfs"
        |
        |main() -> int
        |    tfs_init()
        |    val root = 1
        |    val ino = tfs_create(root, "mydir", 2, 0x1ED)
        |    if ino == -1
        |        putchar(69)
        |        return 1
        |    // Look up . in the new dir
        |    val dot = tfs_dir_lookup(ino, ".")
        |    val dotdot = tfs_dir_lookup(ino, "..")
        |    if dot == ino
        |        putchar(68)
        |    if dotdot == root
        |        putchar(80)
        |    0
        |""".stripMargin,
      prefill = defaultPrefill,
    )
    output shouldBe "DP"
  }

  "create file inside new directory" in {
    val (_, output) = runTFS(
      """import "tfs"
        |
        |main() -> int
        |    tfs_init()
        |    val root = 1
        |    val dir_ino = tfs_create(root, "data", 2, 0x1ED)
        |    val file_ino = tfs_create(dir_ino, "log", 1, 0x1A4)
        |    var msg: [4]i8
        |    val p: *i8 = &msg
        |    p[0] = 79
        |    p[1] = 75
        |    p[2] = 0
        |    tfs_write(file_ino, &msg, 0, 2)
        |    // Lookup via path
        |    val found = tfs_lookup("/data/log")
        |    if found == file_ino
        |        putchar(89)
        |    else
        |        putchar(78)
        |    // Read content back
        |    var buf: [8]i8
        |    tfs_read(found, &buf, 0, 2)
        |    val bp: *i8 = &buf
        |    putchar(bp[0])
        |    putchar(bp[1])
        |    0
        |""".stripMargin,
      prefill = defaultPrefill,
    )
    output shouldBe "YOK"
  }

  // ===== Unlink =====

  "tfs_unlink removes file" in {
    val (_, output) = runTFS(
      """import "tfs"
        |
        |main() -> int
        |    tfs_init()
        |    val root = 1
        |    tfs_create(root, "temp", 1, 0x1A4)
        |    // Verify it exists
        |    val before = tfs_lookup("/temp")
        |    if before > 0
        |        putchar(66)
        |    // Unlink it
        |    val result = tfs_unlink(root, "temp")
        |    if result == 0
        |        putchar(85)
        |    // Verify it's gone
        |    val after = tfs_lookup("/temp")
        |    if after == -1
        |        putchar(71)
        |    0
        |""".stripMargin,
      prefill = defaultPrefill,
    )
    output shouldBe "BUG"
  }

  "tfs_unlink on nonexistent returns -1" in {
    val (_, output) = runTFS(
      """import "tfs"
        |
        |main() -> int
        |    tfs_init()
        |    val result = tfs_unlink(1, "nope")
        |    if result == -1
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin,
      prefill = defaultPrefill,
    )
    output shouldBe "Y"
  }

  // ===== Write then read back =====

  "write updates file size" in {
    val (_, output) = runTFS(
      """import "tfs"
        |
        |main() -> int
        |    tfs_init()
        |    val ino = tfs_create(1, "sized", 1, 0x1A4)
        |    var data: [4]i8
        |    val p: *i8 = &data
        |    p[0] = 65
        |    p[1] = 66
        |    p[2] = 67
        |    tfs_write(ino, &data, 0, 3)
        |    var stat: [7]int
        |    tfs_stat(ino, &stat)
        |    if stat[4] == 3
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin,
      prefill = defaultPrefill,
    )
    output shouldBe "Y"
  }

  "write at offset extends file" in {
    val (_, output) = runTFS(
      """import "tfs"
        |
        |main() -> int
        |    tfs_init()
        |    val ino = tfs_create(1, "ext", 1, 0x1A4)
        |    var data: [4]i8
        |    val p: *i8 = &data
        |    p[0] = 65
        |    p[1] = 66
        |    // Write "AB" at offset 0
        |    tfs_write(ino, &data, 0, 2)
        |    p[0] = 67
        |    p[1] = 68
        |    // Write "CD" at offset 2
        |    tfs_write(ino, &data, 2, 2)
        |    // File should be 4 bytes: "ABCD"
        |    var buf: [8]i8
        |    tfs_read(ino, &buf, 0, 4)
        |    val bp: *i8 = &buf
        |    putchar(bp[0])
        |    putchar(bp[1])
        |    putchar(bp[2])
        |    putchar(bp[3])
        |    0
        |""".stripMargin,
      prefill = defaultPrefill,
    )
    output shouldBe "ABCD"
  }

  "overwrite part of file" in {
    val (_, output) = runTFS(
      """import "tfs"
        |
        |main() -> int
        |    tfs_init()
        |    val ino = tfs_create(1, "ow", 1, 0x1A4)
        |    var data: [8]i8
        |    val p: *i8 = &data
        |    p[0] = 65
        |    p[1] = 66
        |    p[2] = 67
        |    p[3] = 68
        |    p[4] = 69
        |    tfs_write(ino, &data, 0, 5)
        |    // Overwrite bytes 1-3 with "XY"
        |    p[0] = 88
        |    p[1] = 89
        |    tfs_write(ino, &data, 1, 2)
        |    // Read back: should be "AXYДЕ" -> "AXYDE"
        |    var buf: [8]i8
        |    tfs_read(ino, &buf, 0, 5)
        |    val bp: *i8 = &buf
        |    putchar(bp[0])
        |    putchar(bp[1])
        |    putchar(bp[2])
        |    putchar(bp[3])
        |    putchar(bp[4])
        |    0
        |""".stripMargin,
      prefill = defaultPrefill,
    )
    output shouldBe "AXYDE"
  }

  // ===== Device nodes =====

  "tfs_mknod creates device node" in {
    val (_, output) = runTFS(
      """import "tfs"
        |
        |main() -> int
        |    tfs_init()
        |    val dev_dir = tfs_lookup("/dev")
        |    val ino = tfs_mknod(dev_dir, "test", 3, 7, 2)
        |    if ino > 0
        |        putchar(67)
        |    // Verify via stat
        |    var stat: [7]int
        |    tfs_stat(ino, &stat)
        |    if (stat[0] >> 12) == 3
        |        putchar(84)
        |    // Verify lookup
        |    val found = tfs_lookup("/dev/test")
        |    if found == ino
        |        putchar(76)
        |    0
        |""".stripMargin,
      prefill = defaultPrefill,
    )
    output shouldBe "CTL"
  }
}
