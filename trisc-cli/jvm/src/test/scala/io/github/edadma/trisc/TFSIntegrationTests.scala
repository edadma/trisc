package io.github.edadma.trisc

class TFSIntegrationTests extends TFSTestHelpers {

  val defaultPrefill: String =
    """/dev/tty0 char 0 0
      |/dev/null char 0 1
      |/etc/motd file "Hello TOS"
      |/tmp dir
      |""".stripMargin

  // ===== Init =====

  "tfs_init reads superblock" in {
    val (cpu, output) = runTFS(
      """import "tfs"
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
      s"""import "tfs"
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
      s"""import "tfs"
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
      s"""import "tfs"
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
      s"""import "tfs"
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
      s"""import "tfs"
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
      s"""import "tfs"
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
      s"""import "tfs"
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
      s"""import "tfs"
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
      s"""import "tfs"
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
      s"""import "tfs"
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
      s"""import "tfs"
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
      s"""import "tfs"
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

  // ===== Create & Write =====

  "tfs_create and tfs_write" in {
    val (_, output) = runTFS(
      s"""import "tfs"
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "hello")}
         |    val ino = tfs_create(1, &name, 1, 0x1A4)
         |    if ino == -1
         |        putchar(69)
         |        return 1
         |    var data: [6]i8
         |    data[0] = 72
         |    data[1] = 101
         |    data[2] = 108
         |    data[3] = 108
         |    data[4] = 111
         |    tfs_write(ino, &data, 0, 5)
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

  "created file findable via lookup" in {
    val (_, output) = runTFS(
      s"""import "tfs"
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "newfile")}
         |    tfs_create(1, &name, 1, 0x1A4)
         |${syslBytes("path", "/newfile")}
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

  // ===== Mkdir =====

  "tfs_create directory has . and .." in {
    val (_, output) = runTFS(
      s"""import "tfs"
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "mydir")}
         |    val ino = tfs_create(1, &name, 2, 0x1ED)
         |    if ino == -1
         |        putchar(69)
         |        return 1
         |${syslBytes("dot", ".")}
         |${syslBytes("dotdot", "..")}
         |    val d = tfs_dir_lookup(ino, &dot)
         |    val dd = tfs_dir_lookup(ino, &dotdot)
         |    if d == ino
         |        putchar(68)
         |    if dd == 1
         |        putchar(80)
         |    0
         |""".stripMargin,
      prefill = defaultPrefill,
    )
    output shouldBe "DP"
  }

  "create file inside new directory" in {
    val (_, output) = runTFS(
      s"""import "tfs"
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("dirname", "data")}
         |    val dir_ino = tfs_create(1, &dirname, 2, 0x1ED)
         |${syslBytes("fname", "log")}
         |    val file_ino = tfs_create(dir_ino, &fname, 1, 0x1A4)
         |    var msg: [3]i8
         |    msg[0] = 79
         |    msg[1] = 75
         |    tfs_write(file_ino, &msg, 0, 2)
         |${syslBytes("path", "/data/log")}
         |    val found = tfs_lookup(&path)
         |    if found == file_ino
         |        putchar(89)
         |    else
         |        putchar(78)
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
      s"""import "tfs"
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "temp")}
         |    tfs_create(1, &name, 1, 0x1A4)
         |${syslBytes("path", "/temp")}
         |    val before = tfs_lookup(&path)
         |    if before > 0
         |        putchar(66)
         |    val result = tfs_unlink(1, &name)
         |    if result == 0
         |        putchar(85)
         |${syslBytes("path2", "/temp")}
         |    val after = tfs_lookup(&path2)
         |    if after == -1
         |        putchar(71)
         |    0
         |""".stripMargin,
      prefill = defaultPrefill,
    )
    output shouldBe "BUG"
  }

  "tfs_unlink nonexistent returns -1" in {
    val (_, output) = runTFS(
      s"""import "tfs"
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "nope")}
         |    val result = tfs_unlink(1, &name)
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

  // ===== Write =====

  "write updates file size" in {
    val (_, output) = runTFS(
      s"""import "tfs"
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "sized")}
         |    val ino = tfs_create(1, &name, 1, 0x1A4)
         |    var data: [4]i8
         |    data[0] = 65
         |    data[1] = 66
         |    data[2] = 67
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
      s"""import "tfs"
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "ext")}
         |    val ino = tfs_create(1, &name, 1, 0x1A4)
         |    var data: [4]i8
         |    data[0] = 65
         |    data[1] = 66
         |    tfs_write(ino, &data, 0, 2)
         |    data[0] = 67
         |    data[1] = 68
         |    tfs_write(ino, &data, 2, 2)
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
      s"""import "tfs"
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "ow")}
         |    val ino = tfs_create(1, &name, 1, 0x1A4)
         |    var data: [8]i8
         |    data[0] = 65
         |    data[1] = 66
         |    data[2] = 67
         |    data[3] = 68
         |    data[4] = 69
         |    tfs_write(ino, &data, 0, 5)
         |    data[0] = 88
         |    data[1] = 89
         |    tfs_write(ino, &data, 1, 2)
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
      s"""import "tfs"
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("path", "/dev")}
         |    val dev_dir = tfs_lookup(&path)
         |${syslBytes("name", "test")}
         |    val ino = tfs_mknod(dev_dir, &name, 3, 7, 2)
         |    if ino > 0
         |        putchar(67)
         |    var stat: [7]int
         |    tfs_stat(ino, &stat)
         |    if (stat[0] >> 12) == 3
         |        putchar(84)
         |${syslBytes("fullpath", "/dev/test")}
         |    val found = tfs_lookup(&fullpath)
         |    if found == ino
         |        putchar(76)
         |    0
         |""".stripMargin,
      prefill = defaultPrefill,
    )
    output shouldBe "CTL"
  }

  // ===== Truncate =====

  "tfs_truncate shrinks file" in {
    val (_, output) = runTFS(
      s"""import "tfs"
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "big")}
         |    val ino = tfs_create(1, &name[0], 1, 0x1A4)
         |    var data: [8]i8
         |    data[0] = 65
         |    data[1] = 66
         |    data[2] = 67
         |    data[3] = 68
         |    data[4] = 69
         |    tfs_write(ino, &data[0], 0, 5)
         |    tfs_truncate(ino, 3)
         |    var stat: [7]int
         |    tfs_stat(ino, &stat[0])
         |    if stat[4] == 3
         |        putchar(83)
         |    // Read back — should only get 3 bytes
         |    var buf: [8]i8
         |    val n = tfs_read(ino, &buf[0], 0, 10)
         |    val bp: *i8 = &buf[0]
         |    var i = 0
         |    while i < n
         |        putchar(bp[i])
         |        i += 1
         |    0
         |""".stripMargin,
      prefill = defaultPrefill,
    )
    output shouldBe "SABC"
  }

  "tfs_truncate to zero" in {
    val (_, output) = runTFS(
      s"""import "tfs"
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "f")}
         |    val ino = tfs_create(1, &name[0], 1, 0x1A4)
         |    var data: [4]i8
         |    data[0] = 65
         |    tfs_write(ino, &data[0], 0, 1)
         |    tfs_truncate(ino, 0)
         |    var stat: [7]int
         |    tfs_stat(ino, &stat[0])
         |    if stat[4] == 0
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin,
      prefill = defaultPrefill,
    )
    output shouldBe "Y"
  }

  // ===== Rmdir =====

  "tfs_rmdir removes empty directory" in {
    val (_, output) = runTFS(
      s"""import "tfs"
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "empty")}
         |    tfs_create(1, &name[0], 2, 0x1ED)
         |    val r = tfs_rmdir(1, &name[0])
         |    if r == 0
         |        putchar(82)
         |${syslBytes("path", "/empty")}
         |    val ino = tfs_lookup(&path[0])
         |    if ino == -1
         |        putchar(71)
         |    0
         |""".stripMargin,
      prefill = defaultPrefill,
    )
    output shouldBe "RG"
  }

  "tfs_rmdir rejects non-empty directory" in {
    val (_, output) = runTFS(
      s"""import "tfs"
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("dirname", "stuff")}
         |    val dir = tfs_create(1, &dirname[0], 2, 0x1ED)
         |${syslBytes("fname", "f")}
         |    tfs_create(dir, &fname[0], 1, 0x1A4)
         |    val r = tfs_rmdir(1, &dirname[0])
         |    if r == -1
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin,
      prefill = defaultPrefill,
    )
    output shouldBe "Y"
  }

  // ===== Rename =====

  "tfs_rename moves file" in {
    val (_, output) = runTFS(
      s"""import "tfs"
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "old")}
         |    val ino = tfs_create(1, &name[0], 1, 0x1A4)
         |    var data: [4]i8
         |    data[0] = 88
         |    tfs_write(ino, &data[0], 0, 1)
         |${syslBytes("newname", "new")}
         |    tfs_rename(1, &name[0], 1, &newname[0])
         |    // Old name gone
         |${syslBytes("oldpath", "/old")}
         |    if tfs_lookup(&oldpath[0]) == -1
         |        putchar(71)
         |    // New name works
         |${syslBytes("newpath", "/new")}
         |    val found = tfs_lookup(&newpath[0])
         |    if found == ino
         |        putchar(70)
         |    // Content preserved
         |    var buf: [4]i8
         |    tfs_read(found, &buf[0], 0, 1)
         |    val bp: *i8 = &buf[0]
         |    putchar(bp[0])
         |    0
         |""".stripMargin,
      prefill = defaultPrefill,
    )
    output shouldBe "GFX"
  }

  // ===== Readdir =====

  "tfs_readdir lists directory entries" in {
    val (_, output) = runTFS(
      s"""import "tfs"
         |
         |main() -> int
         |    tfs_init()
         |    // Root has: . .. dev etc tmp (from prefill)
         |    var ino = 0
         |    var name: [15]i8
         |    // Entry 0 should be .
         |    tfs_readdir(1, 0, &ino, &name[0])
         |    putchar(name[0])
         |    // Entry 1 should be ..
         |    tfs_readdir(1, 1, &ino, &name[0])
         |    putchar(name[0])
         |    putchar(name[1])
         |    // Entry 2 should be dev
         |    tfs_readdir(1, 2, &ino, &name[0])
         |    putchar(name[0])
         |    // Past end returns -1
         |    val r = tfs_readdir(1, 99, &ino, &name[0])
         |    if r == -1
         |        putchar(69)
         |    0
         |""".stripMargin,
      prefill = defaultPrefill,
    )
    output shouldBe "...dE"
  }

  // ===== Hard link =====

  "tfs_link creates hard link" in {
    val (_, output) = runTFS(
      s"""import "tfs"
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "orig")}
         |    val ino = tfs_create(1, &name[0], 1, 0x1A4)
         |    var data: [4]i8
         |    data[0] = 90
         |    tfs_write(ino, &data[0], 0, 1)
         |${syslBytes("link", "alias")}
         |    tfs_link(1, &link[0], ino)
         |    // Both names resolve to same inode
         |${syslBytes("p1", "/orig")}
         |${syslBytes("p2", "/alias")}
         |    val i1 = tfs_lookup(&p1[0])
         |    val i2 = tfs_lookup(&p2[0])
         |    if i1 == i2
         |        putchar(69)
         |    // nlinks should be 2
         |    var stat: [7]int
         |    tfs_stat(ino, &stat[0])
         |    if stat[1] == 2
         |        putchar(76)
         |    // Content accessible via link
         |    var buf: [4]i8
         |    tfs_read(i2, &buf[0], 0, 1)
         |    val bp: *i8 = &buf[0]
         |    putchar(bp[0])
         |    0
         |""".stripMargin,
      prefill = defaultPrefill,
    )
    output shouldBe "ELZ"
  }

  "tfs_link rejects directory" in {
    val (_, output) = runTFS(
      s"""import "tfs"
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("path", "/dev")}
         |    val dev_ino = tfs_lookup(&path[0])
         |${syslBytes("name", "devlink")}
         |    val r = tfs_link(1, &name[0], dev_ino)
         |    if r == -1
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin,
      prefill = defaultPrefill,
    )
    output shouldBe "Y"
  }

  // ===== Chmod =====

  "tfs_chmod changes permissions" in {
    val (_, output) = runTFS(
      s"""import "tfs"
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "script")}
         |    val ino = tfs_create(1, &name[0], 1, 0x1A4)
         |    // Initially 0644 (rw-r--r--)
         |    var stat: [7]int
         |    tfs_stat(ino, &stat[0])
         |    if (stat[0] & 0x1FF) == 0x1A4
         |        putchar(65)
         |    // chmod to 0755 (rwxr-xr-x)
         |    tfs_chmod(ino, 0x1ED)
         |    tfs_stat(ino, &stat[0])
         |    if (stat[0] & 0x1FF) == 0x1ED
         |        putchar(66)
         |    // Type bits preserved
         |    if (stat[0] >> 12) == 1
         |        putchar(67)
         |    0
         |""".stripMargin,
      prefill = defaultPrefill,
    )
    output shouldBe "ABC"
  }

  "tfs_chmod preserves file type" in {
    val (_, output) = runTFS(
      s"""import "tfs"
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "d")}
         |    val ino = tfs_create(1, &name[0], 2, 0x1ED)
         |    tfs_chmod(ino, 0x1FF)
         |    var stat: [7]int
         |    tfs_stat(ino, &stat[0])
         |    // Still a directory
         |    if (stat[0] >> 12) == 2
         |        putchar(68)
         |    // Permissions now 0777
         |    if (stat[0] & 0x1FF) == 0x1FF
         |        putchar(80)
         |    0
         |""".stripMargin,
      prefill = defaultPrefill,
    )
    output shouldBe "DP"
  }

  // ===== Chown =====

  "tfs_chown changes owner and group" in {
    val (_, output) = runTFS(
      s"""import "tfs"
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "owned")}
         |    val ino = tfs_create(1, &name[0], 1, 0x1A4)
         |    // Initially uid=0 gid=0
         |    var stat: [7]int
         |    tfs_stat(ino, &stat[0])
         |    if stat[2] == 0
         |        putchar(65)
         |    // Change to uid=5 gid=10
         |    tfs_chown(ino, 5, 10)
         |    tfs_stat(ino, &stat[0])
         |    if stat[2] == 5
         |        putchar(66)
         |    if stat[3] == 10
         |        putchar(67)
         |    0
         |""".stripMargin,
      prefill = defaultPrefill,
    )
    output shouldBe "ABC"
  }

  // ===== Filesystem info =====

  "tfs_freeblocks and tfs_freeinodes" in {
    val (_, output) = runTFS(
      s"""import "tfs"
         |
         |main() -> int
         |    tfs_init()
         |    val fb1 = tfs_freeblocks()
         |    val fi1 = tfs_freeinodes()
         |    if fb1 > 0
         |        putchar(66)
         |    if fi1 > 0
         |        putchar(73)
         |    // Create a file with data — should use 1 inode + 1 block
         |${syslBytes("name", "f")}
         |    val ino = tfs_create(1, &name[0], 1, 0x1A4)
         |    var data: [4]i8
         |    data[0] = 65
         |    tfs_write(ino, &data[0], 0, 1)
         |    val fb2 = tfs_freeblocks()
         |    val fi2 = tfs_freeinodes()
         |    if fb2 == fb1 - 1
         |        putchar(98)
         |    if fi2 == fi1 - 1
         |        putchar(105)
         |    0
         |""".stripMargin,
      prefill = defaultPrefill,
    )
    output shouldBe "BIbi"
  }
}
