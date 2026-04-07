package io.github.edadma.trisc

class TFSWriteIntegrationTests extends TFSTestHelpers {

  val defaultPrefill: String =
    """/dev/tty0 char 0 0
      |/dev/null char 0 1
      |/etc/motd file "Hello TOS"
      |/tmp dir
      |""".stripMargin

  // ===== Create & Write =====

  "tfs_create and tfs_write" in {
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "hello")}
         |    val ino = tfs_create(1, &name, name_len, 1, 0x1A4)
         |    if ino == -1
         |        putchar(69)
         |        return 1
         |    var data: [6]byte
         |    data[0] = 72
         |    data[1] = 101
         |    data[2] = 108
         |    data[3] = 108
         |    data[4] = 111
         |    tfs_write(ino, &data, 0, 5)
         |    var buf: [32]byte
         |    val n = tfs_read(ino, &buf, 0, 5)
         |    val bp: *byte = &buf
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
      s"""import oskit.fs.*
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "newfile")}
         |    tfs_create(1, &name, name_len, 1, 0x1A4)
         |${syslBytes("path", "/newfile")}
         |    val ino = tfs_lookup(&path, path_len)
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
      s"""import oskit.fs.*
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "mydir")}
         |    val ino = tfs_create(1, &name, name_len, 2, 0x1ED)
         |    if ino == -1
         |        putchar(69)
         |        return 1
         |${syslBytes("dot", ".")}
         |${syslBytes("dotdot", "..")}
         |    val d = tfs_dir_lookup(ino, &dot, dot_len)
         |    val dd = tfs_dir_lookup(ino, &dotdot, dotdot_len)
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
      s"""import oskit.fs.*
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("dirname", "data")}
         |    val dir_ino = tfs_create(1, &dirname, dirname_len, 2, 0x1ED)
         |${syslBytes("fname", "log")}
         |    val file_ino = tfs_create(dir_ino, &fname, fname_len, 1, 0x1A4)
         |    var msg: [3]byte
         |    msg[0] = 79
         |    msg[1] = 75
         |    tfs_write(file_ino, &msg, 0, 2)
         |${syslBytes("path", "/data/log")}
         |    val found = tfs_lookup(&path, path_len)
         |    if found == file_ino
         |        putchar(89)
         |    else
         |        putchar(78)
         |    var buf: [8]byte
         |    tfs_read(found, &buf, 0, 2)
         |    val bp: *byte = &buf
         |    putchar(bp[0])
         |    putchar(bp[1])
         |    0
         |""".stripMargin,
      prefill = defaultPrefill,
    )
    output shouldBe "YOK"
  }

  // ===== Write =====

  "write updates file size" in {
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "sized")}
         |    val ino = tfs_create(1, &name, name_len, 1, 0x1A4)
         |    var data: [4]byte
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
      s"""import oskit.fs.*
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "ext")}
         |    val ino = tfs_create(1, &name, name_len, 1, 0x1A4)
         |    var data: [4]byte
         |    data[0] = 65
         |    data[1] = 66
         |    tfs_write(ino, &data, 0, 2)
         |    data[0] = 67
         |    data[1] = 68
         |    tfs_write(ino, &data, 2, 2)
         |    var buf: [8]byte
         |    tfs_read(ino, &buf, 0, 4)
         |    val bp: *byte = &buf
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
      s"""import oskit.fs.*
         |
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "ow")}
         |    val ino = tfs_create(1, &name, name_len, 1, 0x1A4)
         |    var data: [8]byte
         |    data[0] = 65
         |    data[1] = 66
         |    data[2] = 67
         |    data[3] = 68
         |    data[4] = 69
         |    tfs_write(ino, &data, 0, 5)
         |    data[0] = 88
         |    data[1] = 89
         |    tfs_write(ino, &data, 1, 2)
         |    var buf: [8]byte
         |    tfs_read(ino, &buf, 0, 5)
         |    val bp: *byte = &buf
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
}
