package io.github.edadma.trisc

class TFSMetadataTests extends TFSTestHelpers {

  val prefill: String =
    """/dev/tty0 char 0 0
      |/etc/motd file "Hello TOS"
      |""".stripMargin

  // ===== tfs_mknod =====

  "tfs_mknod char device" in {
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "c")}
         |    val ino = tfs_mknod(1, &name[0], name_len, 3, 7, 2)
         |    if ino > 0
         |        putchar(65)
         |    var stat: [7]int
         |    tfs_stat(ino, &stat[0])
         |    if (stat[0] >> 12) == 3
         |        putchar(66)
         |    // Check major/minor via inode direct[0]
         |    var ibuf: [32]i8
         |    tfs_read_inode(ino, &ibuf[0])
         |    val dev = ino_direct(&ibuf[0], 0)
         |    if (dev >> 8) == 7
         |        putchar(67)
         |    if (dev & 0xFF) == 2
         |        putchar(68)
         |    0
         |""".stripMargin, prefill = prefill)
    output shouldBe "ABCD"
  }

  // ===== tfs_chmod =====

  "tfs_chmod changes permission bits" in {
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "f")}
         |    val ino = tfs_create(1, &name[0], name_len, 1, 0x1A4)
         |    tfs_chmod(ino, 0x1FF)
         |    var stat: [7]int
         |    tfs_stat(ino, &stat[0])
         |    if (stat[0] & 0x1FF) == 0x1FF
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  // ===== tfs_chown =====

  "tfs_chown sets uid and gid" in {
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "f")}
         |    val ino = tfs_create(1, &name[0], name_len, 1, 0x1A4)
         |    tfs_chown(ino, 3, 7)
         |    var stat: [7]int
         |    tfs_stat(ino, &stat[0])
         |    if stat[2] == 3
         |        putchar(65)
         |    if stat[3] == 7
         |        putchar(66)
         |    0
         |""".stripMargin, prefill = prefill)
    output shouldBe "AB"
  }

  // ===== tfs_link =====

  "tfs_link increments nlinks" in {
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "f")}
         |    val ino = tfs_create(1, &name[0], name_len, 1, 0x1A4)
         |    var stat: [7]int
         |    tfs_stat(ino, &stat[0])
         |    if stat[1] == 1
         |        putchar(65)
         |${syslBytes("link", "l")}
         |    tfs_link(1, &link[0], link_len, ino)
         |    tfs_stat(ino, &stat[0])
         |    if stat[1] == 2
         |        putchar(66)
         |    0
         |""".stripMargin, prefill = prefill)
    output shouldBe "AB"
  }

  // ===== tfs_rmdir =====

  "tfs_rmdir rejects non-directory" in {
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "f")}
         |    tfs_create(1, &name[0], name_len, 1, 0x1A4)
         |    val r = tfs_rmdir(1, &name[0], name_len)
         |    if r == -1
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  // ===== tfs_rename =====

  "tfs_rename nonexistent returns -1" in {
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("old", "nope")}
         |${syslBytes("newn", "also_nope")}
         |    val r = tfs_rename(1, &old[0], old_len, 1, &newn[0], newn_len)
         |    if r == -1
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  "tfs_rename success" in {
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "a")}
         |    val ino = tfs_create(1, &name[0], name_len, 1, 0x1A4)
         |${syslBytes("newn", "b")}
         |    val r = tfs_rename(1, &name[0], name_len, 1, &newn[0], newn_len)
         |    if r == 0
         |        putchar(82)
         |    // Old gone
         |    if tfs_dir_lookup(1, &name[0], name_len) == -1
         |        putchar(71)
         |    // New found
         |    if tfs_dir_lookup(1, &newn[0], newn_len) == ino
         |        putchar(70)
         |    0
         |""".stripMargin, prefill = prefill)
    output shouldBe "RGF"
  }

  "tfs_rmdir success on empty dir" in {
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "d")}
         |    tfs_create(1, &name[0], name_len, 2, 0x1ED)
         |    val r = tfs_rmdir(1, &name[0], name_len)
         |    if r == 0
         |        putchar(82)
         |    if tfs_dir_lookup(1, &name[0], name_len) == -1
         |        putchar(71)
         |    0
         |""".stripMargin, prefill = prefill)
    output shouldBe "RG"
  }

  "tfs_link rejects directory" in {
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "d")}
         |    val ino = tfs_create(1, &name[0], name_len, 2, 0x1ED)
         |${syslBytes("link", "dl")}
         |    val r = tfs_link(1, &link[0], link_len, ino)
         |    if r == -1
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }
}
