package io.github.edadma.trisc

class TFSDirTests extends TFSTestHelpers {

  val prefill: String =
    """/dev/tty0 char 0 0
      |/etc/motd file "Hello TOS"
      |""".stripMargin

  // ===== tfs_dir_lookup =====

  "tfs_dir_lookup finds . in root" in {
    val (_, output) = runTFS(
      s"""import tfs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", ".")}
         |    val ino = tfs_dir_lookup(1, &name)
         |    if ino == 1
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  "tfs_dir_lookup finds .. in root" in {
    val (_, output) = runTFS(
      s"""import tfs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "..")}
         |    val ino = tfs_dir_lookup(1, &name)
         |    if ino == 1
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  "tfs_dir_lookup finds dev in root" in {
    val (_, output) = runTFS(
      s"""import tfs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "dev")}
         |    val ino = tfs_dir_lookup(1, &name)
         |    if ino > 0
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  "tfs_dir_lookup returns -1 for missing name" in {
    val (_, output) = runTFS(
      s"""import tfs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "nope")}
         |    val ino = tfs_dir_lookup(1, &name)
         |    if ino == -1
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  // ===== dir_add_entry / dir_remove_entry =====

  "dir_add_entry adds to root" in {
    val (_, output) = runTFS(
      s"""import tfs.*
         |main() -> int
         |    tfs_init()
         |    val ino = alloc_inode()
         |${syslBytes("name", "test")}
         |    val r = dir_add_entry(1, &name[0], ino)
         |    if r == 0
         |        putchar(65)
         |    // Verify lookup finds it
         |    val found = tfs_dir_lookup(1, &name[0])
         |    if found == ino
         |        putchar(66)
         |    0
         |""".stripMargin, prefill = prefill)
    output shouldBe "AB"
  }

  "dir_remove_entry removes from root" in {
    val (_, output) = runTFS(
      s"""import tfs.*
         |main() -> int
         |    tfs_init()
         |    val ino = alloc_inode()
         |${syslBytes("name", "rem")}
         |    dir_add_entry(1, &name[0], ino)
         |    val removed = dir_remove_entry(1, &name[0])
         |    if removed == ino
         |        putchar(65)
         |    // Verify gone
         |    val found = tfs_dir_lookup(1, &name[0])
         |    if found == -1
         |        putchar(66)
         |    0
         |""".stripMargin, prefill = prefill)
    output shouldBe "AB"
  }

  "dir_remove_entry returns -1 for missing" in {
    val (_, output) = runTFS(
      s"""import tfs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "nope")}
         |    val r = dir_remove_entry(1, &name[0])
         |    if r == -1
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  // ===== tfs_lookup =====

  "tfs_lookup root" in {
    val (_, output) = runTFS(
      s"""import tfs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/")}
         |    if tfs_lookup(&p) == 1
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  "tfs_lookup /dev/tty0" in {
    val (_, output) = runTFS(
      s"""import tfs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/dev/tty0")}
         |    val ino = tfs_lookup(&p)
         |    if ino > 0
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  "tfs_lookup nonexistent returns -1" in {
    val (_, output) = runTFS(
      s"""import tfs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/nonexistent")}
         |    if tfs_lookup(&p) == -1
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  // ===== tfs_readdir =====

  "tfs_readdir index 0 is dot" in {
    val (_, output) = runTFS(
      """import tfs.*
        |main() -> int
        |    tfs_init()
        |    var ino = 0
        |    var name: [15]i8
        |    tfs_readdir(1, 0, &ino, &name[0])
        |    if ino == 1
        |        putchar(65)
        |    if name[0] == 46
        |        putchar(66)
        |    0
        |""".stripMargin, prefill = prefill)
    output shouldBe "AB"
  }

  "tfs_readdir past end returns -1" in {
    val (_, output) = runTFS(
      """import tfs.*
        |main() -> int
        |    tfs_init()
        |    var ino = 0
        |    var name: [15]i8
        |    val r = tfs_readdir(1, 999, &ino, &name[0])
        |    if r == -1
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }
}
