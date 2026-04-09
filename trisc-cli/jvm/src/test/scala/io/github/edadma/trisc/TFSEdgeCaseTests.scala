package io.github.edadma.trisc

/** Edge-case tests for TFS sysl-side filesystem operations.
  *
  * Block size = 4096 bytes. NUM_DIRECT = 6, so files > 24576 bytes
  * use indirect blocks. These tests exercise the boundary between
  * direct and indirect block pointers, cross-block reads/writes,
  * and large file operations.
  */
class TFSEdgeCaseTests extends TFSTestHelpers {

  private val BS = 4096 // block size

  /** Prefill a file at /big with `size` bytes of repeated ASCII chars.
    * Each block gets a different letter so we can verify which block was read.
    * Pattern: bytes 0..4095 = 'A', 4096..8191 = 'B', etc.
    */
  private def patternPrefill(size: Int): String =
    val sb = new StringBuilder
    for i <- 0 until size do
      sb += ('A' + i / BS).toChar
    s"""/big file "$sb"\n"""

  // ===== Read: direct block boundary (6 blocks = 24576 bytes) =====

  "tfs_read file filling all 6 direct blocks" in {
    val prefill = patternPrefill(6 * BS)
    val (cpu, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/big")}
         |    val ino = tfs_lookup(&p, p_len)
         |    var buf: [4]byte
         |    val bp: *byte = &buf
         |    tfs_read(ino, bp, 0, 1)
         |    putchar(bp[0])
         |    tfs_read(ino, bp, $BS, 1)
         |    putchar(bp[0])
         |    tfs_read(ino, bp, ${2 * BS}, 1)
         |    putchar(bp[0])
         |    tfs_read(ino, bp, ${3 * BS}, 1)
         |    putchar(bp[0])
         |    tfs_read(ino, bp, ${4 * BS}, 1)
         |    putchar(bp[0])
         |    tfs_read(ino, bp, ${5 * BS}, 1)
         |    putchar(bp[0])
         |    var stat: [7]int
         |    tfs_stat(ino, &stat)
         |    if stat[4] == ${6 * BS}
         |        putchar(33)
         |    0
         |""".stripMargin,
      prefill = prefill,
      sectors = 128,
    )
    output shouldBe "ABCDEF!"
    cpu.state shouldBe State.Halt
  }

  // ===== Read: first indirect block (7 blocks) =====

  "tfs_read file with 7 blocks (first indirect)" in {
    val prefill = patternPrefill(7 * BS)
    val (cpu, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/big")}
         |    val ino = tfs_lookup(&p, p_len)
         |    var buf: [4]byte
         |    val bp: *byte = &buf
         |    tfs_read(ino, bp, 0, 1)
         |    putchar(bp[0])
         |    tfs_read(ino, bp, ${5 * BS}, 1)
         |    putchar(bp[0])
         |    tfs_read(ino, bp, ${6 * BS}, 1)
         |    putchar(bp[0])
         |    var stat: [7]int
         |    tfs_stat(ino, &stat)
         |    if stat[4] == ${7 * BS}
         |        putchar(33)
         |    0
         |""".stripMargin,
      prefill = prefill,
      sectors = 128,
    )
    output shouldBe "AFG!"
    cpu.state shouldBe State.Halt
  }

  // ===== Read: multiple indirect blocks =====

  "tfs_read file with 10 blocks (4 indirect)" in {
    val prefill = patternPrefill(10 * BS)
    val (cpu, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/big")}
         |    val ino = tfs_lookup(&p, p_len)
         |    var buf: [4]byte
         |    val bp: *byte = &buf
         |    tfs_read(ino, bp, 0, 1)
         |    putchar(bp[0])
         |    tfs_read(ino, bp, ${5 * BS}, 1)
         |    putchar(bp[0])
         |    tfs_read(ino, bp, ${6 * BS}, 1)
         |    putchar(bp[0])
         |    tfs_read(ino, bp, ${9 * BS}, 1)
         |    putchar(bp[0])
         |    var stat: [7]int
         |    tfs_stat(ino, &stat)
         |    if stat[4] == ${10 * BS}
         |        putchar(33)
         |    0
         |""".stripMargin,
      prefill = prefill,
      sectors = 128,
    )
    output shouldBe "AFGJ!"
    cpu.state shouldBe State.Halt
  }

  // ===== Read: crossing block boundary =====

  "tfs_read crossing direct block boundary" in {
    val prefill = patternPrefill(2 * BS)
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/big")}
         |    val ino = tfs_lookup(&p, p_len)
         |    var buf: [8]byte
         |    val n = tfs_read(ino, &buf, ${BS - 2}, 4)
         |    val bp: *byte = &buf
         |    putchar(bp[0])
         |    putchar(bp[1])
         |    putchar(bp[2])
         |    putchar(bp[3])
         |    if n == 4
         |        putchar(33)
         |    0
         |""".stripMargin,
      prefill = prefill,
      sectors = 128,
    )
    output shouldBe "AABB!"
  }

  "tfs_read crossing direct-to-indirect boundary" in {
    val prefill = patternPrefill(7 * BS)
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/big")}
         |    val ino = tfs_lookup(&p, p_len)
         |    var buf: [8]byte
         |    val n = tfs_read(ino, &buf, ${6 * BS - 2}, 4)
         |    val bp: *byte = &buf
         |    putchar(bp[0])
         |    putchar(bp[1])
         |    putchar(bp[2])
         |    putchar(bp[3])
         |    if n == 4
         |        putchar(33)
         |    0
         |""".stripMargin,
      prefill = prefill,
      sectors = 128,
    )
    output shouldBe "FFGG!"
  }

  "tfs_read crossing indirect block boundaries" in {
    val prefill = patternPrefill(8 * BS)
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/big")}
         |    val ino = tfs_lookup(&p, p_len)
         |    var buf: [8]byte
         |    val n = tfs_read(ino, &buf, ${7 * BS - 2}, 4)
         |    val bp: *byte = &buf
         |    putchar(bp[0])
         |    putchar(bp[1])
         |    putchar(bp[2])
         |    putchar(bp[3])
         |    if n == 4
         |        putchar(33)
         |    0
         |""".stripMargin,
      prefill = prefill,
      sectors = 128,
    )
    output shouldBe "GGHH!"
  }

  // ===== Read: exact block boundary offset =====

  "tfs_read starting at exact block boundary" in {
    val prefill = patternPrefill(2 * BS)
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/big")}
         |    val ino = tfs_lookup(&p, p_len)
         |    var buf: [4]byte
         |    val n = tfs_read(ino, &buf, $BS, 2)
         |    val bp: *byte = &buf
         |    putchar(bp[0])
         |    putchar(bp[1])
         |    if n == 2
         |        putchar(33)
         |    0
         |""".stripMargin,
      prefill = prefill,
      sectors = 128,
    )
    output shouldBe "BB!"
  }

  "tfs_read starting at indirect block boundary" in {
    val prefill = patternPrefill(7 * BS)
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/big")}
         |    val ino = tfs_lookup(&p, p_len)
         |    var buf: [4]byte
         |    val n = tfs_read(ino, &buf, ${6 * BS}, 2)
         |    val bp: *byte = &buf
         |    putchar(bp[0])
         |    putchar(bp[1])
         |    if n == 2
         |        putchar(33)
         |    0
         |""".stripMargin,
      prefill = prefill,
      sectors = 128,
    )
    output shouldBe "GG!"
  }

  // ===== Read: full content verification =====

  "tfs_read full 7-block file in loop" in {
    val prefill = patternPrefill(7 * BS)
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/big")}
         |    val ino = tfs_lookup(&p, p_len)
         |    var buf: [4096]byte
         |    val bp: *byte = &buf
         |    var offset = 0
         |    while offset < ${7 * BS}
         |        val n = tfs_read(ino, bp, offset, $BS)
         |        if n <= 0
         |            putchar(63)
         |            return 1
         |        putchar(bp[0])
         |        offset += n
         |    0
         |""".stripMargin,
      prefill = prefill,
      sectors = 128,
      maxCycles = 10000000,
    )
    output shouldBe "ABCDEFG"
  }

  // ===== Read: partial last block =====

  "tfs_read partial last block in indirect region" in {
    // File is 6 full blocks + 128 bytes in block 7
    val fileSize = 6 * BS + 128
    val prefill = patternPrefill(fileSize)
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/big")}
         |    val ino = tfs_lookup(&p, p_len)
         |    var stat: [7]int
         |    tfs_stat(ino, &stat)
         |    if stat[4] == $fileSize
         |        putchar(83)
         |    var buf: [4096]byte
         |    val bp: *byte = &buf
         |    val n = tfs_read(ino, bp, ${6 * BS}, $BS)
         |    if n == 128
         |        putchar(78)
         |    putchar(bp[0])
         |    0
         |""".stripMargin,
      prefill = prefill,
      sectors = 128,
    )
    output shouldBe "SNG"
  }

  // ===== Read: clamp and EOF in indirect region =====

  "tfs_read past EOF in indirect region returns 0" in {
    val prefill = patternPrefill(7 * BS)
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/big")}
         |    val ino = tfs_lookup(&p, p_len)
         |    var buf: [4]byte
         |    val n = tfs_read(ino, &buf, ${8 * BS}, 10)
         |    if n == 0
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin,
      prefill = prefill,
      sectors = 128,
    )
    output shouldBe "Y"
  }

  "tfs_read clamps at EOF in indirect region" in {
    val prefill = patternPrefill(7 * BS)
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/big")}
         |    val ino = tfs_lookup(&p, p_len)
         |    var buf: [4096]byte
         |    val n = tfs_read(ino, &buf, ${6 * BS}, $BS)
         |    if n == $BS
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin,
      prefill = prefill,
      sectors = 128,
    )
    output shouldBe "Y"
  }

  // ===== Write: into indirect blocks =====

  "tfs_write spanning into indirect blocks" in {
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "f")}
         |    val ino = tfs_create(1, &name, name_len, 1, 0x1A4)
         |    var data: [4096]byte
         |    val dp: *byte = &data
         |    var rbuf: [4]byte
         |    val rp: *byte = &rbuf
         |    var offset = 0
         |    var ch = 65
         |    while offset < ${7 * BS}
         |        var j = 0
         |        while j < $BS
         |            dp[j] = byte(ch)
         |            j += 1
         |        var count = $BS
         |        if offset + count > ${7 * BS}
         |            count = ${7 * BS} - offset
         |        tfs_write(ino, dp, offset, count)
         |        offset += count
         |        ch += 1
         |    var stat: [7]int
         |    tfs_stat(ino, &stat)
         |    if stat[4] == ${7 * BS}
         |        putchar(83)
         |    tfs_read(ino, rp, 0, 1)
         |    putchar(rp[0])
         |    tfs_read(ino, rp, ${5 * BS}, 1)
         |    putchar(rp[0])
         |    tfs_read(ino, rp, ${6 * BS}, 1)
         |    putchar(rp[0])
         |    0
         |""".stripMargin,
      prefill = "/tmp dir\n",
      sectors = 128,
      maxCycles = 10000000,
    )
    output shouldBe "SAFG"
  }

  "tfs_write at offset in indirect region" in {
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "f")}
         |    val ino = tfs_create(1, &name, name_len, 1, 0x1A4)
         |    var data: [4096]byte
         |    val dp: *byte = &data
         |    var offset = 0
         |    while offset < ${7 * BS}
         |        var j = 0
         |        while j < $BS
         |            dp[j] = 65
         |            j += 1
         |        var count = $BS
         |        if offset + count > ${7 * BS}
         |            count = ${7 * BS} - offset
         |        tfs_write(ino, dp, offset, count)
         |        offset += count
         |    dp[0] = 90
         |    tfs_write(ino, dp, ${6 * BS + 100}, 1)
         |    var rbuf: [4]byte
         |    val rp: *byte = &rbuf
         |    tfs_read(ino, rp, ${6 * BS + 100}, 1)
         |    putchar(rp[0])
         |    tfs_read(ino, rp, ${6 * BS + 99}, 1)
         |    putchar(rp[0])
         |    0
         |""".stripMargin,
      prefill = "/tmp dir\n",
      sectors = 128,
      maxCycles = 10000000,
    )
    output shouldBe "ZA"
  }

  // ===== Write: crossing direct-to-indirect boundary =====

  "tfs_write crossing direct-to-indirect boundary" in {
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "f")}
         |    val ino = tfs_create(1, &name, name_len, 1, 0x1A4)
         |    var data: [4096]byte
         |    val dp: *byte = &data
         |    var j = 0
         |    while j < $BS
         |        dp[j] = 65
         |        j += 1
         |    var offset = 0
         |    while offset < ${6 * BS}
         |        tfs_write(ino, dp, offset, $BS)
         |        offset += $BS
         |    j = 0
         |    while j < $BS
         |        dp[j] = 90
         |        j += 1
         |    tfs_write(ino, dp, ${6 * BS}, $BS)
         |    var rbuf: [4]byte
         |    val rp: *byte = &rbuf
         |    tfs_read(ino, rp, ${6 * BS - 1}, 1)
         |    putchar(rp[0])
         |    tfs_read(ino, rp, ${6 * BS}, 1)
         |    putchar(rp[0])
         |    var stat: [7]int
         |    tfs_stat(ino, &stat)
         |    if stat[4] == ${7 * BS}
         |        putchar(33)
         |    0
         |""".stripMargin,
      prefill = "/tmp dir\n",
      sectors = 128,
      maxCycles = 10000000,
    )
    output shouldBe "AZ!"
  }

  // ===== Truncate with indirect blocks =====

  "tfs_truncate file with indirect blocks to direct-only size" in {
    val prefill = patternPrefill(7 * BS)
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/big")}
         |    val ino = tfs_lookup(&p, p_len)
         |    tfs_truncate(ino, ${2 * BS})
         |    var stat: [7]int
         |    tfs_stat(ino, &stat)
         |    if stat[4] == ${2 * BS}
         |        putchar(83)
         |    var buf: [4]byte
         |    val bp: *byte = &buf
         |    tfs_read(ino, bp, 0, 1)
         |    putchar(bp[0])
         |    tfs_read(ino, bp, $BS, 1)
         |    putchar(bp[0])
         |    val n = tfs_read(ino, bp, ${4 * BS}, 1)
         |    if n == 0
         |        putchar(33)
         |    0
         |""".stripMargin,
      prefill = prefill,
      sectors = 128,
    )
    output shouldBe "SAB!"
  }

  "tfs_truncate to zero on file with indirect blocks" in {
    val prefill = patternPrefill(7 * BS)
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/big")}
         |    val ino = tfs_lookup(&p, p_len)
         |    tfs_truncate(ino, 0)
         |    var stat: [7]int
         |    tfs_stat(ino, &stat)
         |    if stat[4] == 0
         |        putchar(89)
         |    var buf: [4]byte
         |    val n = tfs_read(ino, &buf, 0, 1)
         |    if n == 0
         |        putchar(33)
         |    0
         |""".stripMargin,
      prefill = prefill,
      sectors = 128,
    )
    output shouldBe "Y!"
  }

  // ===== Unlink with indirect blocks =====

  "tfs_unlink file with indirect blocks frees all blocks" in {
    val prefill = patternPrefill(7 * BS)
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |    val fb1 = tfs_freeblocks()
         |${syslBytes("name", "big")}
         |    tfs_unlink(1, &name, name_len)
         |    val fb2 = tfs_freeblocks()
         |    // Should free 7 data blocks + 1 indirect block = 8 blocks
         |    if fb2 == fb1 + 8
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin,
      prefill = prefill,
      sectors = 128,
    )
    output shouldBe "Y"
  }

  // ===== Binary file content via files parameter =====

  "tfs_read binary file content" in {
    val data = Array.tabulate(256)(i => i.toByte)
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/bin/data")}
         |    val ino = tfs_lookup(&p, p_len)
         |    if ino < 0
         |        putchar(63)
         |        return 1
         |    var buf: [4]byte
         |    val bp: *byte = &buf
         |    tfs_read(ino, bp, 0, 1)
         |    if bp[0] == 0
         |        putchar(65)
         |    tfs_read(ino, bp, 65, 1)
         |    putchar(bp[0])
         |    tfs_read(ino, bp, 90, 1)
         |    putchar(bp[0])
         |    var stat: [7]int
         |    tfs_stat(ino, &stat)
         |    if stat[4] == 256
         |        putchar(33)
         |    0
         |""".stripMargin,
      prefill = "/bin dir\n",
      files = Map("/bin/data" -> data),
      sectors = 128,
    )
    output shouldBe "AAZ!"
  }

  // ===== File exactly at boundary sizes =====

  "tfs_read file of exactly 1 byte" in {
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/etc/x")}
         |    val ino = tfs_lookup(&p, p_len)
         |    var buf: [4]byte
         |    val bp: *byte = &buf
         |    val n = tfs_read(ino, bp, 0, 10)
         |    if n == 1
         |        putchar(78)
         |    putchar(bp[0])
         |    0
         |""".stripMargin,
      prefill = """/etc/x file "Z"""" + "\n",
      sectors = 128,
    )
    output shouldBe "NZ"
  }

  "tfs_read file of exactly one block" in {
    val content = "X" * BS
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/etc/x")}
         |    val ino = tfs_lookup(&p, p_len)
         |    var stat: [7]int
         |    tfs_stat(ino, &stat)
         |    if stat[4] == $BS
         |        putchar(83)
         |    var buf: [4096]byte
         |    val bp: *byte = &buf
         |    val n = tfs_read(ino, bp, 0, $BS)
         |    if n == $BS
         |        putchar(78)
         |    putchar(bp[0])
         |    putchar(bp[${BS - 1}])
         |    0
         |""".stripMargin,
      prefill = s"""/etc/x file "$content"\n""",
      sectors = 128,
    )
    output shouldBe "SNXX"
  }

  "tfs_read file spanning two blocks" in {
    val content = "A" * BS + "B"
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/etc/x")}
         |    val ino = tfs_lookup(&p, p_len)
         |    var stat: [7]int
         |    tfs_stat(ino, &stat)
         |    if stat[4] == ${BS + 1}
         |        putchar(83)
         |    var buf: [4]byte
         |    val bp: *byte = &buf
         |    tfs_read(ino, bp, ${BS - 1}, 2)
         |    putchar(bp[0])
         |    putchar(bp[1])
         |    0
         |""".stripMargin,
      prefill = s"""/etc/x file "$content"\n""",
      sectors = 128,
    )
    output shouldBe "SAB"
  }

  "tfs_read file with first byte in indirect" in {
    val content = "A" * (6 * BS) + "Z"
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/etc/x")}
         |    val ino = tfs_lookup(&p, p_len)
         |    var stat: [7]int
         |    tfs_stat(ino, &stat)
         |    if stat[4] == ${6 * BS + 1}
         |        putchar(83)
         |    var buf: [4]byte
         |    val bp: *byte = &buf
         |    tfs_read(ino, bp, ${6 * BS}, 1)
         |    putchar(bp[0])
         |    0
         |""".stripMargin,
      prefill = s"""/etc/x file "$content"\n""",
      sectors = 128,
    )
    output shouldBe "SZ"
  }

  // ===== Multiple sequential reads across indirect boundary =====

  "tfs_read sequential reads across indirect boundary" in {
    val prefill = patternPrefill(7 * BS)
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/big")}
         |    val ino = tfs_lookup(&p, p_len)
         |    var buf: [4]byte
         |    val bp: *byte = &buf
         |    tfs_read(ino, bp, ${6 * BS - 100}, 1)
         |    putchar(bp[0])
         |    tfs_read(ino, bp, ${6 * BS - 50}, 1)
         |    putchar(bp[0])
         |    tfs_read(ino, bp, ${6 * BS + 100}, 1)
         |    putchar(bp[0])
         |    tfs_read(ino, bp, ${6 * BS + 200}, 1)
         |    putchar(bp[0])
         |    0
         |""".stripMargin,
      prefill = prefill,
      sectors = 128,
    )
    output shouldBe "FFGG"
  }

  // ===== Empty file edge cases =====

  "tfs_read empty file returns 0" in {
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "empty")}
         |    val ino = tfs_create(1, &name, name_len, 1, 0x1A4)
         |    var buf: [4]byte
         |    val n = tfs_read(ino, &buf, 0, 4)
         |    if n == 0
         |        putchar(89)
         |    else
         |        putchar(78)
         |    0
         |""".stripMargin,
      prefill = "/tmp dir\n",
      sectors = 128,
    )
    output shouldBe "Y"
  }

  "tfs_write then read single byte" in {
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("name", "tiny")}
         |    val ino = tfs_create(1, &name, name_len, 1, 0x1A4)
         |    var data: [1]byte
         |    data[0] = 42
         |    tfs_write(ino, &data, 0, 1)
         |    var buf: [4]byte
         |    val bp: *byte = &buf
         |    val n = tfs_read(ino, bp, 0, 1)
         |    if n == 1
         |        putchar(78)
         |    if bp[0] == 42
         |        putchar(89)
         |    0
         |""".stripMargin,
      prefill = "/tmp dir\n",
      sectors = 128,
    )
    output shouldBe "NY"
  }

  // ===== get_file_block regression: the missing-return fix =====

  "get_file_block returns correct block for indirect index (regression)" in {
    val prefill = patternPrefill(7 * BS)
    val (cpu, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/big")}
         |    val ino = tfs_lookup(&p, p_len)
         |    var buf: [4096]byte
         |    val bp: *byte = &buf
         |    var blk = 0
         |    while blk < 7
         |        val n = tfs_read(ino, bp, blk * $BS, 1)
         |        if n != 1
         |            putchar(63)
         |            return 1
         |        val expected = 65 + blk
         |        if int(bp[0]) != expected
         |            putchar(88)
         |            return 1
         |        putchar(bp[0])
         |        blk += 1
         |    0
         |""".stripMargin,
      prefill = prefill,
      sectors = 128,
    )
    output shouldBe "ABCDEFG"
    cpu.state shouldBe State.Halt
  }
}
