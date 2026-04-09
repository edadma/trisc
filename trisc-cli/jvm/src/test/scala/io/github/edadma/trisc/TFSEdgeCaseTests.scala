package io.github.edadma.trisc

/** Edge-case tests for TFS sysl-side filesystem operations.
  *
  * Block size = 512 bytes. NUM_DIRECT = 6, so files > 3072 bytes
  * use indirect blocks. These tests exercise the boundary between
  * direct and indirect block pointers, cross-block reads/writes,
  * and large file operations.
  */
class TFSEdgeCaseTests extends TFSTestHelpers {

  /** Prefill a file at /big with `size` bytes of repeated ASCII chars.
    * Each block gets a different letter so we can verify which block was read.
    * Pattern: bytes 0..511 = 'A', 512..1023 = 'B', 1024..1535 = 'C', etc.
    */
  private def patternPrefill(size: Int): String =
    val sb = new StringBuilder
    for i <- 0 until size do
      sb += ('A' + i / 512).toChar
    s"""/big file "$sb"\n"""

  // ===== Read: direct block boundary (6 blocks = 3072 bytes) =====

  "tfs_read file filling all 6 direct blocks" in {
    val prefill = patternPrefill(3072)
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
         |    tfs_read(ino, bp, 512, 1)
         |    putchar(bp[0])
         |    tfs_read(ino, bp, 1024, 1)
         |    putchar(bp[0])
         |    tfs_read(ino, bp, 1536, 1)
         |    putchar(bp[0])
         |    tfs_read(ino, bp, 2048, 1)
         |    putchar(bp[0])
         |    tfs_read(ino, bp, 2560, 1)
         |    putchar(bp[0])
         |    var stat: [7]int
         |    tfs_stat(ino, &stat)
         |    if stat[4] == 3072
         |        putchar(33)
         |    0
         |""".stripMargin,
      prefill = prefill,
      sectors = 128,
    )
    output shouldBe "ABCDEF!"
    cpu.state shouldBe State.Halt
  }

  // ===== Read: first indirect block (7 blocks = 3584 bytes) =====

  "tfs_read file with 7 blocks (first indirect)" in {
    val prefill = patternPrefill(3584)
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
         |    tfs_read(ino, bp, 2560, 1)
         |    putchar(bp[0])
         |    tfs_read(ino, bp, 3072, 1)
         |    putchar(bp[0])
         |    var stat: [7]int
         |    tfs_stat(ino, &stat)
         |    if stat[4] == 3584
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
    val prefill = patternPrefill(5120)
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
         |    tfs_read(ino, bp, 2560, 1)
         |    putchar(bp[0])
         |    tfs_read(ino, bp, 3072, 1)
         |    putchar(bp[0])
         |    tfs_read(ino, bp, 4608, 1)
         |    putchar(bp[0])
         |    var stat: [7]int
         |    tfs_stat(ino, &stat)
         |    if stat[4] == 5120
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
    val prefill = patternPrefill(1024)
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/big")}
         |    val ino = tfs_lookup(&p, p_len)
         |    var buf: [8]byte
         |    val n = tfs_read(ino, &buf, 510, 4)
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
    val prefill = patternPrefill(3584)
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/big")}
         |    val ino = tfs_lookup(&p, p_len)
         |    var buf: [8]byte
         |    val n = tfs_read(ino, &buf, 3070, 4)
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
    val prefill = patternPrefill(4096)
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/big")}
         |    val ino = tfs_lookup(&p, p_len)
         |    var buf: [8]byte
         |    val n = tfs_read(ino, &buf, 3582, 4)
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
    val prefill = patternPrefill(1024)
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/big")}
         |    val ino = tfs_lookup(&p, p_len)
         |    var buf: [4]byte
         |    val n = tfs_read(ino, &buf, 512, 2)
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
    val prefill = patternPrefill(3584)
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/big")}
         |    val ino = tfs_lookup(&p, p_len)
         |    var buf: [4]byte
         |    val n = tfs_read(ino, &buf, 3072, 2)
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
    val prefill = patternPrefill(3584)
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/big")}
         |    val ino = tfs_lookup(&p, p_len)
         |    var buf: [512]byte
         |    val bp: *byte = &buf
         |    var offset = 0
         |    while offset < 3584
         |        val n = tfs_read(ino, bp, offset, 512)
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
    val prefill = patternPrefill(3200)
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/big")}
         |    val ino = tfs_lookup(&p, p_len)
         |    var stat: [7]int
         |    tfs_stat(ino, &stat)
         |    if stat[4] == 3200
         |        putchar(83)
         |    var buf: [512]byte
         |    val bp: *byte = &buf
         |    val n = tfs_read(ino, bp, 3072, 512)
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
    val prefill = patternPrefill(3584)
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/big")}
         |    val ino = tfs_lookup(&p, p_len)
         |    var buf: [4]byte
         |    val n = tfs_read(ino, &buf, 4000, 10)
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
    val prefill = patternPrefill(3584)
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/big")}
         |    val ino = tfs_lookup(&p, p_len)
         |    var buf: [1024]byte
         |    val n = tfs_read(ino, &buf, 3072, 1024)
         |    if n == 512
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
         |    var data: [512]byte
         |    val dp: *byte = &data
         |    var rbuf: [4]byte
         |    val rp: *byte = &rbuf
         |    var offset = 0
         |    var ch = 65
         |    while offset < 3584
         |        var j = 0
         |        while j < 512
         |            dp[j] = byte(ch)
         |            j += 1
         |        var count = 512
         |        if offset + count > 3584
         |            count = 3584 - offset
         |        tfs_write(ino, dp, offset, count)
         |        offset += count
         |        ch += 1
         |    var stat: [7]int
         |    tfs_stat(ino, &stat)
         |    if stat[4] == 3584
         |        putchar(83)
         |    tfs_read(ino, rp, 0, 1)
         |    putchar(rp[0])
         |    tfs_read(ino, rp, 2560, 1)
         |    putchar(rp[0])
         |    tfs_read(ino, rp, 3072, 1)
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
         |    var data: [512]byte
         |    val dp: *byte = &data
         |    var offset = 0
         |    while offset < 3584
         |        var j = 0
         |        while j < 512
         |            dp[j] = 65
         |            j += 1
         |        var count = 512
         |        if offset + count > 3584
         |            count = 3584 - offset
         |        tfs_write(ino, dp, offset, count)
         |        offset += count
         |    dp[0] = 90
         |    tfs_write(ino, dp, 3100, 1)
         |    var rbuf: [4]byte
         |    val rp: *byte = &rbuf
         |    tfs_read(ino, rp, 3100, 1)
         |    putchar(rp[0])
         |    tfs_read(ino, rp, 3099, 1)
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
         |    var data: [512]byte
         |    val dp: *byte = &data
         |    var j = 0
         |    while j < 512
         |        dp[j] = 65
         |        j += 1
         |    var offset = 0
         |    while offset < 3072
         |        tfs_write(ino, dp, offset, 512)
         |        offset += 512
         |    j = 0
         |    while j < 512
         |        dp[j] = 90
         |        j += 1
         |    tfs_write(ino, dp, 3072, 512)
         |    var rbuf: [4]byte
         |    val rp: *byte = &rbuf
         |    tfs_read(ino, rp, 3071, 1)
         |    putchar(rp[0])
         |    tfs_read(ino, rp, 3072, 1)
         |    putchar(rp[0])
         |    var stat: [7]int
         |    tfs_stat(ino, &stat)
         |    if stat[4] == 3584
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
    val prefill = patternPrefill(3584)
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/big")}
         |    val ino = tfs_lookup(&p, p_len)
         |    tfs_truncate(ino, 1024)
         |    var stat: [7]int
         |    tfs_stat(ino, &stat)
         |    if stat[4] == 1024
         |        putchar(83)
         |    var buf: [4]byte
         |    val bp: *byte = &buf
         |    tfs_read(ino, bp, 0, 1)
         |    putchar(bp[0])
         |    tfs_read(ino, bp, 512, 1)
         |    putchar(bp[0])
         |    val n = tfs_read(ino, bp, 2000, 1)
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
    val prefill = patternPrefill(3584)
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
    val prefill = patternPrefill(3584)
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

  "tfs_read file of exactly 512 bytes (1 block)" in {
    val content = "X" * 512
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/etc/x")}
         |    val ino = tfs_lookup(&p, p_len)
         |    var stat: [7]int
         |    tfs_stat(ino, &stat)
         |    if stat[4] == 512
         |        putchar(83)
         |    var buf: [512]byte
         |    val bp: *byte = &buf
         |    val n = tfs_read(ino, bp, 0, 512)
         |    if n == 512
         |        putchar(78)
         |    putchar(bp[0])
         |    putchar(bp[511])
         |    0
         |""".stripMargin,
      prefill = s"""/etc/x file "$content"\n""",
      sectors = 128,
    )
    output shouldBe "SNXX"
  }

  "tfs_read file of exactly 513 bytes (2 blocks)" in {
    val content = "A" * 512 + "B"
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/etc/x")}
         |    val ino = tfs_lookup(&p, p_len)
         |    var stat: [7]int
         |    tfs_stat(ino, &stat)
         |    if stat[4] == 513
         |        putchar(83)
         |    var buf: [4]byte
         |    val bp: *byte = &buf
         |    tfs_read(ino, bp, 511, 2)
         |    putchar(bp[0])
         |    putchar(bp[1])
         |    0
         |""".stripMargin,
      prefill = s"""/etc/x file "$content"\n""",
      sectors = 128,
    )
    output shouldBe "SAB"
  }

  "tfs_read file of exactly 3073 bytes (first byte in indirect)" in {
    val content = "A" * 3072 + "Z"
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/etc/x")}
         |    val ino = tfs_lookup(&p, p_len)
         |    var stat: [7]int
         |    tfs_stat(ino, &stat)
         |    if stat[4] == 3073
         |        putchar(83)
         |    var buf: [4]byte
         |    val bp: *byte = &buf
         |    tfs_read(ino, bp, 3072, 1)
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
    val prefill = patternPrefill(3584)
    val (_, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/big")}
         |    val ino = tfs_lookup(&p, p_len)
         |    var buf: [4]byte
         |    val bp: *byte = &buf
         |    tfs_read(ino, bp, 3000, 1)
         |    putchar(bp[0])
         |    tfs_read(ino, bp, 3050, 1)
         |    putchar(bp[0])
         |    tfs_read(ino, bp, 3100, 1)
         |    putchar(bp[0])
         |    tfs_read(ino, bp, 3500, 1)
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
    val prefill = patternPrefill(3584)
    val (cpu, output) = runTFS(
      s"""import oskit.fs.*
         |main() -> int
         |    tfs_init()
         |${syslBytes("p", "/big")}
         |    val ino = tfs_lookup(&p, p_len)
         |    var buf: [512]byte
         |    val bp: *byte = &buf
         |    var blk = 0
         |    while blk < 7
         |        val n = tfs_read(ino, bp, blk * 512, 1)
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
