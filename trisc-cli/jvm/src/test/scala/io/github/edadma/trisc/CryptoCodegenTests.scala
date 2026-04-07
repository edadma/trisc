package io.github.edadma.trisc

import javax.crypto.SecretKeyFactory
import javax.crypto.spec.PBEKeySpec

class CryptoCodegenTests extends SyslCodegenHelpers {

  private def readLsysl(path: String): String =
    val raw = scala.io.Source.fromFile(path).mkString
    val doc = new LiterateParser().parse(raw)
    LiterateRenderer.tangle(doc)

  private lazy val debugSysl  = readLsysl("std/debug/debug.lsysl")
  private lazy val memSysl    = readLsysl("std/mem/mem.lsysl")
  private lazy val sha256Sysl = readLsysl("std/crypto/sha256/sha256.lsysl")
  private lazy val hmacSysl   = readLsysl("std/crypto/hmac/hmac.lsysl")
  private lazy val pbkdf2Sysl = readLsysl("std/crypto/pbkdf2/pbkdf2.lsysl")

  private lazy val allocSource  = scala.io.Source.fromFile("posix/stdlib/alloc.sysl").mkString
  private lazy val stringSource = scala.io.Source.fromFile("posix/string/string.sysl").mkString
  private lazy val ctypeSource  = scala.io.Source.fromFile("posix/ctype/ctype.sysl").mkString

  private def sbrkModule(heapSize: Int = 65536): String =
    s"""module posix.unistd
       |
       |var _heap: [$heapSize]i8
       |var _brk: *i8 = *i8(0)
       |var _brk_initialized = false
       |
       |sbrk(increment: int) -> *i8
       |    if !_brk_initialized
       |        _brk = &_heap
       |        _brk_initialized = true
       |    if increment == 0 then return _brk
       |    val old_brk = _brk
       |    val new_brk = old_brk + increment
       |    if i64(new_brk) > i64(&_heap + $heapSize) then return *i8(-1)
       |    _brk = new_brk
       |    old_brk
       |""".stripMargin

  private def cryptoSources(mainSource: String): Map[String, String] = Map(
    "posix/unistd/sbrk"          -> sbrkModule(),
    "posix/string/string"        -> stringSource,
    "posix/ctype/ctype"          -> ctypeSource,
    "posix/stdlib/alloc"         -> allocSource,
    "std/debug/debug"            -> debugSysl,
    "std/mem/mem"                -> memSysl,
    "std/crypto/sha256/sha256"   -> sha256Sysl,
    "std/crypto/hmac/hmac"       -> hmacSysl,
    "std/crypto/pbkdf2/pbkdf2"   -> pbkdf2Sysl,
    "main"                       -> mainSource,
  )

  private def compileMultiAndRunOutput(sources: Map[String, String], maxCycles: Int = 5000000): (Long, String) =
    val driver = new SyslDriver
    val result = driver.compile(sources)
    val codegen = new SyslTriscCodegen
    val tofs = for unit <- result.units yield
      val asm = codegen.generate(unit.typed)
      assemble(asm, relocatable = true)
    val linked = Linker.link(Seq(Runtime.bootTof) ++ tofs ++ Seq(Runtime.ioTof))
    val output = new StringBuilder
    val stdout = new Stdout(Runtime.stdoutAddress, s => output ++= s)
    val ram = new RAM(0, Runtime.stdoutAddress.toInt)
    val mem = new Memory("Memory", ram, stdout)
    linked.load(mem)
    val cpu = new CPU(mem) { this.limit = maxCycles }
    cpu.reset()
    cpu.run()
    (cpu.r(1).read, output.toString)

  private def jvmPbkdf2(password: String, salt: String, iterations: Int): String =
    val spec = new PBEKeySpec(password.toCharArray, salt.getBytes("UTF-8"), iterations, 256)
    val hash = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256").generateSecret(spec).getEncoded
    hash.map(b => f"${b & 0xff}%02x").mkString

  "basic: print before crypto call" in {
    val (code, out) = compileMultiAndRunOutput(maxCycles = 20000000, sources = cryptoSources(
      """import posix.stdlib.*
        |
        |main() -> int
        |    putchar(65)
        |    putchar(10)
        |    0
        |""".stripMargin))
    info(s"Output: '$out', code: $code")
    out shouldBe "A\n"
  }

  "basic: u32 rotr" in {
    val (code, out) = compileMultiAndRunOutput(maxCycles = 20000000, sources = cryptoSources(
      """import std.crypto.sha256.*
        |import posix.stdlib.*
        |
        |main() -> int
        |    val x = 0x6A09E667u32
        |    val r = rotr(x, 2u32)
        |    putchar(65)
        |    0
        |""".stripMargin))
    info(s"Output: '$out', code: $code")
    code shouldBe 0
  }

  "basic: u32 array init and index" in {
    val (code, out) = compileMultiAndRunOutput(maxCycles = 20000000, sources = cryptoSources(
      """import posix.stdlib.*
        |
        |main() -> int
        |    val arr: [4]u32 = [0x6A09E667u32, 0xBB67AE85u32, 0x3C6EF372u32, 0xA54FF53Au32]
        |    putchar(65)
        |    if arr[0] == 0x6A09E667u32
        |        putchar(66)
        |    0
        |""".stripMargin))
    info(s"Output: '$out', code: $code")
    out should include("AB")
  }

  "basic: for-in range loop" in {
    val (code, out) = compileMultiAndRunOutput(maxCycles = 20000000, sources = cryptoSources(
      """import posix.stdlib.*
        |
        |main() -> int
        |    var sum = 0
        |    for i in 0..<4
        |        sum += 1
        |    putchar(48 + sum)
        |    0
        |""".stripMargin))
    info(s"Output: '$out', code: $code")
    out shouldBe "4"
  }

  "basic: slice from array" in {
    val (code, out) = compileMultiAndRunOutput(maxCycles = 20000000, sources = cryptoSources(
      """import posix.stdlib.*
        |
        |sum_slice(s: []byte) -> int
        |    var total = 0
        |    for var i = 0; i < len(s); i++
        |        total += int(s[i])
        |    total
        |
        |main() -> int
        |    val data: [3]byte = "abc"
        |    val result = sum_slice(data[:])
        |    putchar(48 + result / 100)
        |    0
        |""".stripMargin))
    info(s"Output: '$out', code: $code")
    code shouldBe 0
  }

  "basic: K table access" in {
    val (code, out) = compileMultiAndRunOutput(maxCycles = 20000000, sources = cryptoSources(
      """import std.crypto.sha256.*
        |import posix.stdlib.*
        |
        |main() -> int
        |    // Access K[0] = 0x428a2f98
        |    if K[0] == 0x428a2f98u32
        |        putchar(65)
        |    else
        |        putchar(78)
        |    // Access K[63] = 0xc67178f2
        |    if K[63] == 0xc67178f2u32
        |        putchar(66)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin))
    info(s"Output: '$out', code: $code")
    out shouldBe "AB"
  }

  "basic: large u32 local array" in {
    val (code, out) = compileMultiAndRunOutput(maxCycles = 20000000, sources = cryptoSources(
      """import posix.stdlib.*
        |
        |main() -> int
        |    var w: [64]u32
        |    w[0] = 0x12345678u32
        |    w[63] = 0xDEADBEEFu32
        |    if w[0] == 0x12345678u32
        |        putchar(65)
        |    if w[63] == 0xDEADBEEFu32
        |        putchar(66)
        |    0
        |""".stripMargin))
    info(s"Output: '$out', code: $code")
    out shouldBe "AB"
  }

  "basic: u32 wrapping add" in {
    val (code, out) = compileMultiAndRunOutput(maxCycles = 20000000, sources = cryptoSources(
      """import posix.stdlib.*
        |
        |main() -> int
        |    val a = 0xFFFFFFFFu32
        |    val b = 1u32
        |    val c = a + b
        |    // c should wrap to 0
        |    if c == 0u32
        |        putchar(65)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin))
    info(s"Output: '$out', code: $code")
    out shouldBe "A"
  }

  "basic: u32 byte assembly" in {
    val (code, out) = compileMultiAndRunOutput(maxCycles = 20000000, sources = cryptoSources(
      """import posix.stdlib.*
        |
        |main() -> int
        |    var block: [4]byte
        |    block[0] = 0x61u8
        |    block[1] = 0x62u8
        |    block[2] = 0x63u8
        |    block[3] = 0x80u8
        |    // Assemble big-endian u32 like process_block does
        |    val w = (u32(block[0]) << 24u32) | (u32(block[1]) << 16u32) | (u32(block[2]) << 8u32) | u32(block[3])
        |    // w should be 0x61626380
        |    if w == 0x61626380u32
        |        putchar(65)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin))
    info(s"Output: '$out', code: $code")
    out shouldBe "A"
  }

  "basic: u32 slice write and read" in {
    val (code, out) = compileMultiAndRunOutput(maxCycles = 20000000, sources = cryptoSources(
      """import posix.stdlib.*
        |
        |modify(s: []u32)
        |    s[0] = s[0] + 1u32
        |
        |main() -> int
        |    var arr: [4]u32 = [10u32, 20u32, 30u32, 40u32]
        |    modify(arr[:])
        |    if arr[0] == 11u32
        |        putchar(65)
        |    0
        |""".stripMargin))
    info(s"Output: '$out', code: $code")
    out shouldBe "A"
  }

  "basic: byte slice with offset param" in {
    val (code, out) = compileMultiAndRunOutput(maxCycles = 20000000, sources = cryptoSources(
      """import posix.stdlib.*
        |
        |load_word(s: []byte, off: int) -> u32
        |    (u32(s[off]) << 24u32) | (u32(s[off + 1]) << 16u32) | (u32(s[off + 2]) << 8u32) | u32(s[off + 3])
        |
        |main() -> int
        |    var data: [8]byte
        |    data[0] = 0x61u8
        |    data[1] = 0x62u8
        |    data[2] = 0x63u8
        |    data[3] = 0x80u8
        |    data[4] = 0x00u8
        |    data[5] = 0x00u8
        |    data[6] = 0x00u8
        |    data[7] = 0x18u8
        |    val w0 = load_word(data[:], 0)
        |    val w1 = load_word(data[:], 4)
        |    if w0 == 0x61626380u32
        |        putchar(65)
        |    if w1 == 0x00000018u32
        |        putchar(66)
        |    0
        |""".stripMargin))
    info(s"Output: '$out', code: $code")
    out shouldBe "AB"
  }

  "basic: process_block call" in {
    val (code, out) = compileMultiAndRunOutput(maxCycles = 20000000, sources = cryptoSources(
      """import std.crypto.sha256.*
        |import posix.stdlib.*
        |
        |main() -> int
        |    putchar(49)
        |    var state: [8]u32 = [0x6A09E667u32, 0xBB67AE85u32, 0x3C6EF372u32, 0xA54FF53Au32, 0x510E527Fu32, 0x9B05688Cu32, 0x1F83D9ABu32, 0x5BE0CD19u32]
        |    var block: [64]byte
        |    block[0] = 0x61u8
        |    block[1] = 0x62u8
        |    block[2] = 0x63u8
        |    block[3] = 0x80u8
        |    block[56] = 0u8
        |    block[57] = 0u8
        |    block[58] = 0u8
        |    block[59] = 0u8
        |    block[60] = 0u8
        |    block[61] = 0u8
        |    block[62] = 0u8
        |    block[63] = 24u8
        |    putchar(50)
        |    process_block(state[:], block[:], 0)
        |    putchar(51)
        |    0
        |""".stripMargin))
    info(s"Output: '$out', code: $code")
    out should include("123")
  }

  "SHA-256 abc" in {
    val (code, out) = compileMultiAndRunOutput(maxCycles = 20000000, sources = cryptoSources(
      """import std.crypto.sha256.*
        |import posix.stdlib.*
        |
        |main() -> int
        |    val msg: [3]byte = "abc"
        |    var out: [32]byte
        |    sha256(msg[:], out[:])
        |    // Print first 4 bytes as hex
        |    for var i = 0; i < 4; i++
        |        val b = int(out[i])
        |        val hi = b >> 4
        |        val lo = b & 0xF
        |        if hi < 10
        |            putchar(48 + hi)
        |        else
        |            putchar(87 + hi)
        |        if lo < 10
        |            putchar(48 + lo)
        |        else
        |            putchar(87 + lo)
        |    0
        |""".stripMargin))
    info(s"Output: '$out', code: $code")
    out shouldBe "ba7816bf"  // First 4 bytes of SHA-256("abc")
  }

  "HMAC-SHA256 inner hash only" in {
    // Manually compute the inner hash step of HMAC: SHA256(ipad || msg)
    // key = "key" (3 bytes), msg = "msg" (3 bytes)
    // k0 = "key" + 61 zero bytes
    // ipad = k0 XOR 0x36 repeated
    // inner = SHA256(ipad || msg) where ipad is 64 bytes and msg is 3 bytes
    val (code, out) = compileMultiAndRunOutput(maxCycles = 20000000, sources = cryptoSources(
      """import std.crypto.sha256.*
        |import posix.stdlib.*
        |
        |main() -> int
        |    // Build ipad: key XOR 0x36, padded to 64 bytes
        |    var ipad: [67]byte  // 64 + 3 = 67 for ipad || msg
        |    val key: [3]byte = "key"
        |    for var i = 0; i < 3; i++
        |        ipad[i] = key[i] ^ 0x36u8
        |    for var i = 3; i < 64; i++
        |        ipad[i] = 0x36u8
        |    // Append msg
        |    val msg: [3]byte = "msg"
        |    ipad[64] = msg[0]
        |    ipad[65] = msg[1]
        |    ipad[66] = msg[2]
        |    // SHA256(ipad || msg)
        |    var inner: [32]byte
        |    sha256(ipad[:], inner[:])
        |    for var i = 0; i < 4; i++
        |        val b = int(inner[i])
        |        val hi = b >> 4
        |        val lo = b & 0xF
        |        if hi < 10
        |            putchar(48 + hi)
        |        else
        |            putchar(87 + hi)
        |        if lo < 10
        |            putchar(48 + lo)
        |        else
        |            putchar(87 + lo)
        |    0
        |""".stripMargin))
    info(s"Output: '$out', code: $code")
    // Just check it runs and produces something non-zero
    code shouldBe 0
    out.length shouldBe 8
  }

  "HMAC-SHA256 inner hash via dynamic alloc" in {
    val (code, out) = compileMultiAndRunOutput(maxCycles = 20000000, sources = cryptoSources(
      """import std.crypto.sha256.*
        |import posix.stdlib.*
        |
        |main() -> int
        |    val key: [3]byte = "key"
        |    val msg: [3]byte = "msg"
        |    // Build ipad
        |    var ipad: [64]byte
        |    for var i = 0; i < 3; i++
        |        ipad[i] = key[i] ^ 0x36u8
        |    for var i = 3; i < 64; i++
        |        ipad[i] = 0x36u8
        |    // Dynamic buffer like HMAC does
        |    val inner_len = 64 + len(msg[:])
        |    val inner_buf = (new [inner_len]byte)[:]
        |    for i in 0..<64
        |        inner_buf[i] = ipad[i]
        |    for i in 0..<len(msg[:])
        |        inner_buf[64 + i] = msg[i]
        |    var inner: [32]byte
        |    sha256(inner_buf, inner[:])
        |    for var i = 0; i < 4; i++
        |        val b = int(inner[i])
        |        val hi = b >> 4
        |        val lo = b & 0xF
        |        if hi < 10
        |            putchar(48 + hi)
        |        else
        |            putchar(87 + hi)
        |        if lo < 10
        |            putchar(48 + lo)
        |        else
        |            putchar(87 + lo)
        |    0
        |""".stripMargin))
    info(s"Output: '$out', code: $code")
    // Should match the inline version: cb143663
    out shouldBe "cb143663"
  }

  "basic: new array + slice write" in {
    val (code, out) = compileMultiAndRunOutput(maxCycles = 20000000, sources = cryptoSources(
      """import posix.stdlib.*
        |
        |main() -> int
        |    val n = 10
        |    val buf = (new [n]byte)[:]
        |    buf[0] = 65u8
        |    buf[1] = 66u8
        |    putchar(int(buf[0]))
        |    putchar(int(buf[1]))
        |    0
        |""".stripMargin))
    info(s"Output: '$out', code: $code")
    out shouldBe "AB"
  }

  "basic: new array + for-in write" in {
    val (code, out) = compileMultiAndRunOutput(maxCycles = 20000000, sources = cryptoSources(
      """import posix.stdlib.*
        |
        |main() -> int
        |    val n = 67
        |    val buf = (new [n]byte)[:]
        |    for i in 0..<67
        |        buf[i] = 0x36u8
        |    putchar(int(buf[0]) + 29)
        |    putchar(int(buf[66]) + 29)
        |    0
        |""".stripMargin))
    info(s"Output: '$out', code: $code")
    out shouldBe "SS"
  }

  "basic: new array + for-in write then sha256" in {
    val (code, out) = compileMultiAndRunOutput(maxCycles = 20000000, sources = cryptoSources(
      """import std.crypto.sha256.*
        |import posix.stdlib.*
        |
        |main() -> int
        |    val n = 67
        |    val buf = (new [n]byte)[:]
        |    for i in 0..<67
        |        buf[i] = 0x36u8
        |    var hash: [32]byte
        |    sha256(buf, hash[:])
        |    putchar(65)
        |    0
        |""".stripMargin))
    info(s"Output: '$out', code: $code")
    out shouldBe "A"
  }

  "basic: dynamic alloc in function with slice params" in {
    val (code, out) = compileMultiAndRunOutput(maxCycles = 20000000, sources = cryptoSources(
      """import std.crypto.sha256.*
        |import posix.stdlib.*
        |
        |do_hash(key: []byte, msg: []byte, out: []byte)
        |    val inner_len = 64 + len(msg)
        |    val inner_buf = (new [inner_len]byte)[:]
        |    for i in 0..<64
        |        inner_buf[i] = key[i % len(key)] ^ 0x36u8
        |    for i in 0..<len(msg)
        |        inner_buf[64 + i] = msg[i]
        |    sha256(inner_buf, out)
        |
        |main() -> int
        |    val key: [3]byte = "key"
        |    val msg: [3]byte = "msg"
        |    var hash: [32]byte
        |    do_hash(key[:], msg[:], hash[:])
        |    putchar(65)
        |    0
        |""".stripMargin))
    info(s"Output: '$out', code: $code")
    out shouldBe "A"
  }

  "basic: two sha256 calls in one function" in {
    val (code, out) = compileMultiAndRunOutput(maxCycles = 20000000, sources = cryptoSources(
      """import std.crypto.sha256.*
        |import posix.stdlib.*
        |
        |do_hmac(key: []byte, msg: []byte, out: []byte)
        |    // Step 1: build k0 (zero-padded key)
        |    var k0: [64]byte
        |    for var i = 0; i < 64; i++
        |        k0[i] = 0u8
        |    for i in 0..<len(key)
        |        k0[i] = key[i]
        |    // Step 2: ipad/opad
        |    var ipad: [64]byte
        |    var opad: [64]byte
        |    for i in 0..<64
        |        ipad[i] = k0[i] ^ 0x36u8
        |        opad[i] = k0[i] ^ 0x5cu8
        |    // Step 3: inner hash
        |    val inner_len = 64 + len(msg)
        |    val inner_buf = (new [inner_len]byte)[:]
        |    for i in 0..<64
        |        inner_buf[i] = ipad[i]
        |    for i in 0..<len(msg)
        |        inner_buf[64 + i] = msg[i]
        |    var inner_hash: [32]byte
        |    sha256(inner_buf, inner_hash[:])
        |    // Step 4: outer hash
        |    var outer_buf: [96]byte
        |    for i in 0..<64
        |        outer_buf[i] = opad[i]
        |    for i in 0..<32
        |        outer_buf[64 + i] = inner_hash[i]
        |    sha256(outer_buf[:], out)
        |
        |main() -> int
        |    val key: [3]byte = "key"
        |    val msg: [3]byte = "msg"
        |    var hash: [32]byte
        |    do_hmac(key[:], msg[:], hash[:])
        |    for var i = 0; i < 4; i++
        |        val b = int(hash[i])
        |        val hi = b >> 4
        |        val lo = b & 0xF
        |        if hi < 10
        |            putchar(48 + hi)
        |        else
        |            putchar(87 + hi)
        |        if lo < 10
        |            putchar(48 + lo)
        |        else
        |            putchar(87 + lo)
        |    0
        |""".stripMargin))
    info(s"Output: '$out', code: $code")
    out shouldBe "2d93cbc1"
  }

  "basic: check inner hash inside function" in {
    val (code, out) = compileMultiAndRunOutput(maxCycles = 20000000, sources = cryptoSources(
      """import std.crypto.sha256.*
        |import posix.stdlib.*
        |
        |do_inner(key: []byte, msg: []byte, out: []byte)
        |    var k0: [64]byte
        |    for var i = 0; i < 64; i++
        |        k0[i] = 0u8
        |    for i in 0..<len(key)
        |        k0[i] = key[i]
        |    var ipad: [64]byte
        |    for i in 0..<64
        |        ipad[i] = k0[i] ^ 0x36u8
        |    val inner_len = 64 + len(msg)
        |    val inner_buf = (new [inner_len]byte)[:]
        |    for i in 0..<64
        |        inner_buf[i] = ipad[i]
        |    for i in 0..<len(msg)
        |        inner_buf[64 + i] = msg[i]
        |    sha256(inner_buf, out)
        |
        |main() -> int
        |    val key: [3]byte = "key"
        |    val msg: [3]byte = "msg"
        |    var hash: [32]byte
        |    do_inner(key[:], msg[:], hash[:])
        |    for var i = 0; i < 4; i++
        |        val b = int(hash[i])
        |        val hi = b >> 4
        |        val lo = b & 0xF
        |        if hi < 10
        |            putchar(48 + hi)
        |        else
        |            putchar(87 + hi)
        |        if lo < 10
        |            putchar(48 + lo)
        |        else
        |            putchar(87 + lo)
        |    0
        |""".stripMargin))
    info(s"Output: '$out', code: $code")
    // Should match the inline version: cb143663
    out shouldBe "cb143663"
  }

  "HMAC-SHA256 basic" in {
    val (code, out) = compileMultiAndRunOutput(maxCycles = 20000000, sources = cryptoSources(
      """import std.crypto.hmac.*
        |import posix.stdlib.*
        |
        |main() -> int
        |    val key: [3]byte = "key"
        |    val msg: [3]byte = "msg"
        |    var out: [32]byte
        |    hmac_sha256(key[:], msg[:], out[:])
        |    for var i = 0; i < 4; i++
        |        val b = int(out[i])
        |        val hi = b >> 4
        |        val lo = b & 0xF
        |        if hi < 10
        |            putchar(48 + hi)
        |        else
        |            putchar(87 + hi)
        |        if lo < 10
        |            putchar(48 + lo)
        |        else
        |            putchar(87 + lo)
        |    0
        |""".stripMargin))
    info(s"Output: '$out', code: $code")
    // Expected first 4 bytes of HMAC-SHA256("key", "msg") = 2d93cbc1
    out shouldBe "2d93cbc1"
  }

  "PBKDF2 c=1 matches JVM" in {
    val (code, out) = compileMultiAndRunOutput(cryptoSources(
      """import std.crypto.pbkdf2.*
        |import posix.stdlib.*
        |
        |main() -> int
        |    val password: [8]byte = "password"
        |    val salt: [4]byte = "salt"
        |    var out: [32]byte
        |    pbkdf2_hmac_sha256(password[:], salt[:], 1, out[:])
        |    // Print all 32 bytes as hex
        |    for var i = 0; i < 32; i++
        |        val b = int(out[i])
        |        val hi = b >> 4
        |        val lo = b & 0xF
        |        if hi < 10
        |            putchar(48 + hi)
        |        else
        |            putchar(87 + hi)
        |        if lo < 10
        |            putchar(48 + lo)
        |        else
        |            putchar(87 + lo)
        |    0
        |""".stripMargin), maxCycles = 10000000)
    info(s"Output: '$out'")
    out shouldBe jvmPbkdf2("password", "salt", 1)
  }
}
