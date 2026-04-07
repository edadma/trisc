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
        |    // Just check it returns without faulting
        |    putchar(65)
        |    0
        |""".stripMargin))
    info(s"Output: '$out', code: $code")
    code shouldBe 0
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
