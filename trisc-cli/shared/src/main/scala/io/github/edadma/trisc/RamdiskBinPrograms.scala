package io.github.edadma.trisc

import java.io.{ByteArrayOutputStream, InputStream}
import java.nio.charset.StandardCharsets
import scala.io.{Codec, Source}
import scala.util.Try

// TRB v1 binaries for the demo ramdisk /bin (hello, echo, cat).
// When oskit sources are available (typical: run sbt from the trisc repo root), compiles from
// oskit/bin/*.lsysl so the demo tracks edits without a separate regen step. Otherwise loads
// ramdisk/bin/<name>.trb (or legacy .tof) from the classpath for JAR-only use.
object RamdiskBinPrograms:

  private val utf8 = StandardCharsets.UTF_8

  private lazy val progScript: LinkerScript =
    LinkerScriptParser.parse(
      """SECTIONS
        |    code: 0xD0000
        |    rodata
        |    data
        |    bss
        |SYMBOL _heap_start = AFTER bss
        |SYMBOL _heap_end = 0x100000
        |ENTRY _start
        |""".stripMargin,
    ) match
      case Right(s) => s
      case Left(e)  => throw new RuntimeException(s"RamdiskBinPrograms: linker script parse: $e")

  private def readStreamFully(in: InputStream): Array[Byte] =
    val out = new ByteArrayOutputStream()
    val buf = new Array[Byte](8192)
    var n = in.read(buf)
    while n != -1 do
      out.write(buf, 0, n)
      n = in.read(buf)
    out.toByteArray

  private def tangledLsysl(repoRelativePath: String): String =
    val raw = Source.fromFile(repoRelativePath)(using Codec.UTF8).mkString
    val doc = new LiterateParser().parse(raw)
    LiterateRenderer.tangle(doc)

  def compileExecutable(unitPath: String, lsyslRepoPath: String): Array[Byte] =
    val syscallAsm =
      Source.fromFile("oskit/ulib/syscall.asm")(using Codec.UTF8).mkString
    val syscallTof = assemble(syscallAsm, relocatable = true)
    val source     = tangledLsysl(lsyslRepoPath)
    val ulibSource = tangledLsysl("oskit/ulib/ulib.lsysl")
    val srt0Source = tangledLsysl("oskit/ulib/srt0.lsysl")
    val sbrkSource = Source.fromFile("oskit/ulib/sbrk.sysl")(using Codec.UTF8).mkString
    val allocSource = Source.fromFile("posix/stdlib/alloc.sysl")(using Codec.UTF8).mkString
    val stringSource = Source.fromFile("posix/string/string.sysl")(using Codec.UTF8).mkString
    val ctypeSource = Source.fromFile("posix/ctype/ctype.sysl")(using Codec.UTF8).mkString
    val dsClientSource = tangledLsysl("oskit/ds/client.lsysl")
    val allSources = Map(
      unitPath -> source,
      "oskit/ulib/ulib" -> ulibSource,
      "oskit/ulib/srt0" -> srt0Source,
      "posix/unistd/sbrk" -> sbrkSource,
      "posix/stdlib/alloc" -> allocSource,
      "posix/string/string" -> stringSource,
      "posix/ctype/ctype" -> ctypeSource,
      "oskit/ds/client" -> dsClientSource,
    )
    val driver = new SyslDriver
    val result = driver.compile(allSources)
    val codegen = new SyslTriscCodegen
    val tofs = for unit <- result.units yield
      val asm = codegen.generate(unit.typed)
      assemble(asm, relocatable = true)
    val syslTof = Linker.link(tofs, relocatable = true)
    val linked = Linker.link(Seq(syscallTof, syslTof), progScript, 0)
    TriscBinary.serialize(linked)

  // All demo /bin programs; for regenerating embedded ramdisk/bin/*.trb resources.
  def compileAllEmbeddedBinaries(): Map[String, Array[Byte]] =
    Map(
      "/bin/hello"  -> compileExecutable("oskit/bin/hello/hello", "oskit/bin/hello.lsysl"),
      "/bin/echo"   -> compileExecutable("oskit/bin/echo/echo", "oskit/bin/echo.lsysl"),
      "/bin/cat"    -> compileExecutable("oskit/bin/cat/cat", "oskit/bin/cat.lsysl"),
      "/bin/ps"     -> compileExecutable("oskit/bin/ps/ps", "oskit/bin/ps.lsysl"),
      "/bin/count"  -> compileExecutable("oskit/bin/count/count", "oskit/bin/count.lsysl"),
      "/bin/grep"   -> compileExecutable("oskit/bin/grep/grep", "oskit/bin/grep.lsysl"),
      "/bin/wc"     -> compileExecutable("oskit/bin/wc/wc", "oskit/bin/wc.lsysl"),
      "/bin/ls"     -> compileExecutable("oskit/bin/ls/ls", "oskit/bin/ls.lsysl"),
      "/bin/touch"  -> compileExecutable("oskit/bin/touch/touch", "oskit/bin/touch.lsysl"),
      "/bin/write"  -> compileExecutable("oskit/bin/write/write", "oskit/bin/write.lsysl"),
      "/bin/mkdir"  -> compileExecutable("oskit/bin/mkdir/mkdir", "oskit/bin/mkdir.lsysl"),
      "/bin/rm"     -> compileExecutable("oskit/bin/rm/rm", "oskit/bin/rm.lsysl"),
      "/bin/rmdir"  -> compileExecutable("oskit/bin/rmdir/rmdir", "oskit/bin/rmdir.lsysl"),
      "/bin/mv"     -> compileExecutable("oskit/bin/mv/mv", "oskit/bin/mv.lsysl"),
      "/bin/chmod"  -> compileExecutable("oskit/bin/chmod/chmod", "oskit/bin/chmod.lsysl"),
      "/bin/stat"   -> compileExecutable("oskit/bin/stat/stat", "oskit/bin/stat.lsysl"),
      "/bin/uptime" -> compileExecutable("oskit/bin/uptime/uptime", "oskit/bin/uptime.lsysl"),
      "/bin/whoami" -> compileExecutable("oskit/bin/whoami/whoami", "oskit/bin/whoami.lsysl"),
      "/bin/nsh"    -> compileExecutable("oskit/bin/nsh/nsh", "oskit/bin/nsh.lsysl"),
      "/bin/head"   -> compileExecutable("oskit/bin/head/head", "oskit/bin/head.lsysl"),
      "/bin/tail"   -> compileExecutable("oskit/bin/tail/tail", "oskit/bin/tail.lsysl"),
      "/bin/login"  -> compileLoginExecutable(),
      "/bin/su"     -> compileCryptoExecutable("oskit/bin/su/su", "oskit/bin/su.lsysl"),
    )

  /** Compile a program that needs crypto libraries (sha256, hmac, pbkdf2). */
  def compileCryptoExecutable(unitPath: String, lsyslRepoPath: String): Array[Byte] =
    val syscallAsm =
      Source.fromFile("oskit/ulib/syscall.asm")(using Codec.UTF8).mkString
    val syscallTof = assemble(syscallAsm, relocatable = true)
    val source     = tangledLsysl(lsyslRepoPath)
    val ulibSource = tangledLsysl("oskit/ulib/ulib.lsysl")
    val srt0Source = tangledLsysl("oskit/ulib/srt0.lsysl")
    val sbrkSource = Source.fromFile("oskit/ulib/sbrk.sysl")(using Codec.UTF8).mkString
    val allocSource = Source.fromFile("posix/stdlib/alloc.sysl")(using Codec.UTF8).mkString
    val stringSource = Source.fromFile("posix/string/string.sysl")(using Codec.UTF8).mkString
    val ctypeSource = Source.fromFile("posix/ctype/ctype.sysl")(using Codec.UTF8).mkString
    val sha256Source = tangledLsysl("std/crypto/sha256/sha256.lsysl")
    val hmacSource = tangledLsysl("std/crypto/hmac/hmac.lsysl")
    val pbkdf2Source = tangledLsysl("std/crypto/pbkdf2/pbkdf2.lsysl")
    val binarySource = tangledLsysl("std/encoding/binary/binary.lsysl")
    val memSource = tangledLsysl("std/mem/mem.lsysl")
    val debugSource = tangledLsysl("std/debug/debug.lsysl")
    val allSources = Map(
      unitPath -> source,
      "oskit/ulib/ulib" -> ulibSource,
      "oskit/ulib/srt0" -> srt0Source,
      "posix/unistd/sbrk" -> sbrkSource,
      "posix/stdlib/alloc" -> allocSource,
      "posix/string/string" -> stringSource,
      "posix/ctype/ctype" -> ctypeSource,
      "std/crypto/sha256/sha256" -> sha256Source,
      "std/crypto/hmac/hmac" -> hmacSource,
      "std/crypto/pbkdf2/pbkdf2" -> pbkdf2Source,
      "std/encoding/binary/binary" -> binarySource,
      "std/mem/mem" -> memSource,
      "std/debug/debug" -> debugSource,
    )
    val driver = new SyslDriver
    val result = driver.compile(allSources)
    val codegen = new SyslTriscCodegen
    val tofs = for unit <- result.units yield
      val asm = codegen.generate(unit.typed)
      assemble(asm, relocatable = true)
    val syslTof = Linker.link(tofs, relocatable = true)
    val linked = Linker.link(Seq(syscallTof, syslTof), progScript, 0)
    TriscBinary.serialize(linked)

  def compileLoginExecutable(): Array[Byte] =
    compileCryptoExecutable("oskit/bin/login/login", "oskit/bin/login.lsysl")

  private def loadResourceStream(path: String): Option[Array[Byte]] =
    val inOpt =
      Option(RamdiskBinPrograms.getClass.getResourceAsStream("/" + path))
        .orElse(Option(RamdiskBinPrograms.getClass.getResourceAsStream(path)))
    inOpt match
      case None => None
      case Some(in) =>
        try Some(readStreamFully(in))
        finally in.close()

  private def loadResource(short: String): Option[Array[Byte]] =
    loadResourceStream(s"ramdisk/bin/$short.trb")
      .orElse(loadResourceStream(s"ramdisk/bin/$short.tof").map { textBytes =>
        val linked = TOF.deserialize(String(textBytes, utf8))
        TriscBinary.serialize(linked)
      })

  // Load pre-built .trb resources only (no compilation). Used by the emulator at runtime.
  // Run RegenRamdiskBinMain to update embedded .trb files after editing oskit/bin or ulib.
  def loadEmbeddedBinaries(): Map[String, Array[Byte]] =
    Seq("hello", "echo", "cat", "ps", "count", "grep", "wc", "ls", "touch", "write", "mkdir", "rm", "rmdir", "mv", "chmod", "stat", "uptime", "whoami", "nsh", "head", "tail", "login", "su").flatMap { short =>
      loadResource(short).map(bytes => s"/bin/$short" -> bytes)
    }.toMap

end RamdiskBinPrograms
