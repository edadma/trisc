package io.github.edadma.trisc

import java.io.{ByteArrayOutputStream, InputStream}
import java.nio.charset.StandardCharsets
import scala.io.{Codec, Source}
import scala.util.Try

// TRB v1 binaries for the demo ramdisk /bin (hello, echo, cat).
// Prefers classpath resources ramdisk/bin/<name>.trb. Falls back to ramdisk/bin/<name>.tof (UTF-8
// text TOF for migration). If both missing, compiles from oskit/bin sources (emits TRB).
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
        |SYMBOL _heap_end = 0xCC000
        |ENTRY main
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

  private lazy val ulibTangled: String = tangledLsysl("oskit/ulib/ulib.lsysl")

  def compileExecutable(unitPath: String, lsyslRepoPath: String): Array[Byte] =
    val syscallAsm =
      Source.fromFile("oskit/ulib/syscall.asm")(using Codec.UTF8).mkString
    val syscallTof = assemble(syscallAsm, relocatable = true)
    val source = tangledLsysl(lsyslRepoPath)
    val allSources = Map(unitPath -> source, "oskit/ulib/ulib" -> ulibTangled)
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
      "/bin/hello" -> compileExecutable("oskit/bin/hello/hello", "oskit/bin/hello.lsysl"),
      "/bin/echo"  -> compileExecutable("oskit/bin/echo/echo", "oskit/bin/echo.lsysl"),
      "/bin/cat"   -> compileExecutable("oskit/bin/cat/cat", "oskit/bin/cat.lsysl"),
    )

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

  def loadForRamdisk(): Map[String, Array[Byte]] =
    val triples = Seq(
      ("hello", "oskit/bin/hello/hello", "oskit/bin/hello.lsysl"),
      ("echo", "oskit/bin/echo/echo", "oskit/bin/echo.lsysl"),
      ("cat", "oskit/bin/cat/cat", "oskit/bin/cat.lsysl"),
    )
    triples.flatMap { case (short, unitPath, lsysl) =>
      loadResource(short)
        .orElse(Try(compileExecutable(unitPath, lsysl)).toOption)
        .map(bytes => s"/bin/$short" -> bytes)
    }.toMap

end RamdiskBinPrograms
