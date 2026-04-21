package io.github.edadma.trisc

import scala.collection.mutable
import scala.util.chaining.*

object SyslStdlib:
  import Value.*

  // Helper: extract Scala string from any string value
  private def asString(v: Value): String = v match
    case StringVal(bytes) => new String(bytes, "UTF-8")
    case _ => throw RuntimeException(s"expected string, got $v")

  // Helper: build a sysl string from a Scala string (UTF-8 encoded). JVM GC handles lifetime.
  private def mkString(s: String): StringVal =
    StringVal(s.getBytes("UTF-8"))

  // Modules that have JVM runtime implementations (builtins).
  // Metadata is now derived from .lsysl sources on the filesystem.
  val builtinModules: Set[String] = Set("std/io", "std/fs", "std/process", "std/string")

  def builtins(name: String, ctx: StdlibContext): Map[String, List[Value] => Value] = name match
    case "std/io"      => ioBuiltins(ctx)
    case "std/fs"      => fsBuiltins(ctx)
    case "std/process" => processBuiltins(ctx)
    case "std/string"  => stringBuiltins(ctx)
    case _             => throw IllegalArgumentException(s"unknown stdlib module: $name")

  // Shared context for stdlib builtins — holds state like open files, argv, output fn
  class StdlibContext(
      val output: String => Unit = s => print(s),
      val input: () => Int = () => System.in.read(),
      val argv: Array[String] = Array.empty,
  ):
    private val openFiles = new mutable.HashMap[Int, java.io.RandomAccessFile]
    private var nextFd = 3 // 0=stdin, 1=stdout, 2=stderr

    def allocFd(raf: java.io.RandomAccessFile): Int =
      val fd = nextFd
      nextFd += 1
      openFiles(fd) = raf
      fd

    def getFile(fd: Int): Option[java.io.RandomAccessFile] = openFiles.get(fd)

    def closeFile(fd: Int): Boolean =
      openFiles.remove(fd) match
        case Some(raf) => raf.close(); true
        case None      => false

    def closeAll(): Unit =
      for (_, raf) <- openFiles do raf.close()
      openFiles.clear()

  // ── std/io ──────────────────────────────────────────────────────────

  // Open flags
  private val O_RDONLY = 0
  private val O_WRONLY = 1
  private val O_RDWR = 2
  private val O_CREATE = 4
  private val O_TRUNC = 8
  private val O_APPEND = 16

  private val SEEK_SET = 0
  private val SEEK_CUR = 1
  private val SEEK_END = 2

  private def ioBuiltins(ctx: StdlibContext): Map[String, List[Value] => Value] = Map(
    // Constants as "functions" that return their value — will be registered as globals
    "open" -> (args => {
      val path = args.head.pipe(asString)
      val flags = args(1).asInstanceOf[IntVal].n.toInt
      try
        val mode = if (flags & O_RDWR) != 0 then "rw"
        else if (flags & O_WRONLY) != 0 then "rw"
        else "r"
        val file = new java.io.File(path)
        if (flags & O_CREATE) != 0 && !file.exists() then
          file.createNewFile()
        val raf = new java.io.RandomAccessFile(file, mode)
        if (flags & O_TRUNC) != 0 then raf.setLength(0)
        if (flags & O_APPEND) != 0 then raf.seek(raf.length())
        IntVal(ctx.allocFd(raf))
      catch case _: Exception => IntVal(-1)
    }),
    "close" -> (args => {
      val fd = args.head.asInstanceOf[IntVal].n.toInt
      IntVal(if ctx.closeFile(fd) then 0 else -1)
    }),
    "read" -> (args => {
      val fd = args.head.asInstanceOf[IntVal].n.toInt
      val buf = args(1)
      val count = args(2).asInstanceOf[IntVal].n.toInt
      val (cells, off) = buf match
        case ArrVal(c, o) => (c, o)
        case PtrVal(ArrayPtr(c, o)) => (c, o)
        case _ => throw RuntimeException("read: expected array or array pointer for buffer")
      if fd == 0 then
        // stdin
        var bytesRead = 0
        var i = 0
        while i < count do
          val b = ctx.input()
          if b == -1 then
            i = count // break
          else
            cells(off + bytesRead).value = IntVal(b.toLong)
            bytesRead += 1
            i += 1
        IntVal(bytesRead)
      else
        ctx.getFile(fd) match
          case Some(raf) =>
            try
              val bytes = new Array[Byte](count)
              val n = raf.read(bytes)
              if n > 0 then
                for i <- 0 until n do
                  cells(off + i).value = IntVal(bytes(i) & 0xff)
              IntVal(n)
            catch case _: Exception => IntVal(-1)
          case None => IntVal(-1)
    }),
    "write" -> (args => {
      val fd = args.head.asInstanceOf[IntVal].n.toInt
      val buf = args(1)
      val count = args(2).asInstanceOf[IntVal].n.toInt
      val (cells, off) = buf match
        case ArrVal(c, o) => (c, o)
        case PtrVal(ArrayPtr(c, o)) => (c, o)
        case _ => throw RuntimeException("write: expected array or array pointer for buffer")
      val bytes = new Array[Byte](count)
      for i <- 0 until count do
        bytes(i) = cells(off + i).value.asInstanceOf[IntVal].n.toByte
      if fd == 1 || fd == 2 then
        ctx.output(new String(bytes, "UTF-8"))
        IntVal(count)
      else
        ctx.getFile(fd) match
          case Some(raf) =>
            try
              raf.write(bytes)
              IntVal(count)
            catch case _: Exception => IntVal(-1)
          case None => IntVal(-1)
    }),
    "write_str" -> (args => {
      val fd = args.head.asInstanceOf[IntVal].n.toInt
      val s = args(1).pipe(asString)
      if fd == 1 || fd == 2 then
        ctx.output(s)
        IntVal(s.getBytes("UTF-8").length)
      else
        ctx.getFile(fd) match
          case Some(raf) =>
            try
              val bytes = s.getBytes("UTF-8")
              raf.write(bytes)
              IntVal(bytes.length)
            catch case _: Exception => IntVal(-1)
          case None => IntVal(-1)
    }),
    "read_line" -> (args => {
      val fd = args.head.asInstanceOf[IntVal].n.toInt
      if fd == 0 then
        // stdin — read until newline
        val sb = new StringBuilder
        var done = false
        while !done do
          val b = ctx.input()
          if b == -1 || b == '\n' then done = true
          else sb += b.toChar
        mkString(sb.toString)
      else
        ctx.getFile(fd) match
          case Some(raf) =>
            try
              val line = raf.readLine()
              if line == null then mkString("") else mkString(line)
            catch case _: Exception => mkString("")
          case None => mkString("")
    }),
    "seek" -> (args => {
      val fd = args.head.asInstanceOf[IntVal].n.toInt
      val offset = args(1).asInstanceOf[IntVal].n
      val whence = args(2).asInstanceOf[IntVal].n.toInt
      ctx.getFile(fd) match
        case Some(raf) =>
          try
            val pos = whence match
              case SEEK_SET => offset
              case SEEK_CUR => raf.getFilePointer + offset
              case SEEK_END => raf.length() + offset
              case _        => -1L
            if pos >= 0 then
              raf.seek(pos)
              IntVal(pos)
            else IntVal(-1)
          catch case _: Exception => IntVal(-1)
        case None => IntVal(-1)
    }),
  )

  // Constant globals for std/io — registered in the interpreter's globals
  def ioConstants: Map[String, Value] = Map(
    "O_RDONLY" -> IntVal(O_RDONLY),
    "O_WRONLY" -> IntVal(O_WRONLY),
    "O_RDWR" -> IntVal(O_RDWR),
    "O_CREATE" -> IntVal(O_CREATE),
    "O_TRUNC" -> IntVal(O_TRUNC),
    "O_APPEND" -> IntVal(O_APPEND),
    "STDIN" -> IntVal(0),
    "STDOUT" -> IntVal(1),
    "STDERR" -> IntVal(2),
    "SEEK_SET" -> IntVal(SEEK_SET),
    "SEEK_CUR" -> IntVal(SEEK_CUR),
    "SEEK_END" -> IntVal(SEEK_END),
  )

  // ── std/fs ──────────────────────────────────────────────────────────

  private def mkFileStat(file: java.io.File): ArrVal =
    val cells = Array(
      new Cell(IntVal(file.length())),
      new Cell(IntVal(0L)), // mode — not easily available on JVM without NIO
      new Cell(IntVal(file.lastModified())),
      new Cell(IntVal(if file.isDirectory then 1L else 0L)),
      new Cell(IntVal(if file.isFile then 1L else 0L)),
    )
    ArrVal(cells, 0)

  private def mkDirEntry(file: java.io.File): ArrVal =
    val cells = Array(
      new Cell(mkString(file.getName)),
      new Cell(IntVal(if file.isDirectory then 1L else 0L)),
    )
    ArrVal(cells, 0)

  private def fsBuiltins(ctx: StdlibContext): Map[String, List[Value] => Value] = Map(
    "stat" -> (args => {
      val path = args.head.pipe(asString)
      val file = new java.io.File(path)
      if file.exists() then mkFileStat(file)
      else
        // Return zeroed struct on error
        ArrVal(Array.fill(5)(new Cell(IntVal(0))), 0)
    }),
    "readdir" -> (args => {
      val path = args.head.pipe(asString)
      val dir = new java.io.File(path)
      if dir.isDirectory then
        val files = dir.listFiles()
        if files == null then SliceVal(Array.empty, 0, 0, 0)
        else
          val entries = files.map(f => new Cell(mkDirEntry(f)))
          SliceVal(entries, 0, entries.length, entries.length)
      else SliceVal(Array.empty, 0, 0, 0)
    }),
    "exists" -> (args => {
      val path = args.head.pipe(asString)
      IntVal(if new java.io.File(path).exists() then 1L else 0L)
    }),
    "is_dir" -> (args => {
      val path = args.head.pipe(asString)
      IntVal(if new java.io.File(path).isDirectory then 1L else 0L)
    }),
    "mkdir" -> (args => {
      val path = args.head.pipe(asString)
      IntVal(if new java.io.File(path).mkdir() then 0 else -1)
    }),
    "mkdirs" -> (args => {
      val path = args.head.pipe(asString)
      IntVal(if new java.io.File(path).mkdirs() then 0 else -1)
    }),
    "remove" -> (args => {
      val path = args.head.pipe(asString)
      IntVal(if new java.io.File(path).delete() then 0 else -1)
    }),
    "rename" -> (args => {
      val old = args.head.pipe(asString)
      val newName = args(1).pipe(asString)
      IntVal(if new java.io.File(old).renameTo(new java.io.File(newName)) then 0 else -1)
    }),
    "getcwd" -> (_ => mkString(System.getProperty("user.dir"))),
    "read_file" -> (args => {
      val path = args.head.pipe(asString)
      try
        val bytes = java.nio.file.Files.readAllBytes(java.nio.file.Paths.get(path))
        mkString(new String(bytes, "UTF-8"))
      catch case _: Exception => mkString("")
    }),
    "write_file" -> (args => {
      val path = args.head.pipe(asString)
      val data = args(1).pipe(asString)
      try
        java.nio.file.Files.write(java.nio.file.Paths.get(path), data.getBytes("UTF-8"))
        IntVal(0)
      catch case _: Exception => IntVal(-1)
    }),
  )

  // ── std/process ─────────────────────────────────────────────────────

  private class ExitException(val code: Int) extends RuntimeException(s"exit($code)")

  private def processBuiltins(ctx: StdlibContext): Map[String, List[Value] => Value] = Map(
    "exit" -> (args => {
      val code = args.head.asInstanceOf[IntVal].n.toInt
      throw new ExitException(code)
    }),
    "getenv" -> (args => {
      val name = args.head.pipe(asString)
      val v = System.getenv(name)
      mkString(if v == null then "" else v)
    }),
    "argc" -> (_ => IntVal(ctx.argv.length)),
    "argv" -> (args => {
      val i = args.head.asInstanceOf[IntVal].n.toInt
      if i >= 0 && i < ctx.argv.length then mkString(ctx.argv(i))
      else mkString("")
    }),
  )

  // ── std/string ──────────────────────────────────────────────────────

  private def stringBuiltins(ctx: StdlibContext): Map[String, List[Value] => Value] = Map(
    "length" -> (args => {
      val s = args.head.pipe(asString)
      IntVal(s.length) // character count, not byte count
    }),
    "concat" -> (args => {
      val a = args.head.pipe(asString)
      val b = args(1).pipe(asString)
      mkString(a + b)
    }),
    "substr" -> (args => {
      val s = args.head.pipe(asString)
      val start = args(1).asInstanceOf[IntVal].n.toInt
      val length = args(2).asInstanceOf[IntVal].n.toInt
      try mkString(s.substring(start, (start + length).min(s.length)))
      catch case _: Exception => mkString("")
    }),
    "index_of" -> (args => {
      val s = args.head.pipe(asString)
      val sub = args(1).pipe(asString)
      IntVal(s.indexOf(sub))
    }),
    "starts_with" -> (args => {
      val s = args.head.pipe(asString)
      val prefix = args(1).pipe(asString)
      IntVal(if s.startsWith(prefix) then 1L else 0L)
    }),
    "ends_with" -> (args => {
      val s = args.head.pipe(asString)
      val suffix = args(1).pipe(asString)
      IntVal(if s.endsWith(suffix) then 1L else 0L)
    }),
    "trim" -> (args => {
      val s = args.head.pipe(asString)
      mkString(s.trim)
    }),
    "split" -> (args => {
      val s = args.head.pipe(asString)
      val delim = args(1).pipe(asString)
      val parts = if s.isEmpty then Array.empty[String]
      else s.split(java.util.regex.Pattern.quote(delim), -1)
      val cells = parts.map(p => new Cell(mkString(p)))
      SliceVal(cells, 0, cells.length, cells.length)
    }),
    "to_int" -> (args => {
      val s = args.head.pipe(asString)
      try IntVal(s.trim.toLong)
      catch case _: Exception => IntVal(0)
    }),
    "from_int" -> (args => {
      val n = args.head.asInstanceOf[IntVal].n
      mkString(n.toString)
    }),
    "char_at" -> (args => {
      val s = args.head.pipe(asString)
      val i = args(1).asInstanceOf[IntVal].n.toInt
      if i >= 0 && i < s.length then IntVal(s.charAt(i).toLong)
      else IntVal(-1)
    }),
    "equal" -> (args => {
      val a = args.head.pipe(asString)
      val b = args(1).pipe(asString)
      IntVal(if a == b then 1L else 0L)
    }),
    "contains" -> (args => {
      val s = args.head.pipe(asString)
      val sub = args(1).pipe(asString)
      IntVal(if s.contains(sub) then 1L else 0L)
    }),
  )
