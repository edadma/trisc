package io.github.edadma.trisc

import scala.collection.mutable
import SyslType.*

object SyslStdlib:
  import Value.*

  val modules: Set[String] = Set("std/io", "std/fs", "std/process", "std/string")

  def meta(name: String): ModuleMeta = name match
    case "std/io"      => ioMeta
    case "std/fs"      => fsMeta
    case "std/process" => processMeta
    case "std/string"  => stringMeta
    case _             => throw IllegalArgumentException(s"unknown stdlib module: $name")

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

  private lazy val ioMeta: ModuleMeta =
    new ModuleMeta(List(
      // Constants
      SymbolMeta("O_RDONLY", SymbolMeta.Kind.Data(I32), isPrivate = false),
      SymbolMeta("O_WRONLY", SymbolMeta.Kind.Data(I32), isPrivate = false),
      SymbolMeta("O_RDWR", SymbolMeta.Kind.Data(I32), isPrivate = false),
      SymbolMeta("O_CREATE", SymbolMeta.Kind.Data(I32), isPrivate = false),
      SymbolMeta("O_TRUNC", SymbolMeta.Kind.Data(I32), isPrivate = false),
      SymbolMeta("O_APPEND", SymbolMeta.Kind.Data(I32), isPrivate = false),
      SymbolMeta("STDIN", SymbolMeta.Kind.Data(I32), isPrivate = false),
      SymbolMeta("STDOUT", SymbolMeta.Kind.Data(I32), isPrivate = false),
      SymbolMeta("STDERR", SymbolMeta.Kind.Data(I32), isPrivate = false),
      SymbolMeta("SEEK_SET", SymbolMeta.Kind.Data(I32), isPrivate = false),
      SymbolMeta("SEEK_CUR", SymbolMeta.Kind.Data(I32), isPrivate = false),
      SymbolMeta("SEEK_END", SymbolMeta.Kind.Data(I32), isPrivate = false),
      // Functions
      SymbolMeta("io_open", SymbolMeta.Kind.Func(List(StringType, I32), I32), isPrivate = false),
      SymbolMeta("io_close", SymbolMeta.Kind.Func(List(I32), I32), isPrivate = false),
      SymbolMeta("io_read", SymbolMeta.Kind.Func(List(I32, PtrType(U8), I32), I32), isPrivate = false),
      SymbolMeta("io_write", SymbolMeta.Kind.Func(List(I32, PtrType(U8), I32), I32), isPrivate = false),
      SymbolMeta("io_write_string", SymbolMeta.Kind.Func(List(I32, StringType), I32), isPrivate = false),
      SymbolMeta("io_read_line", SymbolMeta.Kind.Func(List(I32), StringType), isPrivate = false),
      SymbolMeta("io_seek", SymbolMeta.Kind.Func(List(I32, I64, I32), I64), isPrivate = false),
    ))

  private def ioBuiltins(ctx: StdlibContext): Map[String, List[Value] => Value] = Map(
    // Constants as "functions" that return their value — will be registered as globals
    "io_open" -> (args => {
      val path = args.head.asInstanceOf[StrVal].s
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
    "io_close" -> (args => {
      val fd = args.head.asInstanceOf[IntVal].n.toInt
      IntVal(if ctx.closeFile(fd) then 0 else -1)
    }),
    "io_read" -> (args => {
      val fd = args.head.asInstanceOf[IntVal].n.toInt
      val buf = args(1) // ArrVal or PtrVal
      val count = args(2).asInstanceOf[IntVal].n.toInt
      if fd == 0 then
        // stdin
        var bytesRead = 0
        val arr = buf.asInstanceOf[ArrVal]
        var i = 0
        while i < count do
          val b = ctx.input()
          if b == -1 then
            i = count // break
          else
            arr.cells(arr.offset + bytesRead).value = IntVal(b.toLong)
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
                val arr = buf.asInstanceOf[ArrVal]
                for i <- 0 until n do
                  arr.cells(arr.offset + i).value = IntVal(bytes(i) & 0xff)
              IntVal(n)
            catch case _: Exception => IntVal(-1)
          case None => IntVal(-1)
    }),
    "io_write" -> (args => {
      val fd = args.head.asInstanceOf[IntVal].n.toInt
      val buf = args(1).asInstanceOf[ArrVal]
      val count = args(2).asInstanceOf[IntVal].n.toInt
      val bytes = new Array[Byte](count)
      for i <- 0 until count do
        bytes(i) = buf.cells(buf.offset + i).value.asInstanceOf[IntVal].n.toByte
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
    "io_write_string" -> (args => {
      val fd = args.head.asInstanceOf[IntVal].n.toInt
      val s = args(1).asInstanceOf[StrVal].s
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
    "io_read_line" -> (args => {
      val fd = args.head.asInstanceOf[IntVal].n.toInt
      if fd == 0 then
        // stdin — read until newline
        val sb = new StringBuilder
        var done = false
        while !done do
          val b = ctx.input()
          if b == -1 || b == '\n' then done = true
          else sb += b.toChar
        StrVal(sb.toString)
      else
        ctx.getFile(fd) match
          case Some(raf) =>
            try
              val line = raf.readLine()
              if line == null then StrVal("") else StrVal(line)
            catch case _: Exception => StrVal("")
          case None => StrVal("")
    }),
    "io_seek" -> (args => {
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

  private val fileStatType: StructType = StructType("FileStat", List(
    ("size", I64),
    ("mode", I32),
    ("mtime", I64),
    ("is_dir", BoolType),
    ("is_file", BoolType),
  ))

  private val dirEntryType: StructType = StructType("DirEntry", List(
    ("name", StringType),
    ("is_dir", BoolType),
  ))

  private lazy val fsMeta: ModuleMeta =
    new ModuleMeta(List(
      SymbolMeta("FileStat", SymbolMeta.Kind.Struct(fileStatType), isPrivate = false),
      SymbolMeta("DirEntry", SymbolMeta.Kind.Struct(dirEntryType), isPrivate = false),
      SymbolMeta("fs_stat", SymbolMeta.Kind.Func(List(StringType), fileStatType), isPrivate = false),
      SymbolMeta("fs_readdir", SymbolMeta.Kind.Func(List(StringType), SliceType(dirEntryType)), isPrivate = false),
      SymbolMeta("fs_exists", SymbolMeta.Kind.Func(List(StringType), BoolType), isPrivate = false),
      SymbolMeta("fs_is_dir", SymbolMeta.Kind.Func(List(StringType), BoolType), isPrivate = false),
      SymbolMeta("fs_mkdir", SymbolMeta.Kind.Func(List(StringType), I32), isPrivate = false),
      SymbolMeta("fs_mkdirs", SymbolMeta.Kind.Func(List(StringType), I32), isPrivate = false),
      SymbolMeta("fs_remove", SymbolMeta.Kind.Func(List(StringType), I32), isPrivate = false),
      SymbolMeta("fs_rename", SymbolMeta.Kind.Func(List(StringType, StringType), I32), isPrivate = false),
      SymbolMeta("fs_getcwd", SymbolMeta.Kind.Func(List(), StringType), isPrivate = false),
      SymbolMeta("fs_read_file", SymbolMeta.Kind.Func(List(StringType), StringType), isPrivate = false),
      SymbolMeta("fs_write_file", SymbolMeta.Kind.Func(List(StringType, StringType), I32), isPrivate = false),
    ))

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
      new Cell(StrVal(file.getName)),
      new Cell(IntVal(if file.isDirectory then 1L else 0L)),
    )
    ArrVal(cells, 0)

  private def fsBuiltins(ctx: StdlibContext): Map[String, List[Value] => Value] = Map(
    "fs_stat" -> (args => {
      val path = args.head.asInstanceOf[StrVal].s
      val file = new java.io.File(path)
      if file.exists() then mkFileStat(file)
      else
        // Return zeroed struct on error
        ArrVal(Array.fill(5)(new Cell(IntVal(0))), 0)
    }),
    "fs_readdir" -> (args => {
      val path = args.head.asInstanceOf[StrVal].s
      val dir = new java.io.File(path)
      if dir.isDirectory then
        val files = dir.listFiles()
        if files == null then SliceVal(Array.empty, 0, 0, 0)
        else
          val entries = files.map(f => new Cell(mkDirEntry(f)))
          SliceVal(entries, 0, entries.length, entries.length)
      else SliceVal(Array.empty, 0, 0, 0)
    }),
    "fs_exists" -> (args => {
      val path = args.head.asInstanceOf[StrVal].s
      IntVal(if new java.io.File(path).exists() then 1L else 0L)
    }),
    "fs_is_dir" -> (args => {
      val path = args.head.asInstanceOf[StrVal].s
      IntVal(if new java.io.File(path).isDirectory then 1L else 0L)
    }),
    "fs_mkdir" -> (args => {
      val path = args.head.asInstanceOf[StrVal].s
      IntVal(if new java.io.File(path).mkdir() then 0 else -1)
    }),
    "fs_mkdirs" -> (args => {
      val path = args.head.asInstanceOf[StrVal].s
      IntVal(if new java.io.File(path).mkdirs() then 0 else -1)
    }),
    "fs_remove" -> (args => {
      val path = args.head.asInstanceOf[StrVal].s
      IntVal(if new java.io.File(path).delete() then 0 else -1)
    }),
    "fs_rename" -> (args => {
      val old = args.head.asInstanceOf[StrVal].s
      val newName = args(1).asInstanceOf[StrVal].s
      IntVal(if new java.io.File(old).renameTo(new java.io.File(newName)) then 0 else -1)
    }),
    "fs_getcwd" -> (_ => StrVal(System.getProperty("user.dir"))),
    "fs_read_file" -> (args => {
      val path = args.head.asInstanceOf[StrVal].s
      try
        val bytes = java.nio.file.Files.readAllBytes(java.nio.file.Paths.get(path))
        StrVal(new String(bytes, "UTF-8"))
      catch case _: Exception => StrVal("")
    }),
    "fs_write_file" -> (args => {
      val path = args.head.asInstanceOf[StrVal].s
      val data = args(1).asInstanceOf[StrVal].s
      try
        java.nio.file.Files.write(java.nio.file.Paths.get(path), data.getBytes("UTF-8"))
        IntVal(0)
      catch case _: Exception => IntVal(-1)
    }),
  )

  // ── std/process ─────────────────────────────────────────────────────

  private lazy val processMeta: ModuleMeta =
    new ModuleMeta(List(
      SymbolMeta("proc_exit", SymbolMeta.Kind.Func(List(I32), VoidType), isPrivate = false),
      SymbolMeta("proc_getenv", SymbolMeta.Kind.Func(List(StringType), StringType), isPrivate = false),
      SymbolMeta("proc_argc", SymbolMeta.Kind.Func(List(), I32), isPrivate = false),
      SymbolMeta("proc_argv", SymbolMeta.Kind.Func(List(I32), StringType), isPrivate = false),
    ))

  private class ExitException(val code: Int) extends RuntimeException(s"exit($code)")

  private def processBuiltins(ctx: StdlibContext): Map[String, List[Value] => Value] = Map(
    "proc_exit" -> (args => {
      val code = args.head.asInstanceOf[IntVal].n.toInt
      throw new ExitException(code)
    }),
    "proc_getenv" -> (args => {
      val name = args.head.asInstanceOf[StrVal].s
      val v = System.getenv(name)
      StrVal(if v == null then "" else v)
    }),
    "proc_argc" -> (_ => IntVal(ctx.argv.length)),
    "proc_argv" -> (args => {
      val i = args.head.asInstanceOf[IntVal].n.toInt
      if i >= 0 && i < ctx.argv.length then StrVal(ctx.argv(i))
      else StrVal("")
    }),
  )

  // ── std/string ──────────────────────────────────────────────────────

  private lazy val stringMeta: ModuleMeta =
    new ModuleMeta(List(
      SymbolMeta("str_len", SymbolMeta.Kind.Func(List(StringType), I32), isPrivate = false),
      SymbolMeta("str_concat", SymbolMeta.Kind.Func(List(StringType, StringType), StringType), isPrivate = false),
      SymbolMeta("str_substr", SymbolMeta.Kind.Func(List(StringType, I32, I32), StringType), isPrivate = false),
      SymbolMeta("str_index_of", SymbolMeta.Kind.Func(List(StringType, StringType), I32), isPrivate = false),
      SymbolMeta("str_starts_with", SymbolMeta.Kind.Func(List(StringType, StringType), BoolType), isPrivate = false),
      SymbolMeta("str_ends_with", SymbolMeta.Kind.Func(List(StringType, StringType), BoolType), isPrivate = false),
      SymbolMeta("str_trim", SymbolMeta.Kind.Func(List(StringType), StringType), isPrivate = false),
      SymbolMeta("str_split", SymbolMeta.Kind.Func(List(StringType, StringType), SliceType(StringType)), isPrivate = false),
      SymbolMeta("str_to_int", SymbolMeta.Kind.Func(List(StringType), I64), isPrivate = false),
      SymbolMeta("str_from_int", SymbolMeta.Kind.Func(List(I64), StringType), isPrivate = false),
      SymbolMeta("str_char_at", SymbolMeta.Kind.Func(List(StringType, I32), I32), isPrivate = false),
      SymbolMeta("str_equal", SymbolMeta.Kind.Func(List(StringType, StringType), BoolType), isPrivate = false),
      SymbolMeta("str_contains", SymbolMeta.Kind.Func(List(StringType, StringType), BoolType), isPrivate = false),
    ))

  private def stringBuiltins(ctx: StdlibContext): Map[String, List[Value] => Value] = Map(
    "str_len" -> (args => {
      val s = args.head.asInstanceOf[StrVal].s
      IntVal(s.length) // character count, not byte count
    }),
    "str_concat" -> (args => {
      val a = args.head.asInstanceOf[StrVal].s
      val b = args(1).asInstanceOf[StrVal].s
      StrVal(a + b)
    }),
    "str_substr" -> (args => {
      val s = args.head.asInstanceOf[StrVal].s
      val start = args(1).asInstanceOf[IntVal].n.toInt
      val length = args(2).asInstanceOf[IntVal].n.toInt
      try StrVal(s.substring(start, (start + length).min(s.length)))
      catch case _: Exception => StrVal("")
    }),
    "str_index_of" -> (args => {
      val s = args.head.asInstanceOf[StrVal].s
      val sub = args(1).asInstanceOf[StrVal].s
      IntVal(s.indexOf(sub))
    }),
    "str_starts_with" -> (args => {
      val s = args.head.asInstanceOf[StrVal].s
      val prefix = args(1).asInstanceOf[StrVal].s
      IntVal(if s.startsWith(prefix) then 1L else 0L)
    }),
    "str_ends_with" -> (args => {
      val s = args.head.asInstanceOf[StrVal].s
      val suffix = args(1).asInstanceOf[StrVal].s
      IntVal(if s.endsWith(suffix) then 1L else 0L)
    }),
    "str_trim" -> (args => {
      val s = args.head.asInstanceOf[StrVal].s
      StrVal(s.trim)
    }),
    "str_split" -> (args => {
      val s = args.head.asInstanceOf[StrVal].s
      val delim = args(1).asInstanceOf[StrVal].s
      val parts = if s.isEmpty then Array.empty[String]
      else s.split(java.util.regex.Pattern.quote(delim), -1)
      val cells = parts.map(p => new Cell(StrVal(p)))
      SliceVal(cells, 0, cells.length, cells.length)
    }),
    "str_to_int" -> (args => {
      val s = args.head.asInstanceOf[StrVal].s
      try IntVal(s.trim.toLong)
      catch case _: Exception => IntVal(0)
    }),
    "str_from_int" -> (args => {
      val n = args.head.asInstanceOf[IntVal].n
      StrVal(n.toString)
    }),
    "str_char_at" -> (args => {
      val s = args.head.asInstanceOf[StrVal].s
      val i = args(1).asInstanceOf[IntVal].n.toInt
      if i >= 0 && i < s.length then IntVal(s.charAt(i).toLong)
      else IntVal(-1)
    }),
    "str_equal" -> (args => {
      val a = args.head.asInstanceOf[StrVal].s
      val b = args(1).asInstanceOf[StrVal].s
      IntVal(if a == b then 1L else 0L)
    }),
    "str_contains" -> (args => {
      val s = args.head.asInstanceOf[StrVal].s
      val sub = args(1).asInstanceOf[StrVal].s
      IntVal(if s.contains(sub) then 1L else 0L)
    }),
  )
