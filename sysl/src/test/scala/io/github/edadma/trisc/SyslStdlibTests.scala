package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SyslStdlibTests extends AnyFreeSpec with Matchers {

  // Helper: run a single-file program with stdlib support
  private def run(source: String): (Long, String) =
    val buf = new StringBuilder
    val parser = new SyslParser
    val Right(ast) = parser.parseProgram(source): @unchecked
    val stdlibImports = ast.decls.collect {
      case ImportDeclAST(path, _) if SyslStdlib.modules.contains(path) => path
    }.toSet
    val analyzer = new SyslAnalyzer
    for mod <- stdlibImports do
      analyzer.registerImport(SyslStdlib.meta(mod))
    val typed = analyzer.analyze(ast)
    val interp = new SyslInterpreter(s => buf ++= s)
    val ctx = new SyslStdlib.StdlibContext(output = s => buf ++= s)
    for mod <- stdlibImports do
      interp.registerBuiltins(SyslStdlib.builtins(mod, ctx))
    if stdlibImports.contains("std/io") then
      for (name, value) <- SyslStdlib.ioConstants do
        interp.registerGlobal(name, value)
    val result = interp.run(typed)
    (result, buf.toString)

  private def eval(source: String): Long = run(source)._1
  private def output(source: String): String = run(source)._2

  // Helper: run with multi-file driver
  private def runMulti(sources: Map[String, String]): (Long, String) =
    val buf = new StringBuilder
    val driver = new SyslDriver
    val result = driver.compile(sources)
    val stdlibImports = driver.collectStdlibImports(result.units)
    val merged = TProgram(result.units.flatMap(_.typed.decls))
    val interp = new SyslInterpreter(s => buf ++= s)
    val ctx = new SyslStdlib.StdlibContext(output = s => buf ++= s)
    for mod <- stdlibImports do
      interp.registerBuiltins(SyslStdlib.builtins(mod, ctx))
    if stdlibImports.contains("std/io") then
      for (name, value) <- SyslStdlib.ioConstants do
        interp.registerGlobal(name, value)
    val value = interp.run(merged)
    (value, buf.toString)

  // ===== std/string =====

  "std/string" - {
    "concat" in {
      output(
        """import std.string.*
          |main() -> int
          |    puts(concat("hello", " world"))
          |    0
          |""".stripMargin
      ) shouldBe "hello world"
    }

    "concat empty strings" in {
      output(
        """import std.string.*
          |main() -> int
          |    puts(concat("", "abc"))
          |    0
          |""".stripMargin
      ) shouldBe "abc"
    }

    "concat both empty" in {
      eval(
        """import std.string.*
          |main() -> int = length(concat("", ""))
          |""".stripMargin
      ) shouldBe 0
    }

    "length" in {
      eval(
        """import std.string.*
          |main() -> int = length("hello")
          |""".stripMargin
      ) shouldBe 5
    }

    "length empty" in {
      eval(
        """import std.string.*
          |main() -> int = length("")
          |""".stripMargin
      ) shouldBe 0
    }

    "substr" in {
      output(
        """import std.string.*
          |main() -> int
          |    puts(substr("hello world", 6, 5))
          |    0
          |""".stripMargin
      ) shouldBe "world"
    }

    "substr from start" in {
      output(
        """import std.string.*
          |main() -> int
          |    puts(substr("hello", 0, 3))
          |    0
          |""".stripMargin
      ) shouldBe "hel"
    }

    "substr length exceeds string" in {
      output(
        """import std.string.*
          |main() -> int
          |    puts(substr("hi", 0, 100))
          |    0
          |""".stripMargin
      ) shouldBe "hi"
    }

    "index_of" in {
      eval(
        """import std.string.*
          |main() -> int = index_of("hello world", "world")
          |""".stripMargin
      ) shouldBe 6
    }

    "index_of at start" in {
      eval(
        """import std.string.*
          |main() -> int = index_of("hello", "hel")
          |""".stripMargin
      ) shouldBe 0
    }

    "index_of not found" in {
      eval(
        """import std.string.*
          |main() -> int = index_of("hello", "xyz")
          |""".stripMargin
      ) shouldBe -1
    }

    "starts_with true" in {
      eval(
        """import std.string.*
          |main() -> int = int(starts_with("hello world", "hello"))
          |""".stripMargin
      ) shouldBe 1
    }

    "starts_with false" in {
      eval(
        """import std.string.*
          |main() -> int = int(starts_with("hello world", "world"))
          |""".stripMargin
      ) shouldBe 0
    }

    "starts_with empty prefix" in {
      eval(
        """import std.string.*
          |main() -> int = int(starts_with("hello", ""))
          |""".stripMargin
      ) shouldBe 1
    }

    "ends_with true" in {
      eval(
        """import std.string.*
          |main() -> int = int(ends_with("hello world", "world"))
          |""".stripMargin
      ) shouldBe 1
    }

    "ends_with false" in {
      eval(
        """import std.string.*
          |main() -> int = int(ends_with("hello world", "hello"))
          |""".stripMargin
      ) shouldBe 0
    }

    "trim" in {
      output(
        """import std.string.*
          |main() -> int
          |    puts(trim("  hello  "))
          |    0
          |""".stripMargin
      ) shouldBe "hello"
    }

    "trim no whitespace" in {
      output(
        """import std.string.*
          |main() -> int
          |    puts(trim("hello"))
          |    0
          |""".stripMargin
      ) shouldBe "hello"
    }

    "trim all whitespace" in {
      eval(
        """import std.string.*
          |main() -> int = length(trim("   "))
          |""".stripMargin
      ) shouldBe 0
    }

    "split" in {
      output(
        """import std.string.*
          |main() -> int
          |    parts = split("a,b,c", ",")
          |    for i = 0; i < len(parts); i++
          |        puts(parts[i])
          |        putchar(32)
          |    0
          |""".stripMargin
      ) shouldBe "a b c "
    }

    "split no delimiter found" in {
      output(
        """import std.string.*
          |main() -> int
          |    parts = split("hello", ",")
          |    puts(parts[0])
          |    0
          |""".stripMargin
      ) shouldBe "hello"
    }

    "split empty string" in {
      eval(
        """import std.string.*
          |main() -> int = len(split("", ","))
          |""".stripMargin
      ) shouldBe 0 // empty string returns empty slice
    }

    "to_int" in {
      eval(
        """import std.string.*
          |main() -> i64 = to_int("42")
          |""".stripMargin
      ) shouldBe 42
    }

    "to_int negative" in {
      eval(
        """import std.string.*
          |main() -> i64 = to_int("-100")
          |""".stripMargin
      ) shouldBe -100
    }

    "to_int invalid returns 0" in {
      eval(
        """import std.string.*
          |main() -> i64 = to_int("abc")
          |""".stripMargin
      ) shouldBe 0
    }

    "from_int" in {
      output(
        """import std.string.*
          |main() -> int
          |    puts(from_int(123))
          |    0
          |""".stripMargin
      ) shouldBe "123"
    }

    "from_int negative" in {
      output(
        """import std.string.*
          |main() -> int
          |    puts(from_int(-42))
          |    0
          |""".stripMargin
      ) shouldBe "-42"
    }

    "from_int zero" in {
      output(
        """import std.string.*
          |main() -> int
          |    puts(from_int(0))
          |    0
          |""".stripMargin
      ) shouldBe "0"
    }

    "equal true" in {
      eval(
        """import std.string.*
          |main() -> int = int(equal("abc", "abc"))
          |""".stripMargin
      ) shouldBe 1
    }

    "equal false" in {
      eval(
        """import std.string.*
          |main() -> int = int(equal("abc", "def"))
          |""".stripMargin
      ) shouldBe 0
    }

    "equal empty strings" in {
      eval(
        """import std.string.*
          |main() -> int = int(equal("", ""))
          |""".stripMargin
      ) shouldBe 1
    }

    "contains true" in {
      eval(
        """import std.string.*
          |main() -> int = int(contains("hello world", "lo wo"))
          |""".stripMargin
      ) shouldBe 1
    }

    "contains false" in {
      eval(
        """import std.string.*
          |main() -> int = int(contains("hello", "xyz"))
          |""".stripMargin
      ) shouldBe 0
    }

    "contains empty needle" in {
      eval(
        """import std.string.*
          |main() -> int = int(contains("hello", ""))
          |""".stripMargin
      ) shouldBe 1
    }

    "char_at" in {
      eval(
        """import std.string.*
          |main() -> int = char_at("ABC", 1)
          |""".stripMargin
      ) shouldBe 66 // 'B'
    }

    "char_at first" in {
      eval(
        """import std.string.*
          |main() -> int = char_at("ABC", 0)
          |""".stripMargin
      ) shouldBe 65 // 'A'
    }

    "char_at out of bounds" in {
      eval(
        """import std.string.*
          |main() -> int = char_at("ABC", 10)
          |""".stripMargin
      ) shouldBe -1
    }
  }

  // ===== std/io =====

  "std/io" - {
    "write_string to STDOUT" in {
      output(
        """import std.io.*
          |main() -> int
          |    write_string(STDOUT, "hello io")
          |    0
          |""".stripMargin
      ) shouldBe "hello io"
    }

    "write_string returns byte count" in {
      eval(
        """import std.io.*
          |main() -> int = write_string(STDOUT, "hello")
          |""".stripMargin
      ) shouldBe 5
    }

    "write_string to STDERR" in {
      output(
        """import std.io.*
          |main() -> int
          |    write_string(STDERR, "err msg")
          |    0
          |""".stripMargin
      ) shouldBe "err msg"
    }

    "open, write_string, close, read round-trip" in {
      // Write a temp file, read it back
      output(
        """import std.io.*
          |import std.fs.*
          |main() -> int
          |    path = "/tmp/_sysl_test_io_roundtrip.txt"
          |    fd = open(path, O_CREATE + O_WRONLY + O_TRUNC)
          |    if fd < 0
          |        return 1
          |    write_string(fd, "hello from sysl")
          |    close(fd)
          |    content = read_file(path)
          |    puts(content)
          |    remove(path)
          |    0
          |""".stripMargin
      ) shouldBe "hello from sysl"
    }

    "open read mode" in {
      // Write with fs, then read with open + read_line
      output(
        """import std.io.*
          |import std.fs.*
          |main() -> int
          |    path = "/tmp/_sysl_test_read.txt"
          |    write_file(path, "test line")
          |    fd = open(path, O_RDONLY)
          |    if fd < 0
          |        return 1
          |    line = read_line(fd)
          |    close(fd)
          |    puts(line)
          |    remove(path)
          |    0
          |""".stripMargin
      ) shouldBe "test line"
    }

    "open nonexistent file returns -1" in {
      eval(
        """import std.io.*
          |main() -> int = open("/tmp/_sysl_nonexistent_12345.txt", O_RDONLY)
          |""".stripMargin
      ) shouldBe -1
    }

    "close invalid fd returns -1" in {
      eval(
        """import std.io.*
          |main() -> int = close(999)
          |""".stripMargin
      ) shouldBe -1
    }

    "read into buffer" in {
      output(
        """import std.io.*
          |import std.fs.*
          |main() -> int
          |    path = "/tmp/_sysl_test_readbuf.txt"
          |    write_file(path, "ABCDE")
          |    fd = open(path, O_RDONLY)
          |    var buf: [10]u8
          |    n = read(fd, &buf[0], 5)
          |    close(fd)
          |    for i = 0; i < n; i++
          |        putchar(int(buf[i]))
          |    remove(path)
          |    0
          |""".stripMargin
      ) shouldBe "ABCDE"
    }

    "write from buffer" in {
      output(
        """import std.io.*
          |import std.fs.*
          |main() -> int
          |    path = "/tmp/_sysl_test_writebuf.txt"
          |    fd = open(path, O_CREATE + O_WRONLY + O_TRUNC)
          |    var buf: [3]u8
          |    buf[0] = 88u8
          |    buf[1] = 89u8
          |    buf[2] = 90u8
          |    write(fd, &buf[0], 3)
          |    close(fd)
          |    puts(read_file(path))
          |    remove(path)
          |    0
          |""".stripMargin
      ) shouldBe "XYZ"
    }

    "seek and read" in {
      output(
        """import std.io.*
          |import std.fs.*
          |main() -> int
          |    path = "/tmp/_sysl_test_seek.txt"
          |    write_file(path, "ABCDEFGH")
          |    fd = open(path, O_RDONLY)
          |    seek(fd, 4i64, SEEK_SET)
          |    line = read_line(fd)
          |    close(fd)
          |    puts(line)
          |    remove(path)
          |    0
          |""".stripMargin
      ) shouldBe "EFGH"
    }

    "seek returns new position" in {
      eval(
        """import std.io.*
          |import std.fs.*
          |main() -> i64
          |    path = "/tmp/_sysl_test_seekpos.txt"
          |    write_file(path, "ABCDEFGH")
          |    fd = open(path, O_RDONLY)
          |    pos = seek(fd, 5i64, SEEK_SET)
          |    close(fd)
          |    remove(path)
          |    pos
          |""".stripMargin
      ) shouldBe 5
    }

    "STDOUT constant is 1" in {
      eval(
        """import std.io.*
          |main() -> int = STDOUT
          |""".stripMargin
      ) shouldBe 1
    }

    "STDIN constant is 0" in {
      eval(
        """import std.io.*
          |main() -> int = STDIN
          |""".stripMargin
      ) shouldBe 0
    }

    "STDERR constant is 2" in {
      eval(
        """import std.io.*
          |main() -> int = STDERR
          |""".stripMargin
      ) shouldBe 2
    }

    "O_RDONLY constant is 0" in {
      eval(
        """import std.io.*
          |main() -> int = O_RDONLY
          |""".stripMargin
      ) shouldBe 0
    }

    "all flag constants accessible" in {
      eval(
        """import std.io.*
          |main() -> int
          |    // Just verify they all type-check and are accessible
          |    sum = O_RDONLY + O_WRONLY + O_RDWR + O_CREATE + O_TRUNC + O_APPEND
          |    sum = sum + SEEK_SET + SEEK_CUR + SEEK_END
          |    if sum > 0
          |        return 1
          |    0
          |""".stripMargin
      ) shouldBe 1
    }
  }

  // ===== std/fs =====

  "std/fs" - {
    "exists on existing path" in {
      eval(
        """import std.fs.*
          |main() -> int = int(exists("."))
          |""".stripMargin
      ) shouldBe 1
    }

    "exists on nonexistent path" in {
      eval(
        """import std.fs.*
          |main() -> int = int(exists("nonexistent_path_xyz_987"))
          |""".stripMargin
      ) shouldBe 0
    }

    "is_dir on directory" in {
      eval(
        """import std.fs.*
          |main() -> int = int(is_dir("."))
          |""".stripMargin
      ) shouldBe 1
    }

    "is_dir on file" in {
      eval(
        """import std.fs.*
          |main() -> int = int(is_dir("build.sbt"))
          |""".stripMargin
      ) shouldBe 0
    }

    "getcwd returns non-empty string" in {
      eval(
        """import std.fs.*
          |import std.string.*
          |main() -> int
          |    cwd = getcwd()
          |    if length(cwd) > 0
          |        return 1
          |    0
          |""".stripMargin
      ) shouldBe 1
    }

    "readdir returns entries" in {
      eval(
        """import std.fs.*
          |main() -> int
          |    entries = readdir(".")
          |    if len(entries) > 0
          |        return 1
          |    0
          |""".stripMargin
      ) shouldBe 1
    }

    "readdir entry has name field" in {
      eval(
        """import std.fs.*
          |import std.string.*
          |main() -> int
          |    entries = readdir(".")
          |    if length(entries[0].name) > 0
          |        return 1
          |    0
          |""".stripMargin
      ) shouldBe 1
    }

    "readdir on nonexistent dir returns empty" in {
      eval(
        """import std.fs.*
          |main() -> int = len(readdir("/nonexistent_dir_xyz_987"))
          |""".stripMargin
      ) shouldBe 0
    }

    "stat returns size" in {
      eval(
        """import std.fs.*
          |main() -> int
          |    info = stat("build.sbt")
          |    if info.size > 0
          |        return 1
          |    0
          |""".stripMargin
      ) shouldBe 1
    }

    "stat is_file on file" in {
      eval(
        """import std.fs.*
          |main() -> int
          |    info = stat("build.sbt")
          |    int(info.is_file)
          |""".stripMargin
      ) shouldBe 1
    }

    "stat is_dir on directory" in {
      eval(
        """import std.fs.*
          |main() -> int
          |    info = stat(".")
          |    int(info.is_dir)
          |""".stripMargin
      ) shouldBe 1
    }

    "stat mtime is positive" in {
      eval(
        """import std.fs.*
          |main() -> int
          |    info = stat("build.sbt")
          |    if info.mtime > 0
          |        return 1
          |    0
          |""".stripMargin
      ) shouldBe 1
    }

    "stat on nonexistent file returns zeroed struct" in {
      eval(
        """import std.fs.*
          |main() -> i64
          |    info = stat("/nonexistent_xyz_987")
          |    info.size
          |""".stripMargin
      ) shouldBe 0
    }

    "read_file" in {
      output(
        """import std.fs.*
          |main() -> int
          |    write_file("/tmp/_sysl_test_readfile.txt", "hello file")
          |    puts(read_file("/tmp/_sysl_test_readfile.txt"))
          |    remove("/tmp/_sysl_test_readfile.txt")
          |    0
          |""".stripMargin
      ) shouldBe "hello file"
    }

    "read_file nonexistent returns empty" in {
      eval(
        """import std.fs.*
          |import std.string.*
          |main() -> int = length(read_file("/nonexistent_xyz_987"))
          |""".stripMargin
      ) shouldBe 0
    }

    "write_file and read back" in {
      output(
        """import std.fs.*
          |main() -> int
          |    path = "/tmp/_sysl_test_writefile.txt"
          |    write_file(path, "written by sysl")
          |    puts(read_file(path))
          |    remove(path)
          |    0
          |""".stripMargin
      ) shouldBe "written by sysl"
    }

    "write_file returns 0 on success" in {
      eval(
        """import std.fs.*
          |main() -> int
          |    path = "/tmp/_sysl_test_writeret.txt"
          |    result = write_file(path, "test")
          |    remove(path)
          |    result
          |""".stripMargin
      ) shouldBe 0
    }

    "mkdir and remove" in {
      eval(
        """import std.fs.*
          |main() -> int
          |    path = "/tmp/_sysl_test_mkdir"
          |    r1 = mkdir(path)
          |    did_exist = int(is_dir(path))
          |    r2 = remove(path)
          |    gone = int(exists(path))
          |    if r1 == 0 && did_exist == 1 && r2 == 0 && gone == 0
          |        return 1
          |    0
          |""".stripMargin
      ) shouldBe 1
    }

    "mkdirs nested" in {
      eval(
        """import std.fs.*
          |main() -> int
          |    path = "/tmp/_sysl_test_mkdirs/sub/dir"
          |    r = mkdirs(path)
          |    did_exist = int(is_dir(path))
          |    remove("/tmp/_sysl_test_mkdirs/sub/dir")
          |    remove("/tmp/_sysl_test_mkdirs/sub")
          |    remove("/tmp/_sysl_test_mkdirs")
          |    if r == 0 && did_exist == 1
          |        return 1
          |    0
          |""".stripMargin
      ) shouldBe 1
    }

    "rename" in {
      eval(
        """import std.fs.*
          |main() -> int
          |    old = "/tmp/_sysl_test_rename_old.txt"
          |    new_path = "/tmp/_sysl_test_rename_new.txt"
          |    write_file(old, "data")
          |    r = rename(old, new_path)
          |    old_gone = int(exists(old))
          |    new_exists = int(exists(new_path))
          |    remove(new_path)
          |    if r == 0 && old_gone == 0 && new_exists == 1
          |        return 1
          |    0
          |""".stripMargin
      ) shouldBe 1
    }

    "remove returns -1 on nonexistent" in {
      eval(
        """import std.fs.*
          |main() -> int = remove("/nonexistent_xyz_987")
          |""".stripMargin
      ) shouldBe -1
    }
  }

  // ===== std/process =====

  "std/process" - {
    "argc returns 0 with no args" in {
      eval(
        """import std.process.*
          |main() -> int = argc()
          |""".stripMargin
      ) shouldBe 0
    }

    "argv out of bounds returns empty" in {
      eval(
        """import std.process.*
          |import std.string.*
          |main() -> int = length(argv(0))
          |""".stripMargin
      ) shouldBe 0
    }

    "getenv returns empty for nonexistent var" in {
      eval(
        """import std.process.*
          |import std.string.*
          |main() -> int = length(getenv("NONEXISTENT_VAR_XYZ_123"))
          |""".stripMargin
      ) shouldBe 0
    }

    "getenv returns value for PATH" in {
      // PATH should always be set
      eval(
        """import std.process.*
          |import std.string.*
          |main() -> int
          |    path = getenv("PATH")
          |    if length(path) > 0
          |        return 1
          |    0
          |""".stripMargin
      ) shouldBe 1
    }

    "exit throws" in {
      an[Exception] should be thrownBy eval(
        """import std.process.*
          |main() -> int
          |    exit(42)
          |    0
          |""".stripMargin
      )
    }
  }

  // ===== Multi-module with stdlib =====

  "stdlib with multi-file driver" - {
    "user module + stdlib import" in {
      val (_, out) = runMulti(Map(
        "util" ->
          """greet(name: string)
            |    puts(name)
            |""".stripMargin,
        "main" ->
          """import util.*
            |import std.string.*
            |main() -> int
            |    greet(concat("hi ", "there"))
            |    0
            |""".stripMargin,
      ))
      out shouldBe "hi there"
    }

    "multiple stdlib imports in one file" in {
      output(
        """import std.io.*
          |import std.string.*
          |import std.fs.*
          |main() -> int
          |    found = int(exists("."))
          |    msg = concat("exists=", from_int(i64(found)))
          |    write_string(STDOUT, msg)
          |    0
          |""".stripMargin
      ) shouldBe "exists=1"
    }
  }
}
