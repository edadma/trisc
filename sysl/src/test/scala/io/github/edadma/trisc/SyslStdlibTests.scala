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
      case ImportDeclAST(path) if SyslStdlib.modules.contains(path) => path
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
    "str_concat" in {
      output(
        """import "std/string"
          |main() -> int
          |    puts(str_concat("hello", " world"))
          |    0
          |""".stripMargin
      ) shouldBe "hello world"
    }

    "str_concat empty strings" in {
      output(
        """import "std/string"
          |main() -> int
          |    puts(str_concat("", "abc"))
          |    0
          |""".stripMargin
      ) shouldBe "abc"
    }

    "str_concat both empty" in {
      eval(
        """import "std/string"
          |main() -> int = str_len(str_concat("", ""))
          |""".stripMargin
      ) shouldBe 0
    }

    "str_len" in {
      eval(
        """import "std/string"
          |main() -> int = str_len("hello")
          |""".stripMargin
      ) shouldBe 5
    }

    "str_len empty" in {
      eval(
        """import "std/string"
          |main() -> int = str_len("")
          |""".stripMargin
      ) shouldBe 0
    }

    "str_substr" in {
      output(
        """import "std/string"
          |main() -> int
          |    puts(str_substr("hello world", 6, 5))
          |    0
          |""".stripMargin
      ) shouldBe "world"
    }

    "str_substr from start" in {
      output(
        """import "std/string"
          |main() -> int
          |    puts(str_substr("hello", 0, 3))
          |    0
          |""".stripMargin
      ) shouldBe "hel"
    }

    "str_substr length exceeds string" in {
      output(
        """import "std/string"
          |main() -> int
          |    puts(str_substr("hi", 0, 100))
          |    0
          |""".stripMargin
      ) shouldBe "hi"
    }

    "str_index_of" in {
      eval(
        """import "std/string"
          |main() -> int = str_index_of("hello world", "world")
          |""".stripMargin
      ) shouldBe 6
    }

    "str_index_of at start" in {
      eval(
        """import "std/string"
          |main() -> int = str_index_of("hello", "hel")
          |""".stripMargin
      ) shouldBe 0
    }

    "str_index_of not found" in {
      eval(
        """import "std/string"
          |main() -> int = str_index_of("hello", "xyz")
          |""".stripMargin
      ) shouldBe -1
    }

    "str_starts_with true" in {
      eval(
        """import "std/string"
          |main() -> int = int(str_starts_with("hello world", "hello"))
          |""".stripMargin
      ) shouldBe 1
    }

    "str_starts_with false" in {
      eval(
        """import "std/string"
          |main() -> int = int(str_starts_with("hello world", "world"))
          |""".stripMargin
      ) shouldBe 0
    }

    "str_starts_with empty prefix" in {
      eval(
        """import "std/string"
          |main() -> int = int(str_starts_with("hello", ""))
          |""".stripMargin
      ) shouldBe 1
    }

    "str_ends_with true" in {
      eval(
        """import "std/string"
          |main() -> int = int(str_ends_with("hello world", "world"))
          |""".stripMargin
      ) shouldBe 1
    }

    "str_ends_with false" in {
      eval(
        """import "std/string"
          |main() -> int = int(str_ends_with("hello world", "hello"))
          |""".stripMargin
      ) shouldBe 0
    }

    "str_trim" in {
      output(
        """import "std/string"
          |main() -> int
          |    puts(str_trim("  hello  "))
          |    0
          |""".stripMargin
      ) shouldBe "hello"
    }

    "str_trim no whitespace" in {
      output(
        """import "std/string"
          |main() -> int
          |    puts(str_trim("hello"))
          |    0
          |""".stripMargin
      ) shouldBe "hello"
    }

    "str_trim all whitespace" in {
      eval(
        """import "std/string"
          |main() -> int = str_len(str_trim("   "))
          |""".stripMargin
      ) shouldBe 0
    }

    "str_split" in {
      output(
        """import "std/string"
          |main() -> int
          |    parts = str_split("a,b,c", ",")
          |    for i = 0; i < len(parts); i++
          |        puts(parts[i])
          |        putchar(32)
          |    0
          |""".stripMargin
      ) shouldBe "a b c "
    }

    "str_split no delimiter found" in {
      output(
        """import "std/string"
          |main() -> int
          |    parts = str_split("hello", ",")
          |    puts(parts[0])
          |    0
          |""".stripMargin
      ) shouldBe "hello"
    }

    "str_split empty string" in {
      eval(
        """import "std/string"
          |main() -> int = len(str_split("", ","))
          |""".stripMargin
      ) shouldBe 0 // empty string returns empty slice
    }

    "str_to_int" in {
      eval(
        """import "std/string"
          |main() -> i64 = str_to_int("42")
          |""".stripMargin
      ) shouldBe 42
    }

    "str_to_int negative" in {
      eval(
        """import "std/string"
          |main() -> i64 = str_to_int("-100")
          |""".stripMargin
      ) shouldBe -100
    }

    "str_to_int invalid returns 0" in {
      eval(
        """import "std/string"
          |main() -> i64 = str_to_int("abc")
          |""".stripMargin
      ) shouldBe 0
    }

    "str_from_int" in {
      output(
        """import "std/string"
          |main() -> int
          |    puts(str_from_int(123))
          |    0
          |""".stripMargin
      ) shouldBe "123"
    }

    "str_from_int negative" in {
      output(
        """import "std/string"
          |main() -> int
          |    puts(str_from_int(-42))
          |    0
          |""".stripMargin
      ) shouldBe "-42"
    }

    "str_from_int zero" in {
      output(
        """import "std/string"
          |main() -> int
          |    puts(str_from_int(0))
          |    0
          |""".stripMargin
      ) shouldBe "0"
    }

    "str_equal true" in {
      eval(
        """import "std/string"
          |main() -> int = int(str_equal("abc", "abc"))
          |""".stripMargin
      ) shouldBe 1
    }

    "str_equal false" in {
      eval(
        """import "std/string"
          |main() -> int = int(str_equal("abc", "def"))
          |""".stripMargin
      ) shouldBe 0
    }

    "str_equal empty strings" in {
      eval(
        """import "std/string"
          |main() -> int = int(str_equal("", ""))
          |""".stripMargin
      ) shouldBe 1
    }

    "str_contains true" in {
      eval(
        """import "std/string"
          |main() -> int = int(str_contains("hello world", "lo wo"))
          |""".stripMargin
      ) shouldBe 1
    }

    "str_contains false" in {
      eval(
        """import "std/string"
          |main() -> int = int(str_contains("hello", "xyz"))
          |""".stripMargin
      ) shouldBe 0
    }

    "str_contains empty needle" in {
      eval(
        """import "std/string"
          |main() -> int = int(str_contains("hello", ""))
          |""".stripMargin
      ) shouldBe 1
    }

    "str_char_at" in {
      eval(
        """import "std/string"
          |main() -> int = str_char_at("ABC", 1)
          |""".stripMargin
      ) shouldBe 66 // 'B'
    }

    "str_char_at first" in {
      eval(
        """import "std/string"
          |main() -> int = str_char_at("ABC", 0)
          |""".stripMargin
      ) shouldBe 65 // 'A'
    }

    "str_char_at out of bounds" in {
      eval(
        """import "std/string"
          |main() -> int = str_char_at("ABC", 10)
          |""".stripMargin
      ) shouldBe -1
    }
  }

  // ===== std/io =====

  "std/io" - {
    "io_write_string to STDOUT" in {
      output(
        """import "std/io"
          |main() -> int
          |    io_write_string(STDOUT, "hello io")
          |    0
          |""".stripMargin
      ) shouldBe "hello io"
    }

    "io_write_string returns byte count" in {
      eval(
        """import "std/io"
          |main() -> int = io_write_string(STDOUT, "hello")
          |""".stripMargin
      ) shouldBe 5
    }

    "io_write_string to STDERR" in {
      output(
        """import "std/io"
          |main() -> int
          |    io_write_string(STDERR, "err msg")
          |    0
          |""".stripMargin
      ) shouldBe "err msg"
    }

    "io_open, io_write_string, io_close, io_read round-trip" in {
      // Write a temp file, read it back
      output(
        """import "std/io"
          |import "std/fs"
          |main() -> int
          |    path = "/tmp/_sysl_test_io_roundtrip.txt"
          |    fd = io_open(path, O_CREATE + O_WRONLY + O_TRUNC)
          |    if fd < 0
          |        return 1
          |    io_write_string(fd, "hello from sysl")
          |    io_close(fd)
          |    content = fs_read_file(path)
          |    puts(content)
          |    fs_remove(path)
          |    0
          |""".stripMargin
      ) shouldBe "hello from sysl"
    }

    "io_open read mode" in {
      // Write with fs, then read with io_open + io_read_line
      output(
        """import "std/io"
          |import "std/fs"
          |main() -> int
          |    path = "/tmp/_sysl_test_io_read.txt"
          |    fs_write_file(path, "test line")
          |    fd = io_open(path, O_RDONLY)
          |    if fd < 0
          |        return 1
          |    line = io_read_line(fd)
          |    io_close(fd)
          |    puts(line)
          |    fs_remove(path)
          |    0
          |""".stripMargin
      ) shouldBe "test line"
    }

    "io_open nonexistent file returns -1" in {
      eval(
        """import "std/io"
          |main() -> int = io_open("/tmp/_sysl_nonexistent_12345.txt", O_RDONLY)
          |""".stripMargin
      ) shouldBe -1
    }

    "io_close invalid fd returns -1" in {
      eval(
        """import "std/io"
          |main() -> int = io_close(999)
          |""".stripMargin
      ) shouldBe -1
    }

    "io_read into buffer" in {
      output(
        """import "std/io"
          |import "std/fs"
          |main() -> int
          |    path = "/tmp/_sysl_test_io_readbuf.txt"
          |    fs_write_file(path, "ABCDE")
          |    fd = io_open(path, O_RDONLY)
          |    var buf: [10]u8
          |    n = io_read(fd, &buf[0], 5)
          |    io_close(fd)
          |    for i = 0; i < n; i++
          |        putchar(int(buf[i]))
          |    fs_remove(path)
          |    0
          |""".stripMargin
      ) shouldBe "ABCDE"
    }

    "io_write from buffer" in {
      output(
        """import "std/io"
          |import "std/fs"
          |main() -> int
          |    path = "/tmp/_sysl_test_io_writebuf.txt"
          |    fd = io_open(path, O_CREATE + O_WRONLY + O_TRUNC)
          |    var buf: [3]u8
          |    buf[0] = 88u8
          |    buf[1] = 89u8
          |    buf[2] = 90u8
          |    io_write(fd, &buf[0], 3)
          |    io_close(fd)
          |    puts(fs_read_file(path))
          |    fs_remove(path)
          |    0
          |""".stripMargin
      ) shouldBe "XYZ"
    }

    "io_seek and read" in {
      output(
        """import "std/io"
          |import "std/fs"
          |main() -> int
          |    path = "/tmp/_sysl_test_io_seek.txt"
          |    fs_write_file(path, "ABCDEFGH")
          |    fd = io_open(path, O_RDONLY)
          |    io_seek(fd, 4i64, SEEK_SET)
          |    line = io_read_line(fd)
          |    io_close(fd)
          |    puts(line)
          |    fs_remove(path)
          |    0
          |""".stripMargin
      ) shouldBe "EFGH"
    }

    "io_seek returns new position" in {
      eval(
        """import "std/io"
          |import "std/fs"
          |main() -> i64
          |    path = "/tmp/_sysl_test_io_seekpos.txt"
          |    fs_write_file(path, "ABCDEFGH")
          |    fd = io_open(path, O_RDONLY)
          |    pos = io_seek(fd, 5i64, SEEK_SET)
          |    io_close(fd)
          |    fs_remove(path)
          |    pos
          |""".stripMargin
      ) shouldBe 5
    }

    "STDOUT constant is 1" in {
      eval(
        """import "std/io"
          |main() -> int = STDOUT
          |""".stripMargin
      ) shouldBe 1
    }

    "STDIN constant is 0" in {
      eval(
        """import "std/io"
          |main() -> int = STDIN
          |""".stripMargin
      ) shouldBe 0
    }

    "STDERR constant is 2" in {
      eval(
        """import "std/io"
          |main() -> int = STDERR
          |""".stripMargin
      ) shouldBe 2
    }

    "O_RDONLY constant is 0" in {
      eval(
        """import "std/io"
          |main() -> int = O_RDONLY
          |""".stripMargin
      ) shouldBe 0
    }

    "all flag constants accessible" in {
      eval(
        """import "std/io"
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
    "fs_exists on existing path" in {
      eval(
        """import "std/fs"
          |main() -> int = int(fs_exists("."))
          |""".stripMargin
      ) shouldBe 1
    }

    "fs_exists on nonexistent path" in {
      eval(
        """import "std/fs"
          |main() -> int = int(fs_exists("nonexistent_path_xyz_987"))
          |""".stripMargin
      ) shouldBe 0
    }

    "fs_is_dir on directory" in {
      eval(
        """import "std/fs"
          |main() -> int = int(fs_is_dir("."))
          |""".stripMargin
      ) shouldBe 1
    }

    "fs_is_dir on file" in {
      eval(
        """import "std/fs"
          |main() -> int = int(fs_is_dir("build.sbt"))
          |""".stripMargin
      ) shouldBe 0
    }

    "fs_getcwd returns non-empty string" in {
      eval(
        """import "std/fs"
          |import "std/string"
          |main() -> int
          |    cwd = fs_getcwd()
          |    if str_len(cwd) > 0
          |        return 1
          |    0
          |""".stripMargin
      ) shouldBe 1
    }

    "fs_readdir returns entries" in {
      eval(
        """import "std/fs"
          |main() -> int
          |    entries = fs_readdir(".")
          |    if len(entries) > 0
          |        return 1
          |    0
          |""".stripMargin
      ) shouldBe 1
    }

    "fs_readdir entry has name field" in {
      eval(
        """import "std/fs"
          |import "std/string"
          |main() -> int
          |    entries = fs_readdir(".")
          |    if str_len(entries[0].name) > 0
          |        return 1
          |    0
          |""".stripMargin
      ) shouldBe 1
    }

    "fs_readdir on nonexistent dir returns empty" in {
      eval(
        """import "std/fs"
          |main() -> int = len(fs_readdir("/nonexistent_dir_xyz_987"))
          |""".stripMargin
      ) shouldBe 0
    }

    "fs_stat returns size" in {
      eval(
        """import "std/fs"
          |main() -> int
          |    info = fs_stat("build.sbt")
          |    if info.size > 0
          |        return 1
          |    0
          |""".stripMargin
      ) shouldBe 1
    }

    "fs_stat is_file on file" in {
      eval(
        """import "std/fs"
          |main() -> int
          |    info = fs_stat("build.sbt")
          |    int(info.is_file)
          |""".stripMargin
      ) shouldBe 1
    }

    "fs_stat is_dir on directory" in {
      eval(
        """import "std/fs"
          |main() -> int
          |    info = fs_stat(".")
          |    int(info.is_dir)
          |""".stripMargin
      ) shouldBe 1
    }

    "fs_stat mtime is positive" in {
      eval(
        """import "std/fs"
          |main() -> int
          |    info = fs_stat("build.sbt")
          |    if info.mtime > 0
          |        return 1
          |    0
          |""".stripMargin
      ) shouldBe 1
    }

    "fs_stat on nonexistent file returns zeroed struct" in {
      eval(
        """import "std/fs"
          |main() -> i64
          |    info = fs_stat("/nonexistent_xyz_987")
          |    info.size
          |""".stripMargin
      ) shouldBe 0
    }

    "fs_read_file" in {
      output(
        """import "std/fs"
          |main() -> int
          |    fs_write_file("/tmp/_sysl_test_readfile.txt", "hello file")
          |    puts(fs_read_file("/tmp/_sysl_test_readfile.txt"))
          |    fs_remove("/tmp/_sysl_test_readfile.txt")
          |    0
          |""".stripMargin
      ) shouldBe "hello file"
    }

    "fs_read_file nonexistent returns empty" in {
      eval(
        """import "std/fs"
          |import "std/string"
          |main() -> int = str_len(fs_read_file("/nonexistent_xyz_987"))
          |""".stripMargin
      ) shouldBe 0
    }

    "fs_write_file and read back" in {
      output(
        """import "std/fs"
          |main() -> int
          |    path = "/tmp/_sysl_test_writefile.txt"
          |    fs_write_file(path, "written by sysl")
          |    puts(fs_read_file(path))
          |    fs_remove(path)
          |    0
          |""".stripMargin
      ) shouldBe "written by sysl"
    }

    "fs_write_file returns 0 on success" in {
      eval(
        """import "std/fs"
          |main() -> int
          |    path = "/tmp/_sysl_test_writeret.txt"
          |    result = fs_write_file(path, "test")
          |    fs_remove(path)
          |    result
          |""".stripMargin
      ) shouldBe 0
    }

    "fs_mkdir and fs_remove" in {
      eval(
        """import "std/fs"
          |main() -> int
          |    path = "/tmp/_sysl_test_mkdir"
          |    r1 = fs_mkdir(path)
          |    exists = int(fs_is_dir(path))
          |    r2 = fs_remove(path)
          |    gone = int(fs_exists(path))
          |    if r1 == 0 && exists == 1 && r2 == 0 && gone == 0
          |        return 1
          |    0
          |""".stripMargin
      ) shouldBe 1
    }

    "fs_mkdirs nested" in {
      eval(
        """import "std/fs"
          |main() -> int
          |    path = "/tmp/_sysl_test_mkdirs/sub/dir"
          |    r = fs_mkdirs(path)
          |    exists = int(fs_is_dir(path))
          |    fs_remove("/tmp/_sysl_test_mkdirs/sub/dir")
          |    fs_remove("/tmp/_sysl_test_mkdirs/sub")
          |    fs_remove("/tmp/_sysl_test_mkdirs")
          |    if r == 0 && exists == 1
          |        return 1
          |    0
          |""".stripMargin
      ) shouldBe 1
    }

    "fs_rename" in {
      eval(
        """import "std/fs"
          |main() -> int
          |    old = "/tmp/_sysl_test_rename_old.txt"
          |    new_path = "/tmp/_sysl_test_rename_new.txt"
          |    fs_write_file(old, "data")
          |    r = fs_rename(old, new_path)
          |    old_gone = int(fs_exists(old))
          |    new_exists = int(fs_exists(new_path))
          |    fs_remove(new_path)
          |    if r == 0 && old_gone == 0 && new_exists == 1
          |        return 1
          |    0
          |""".stripMargin
      ) shouldBe 1
    }

    "fs_remove returns -1 on nonexistent" in {
      eval(
        """import "std/fs"
          |main() -> int = fs_remove("/nonexistent_xyz_987")
          |""".stripMargin
      ) shouldBe -1
    }
  }

  // ===== std/process =====

  "std/process" - {
    "proc_argc returns 0 with no args" in {
      eval(
        """import "std/process"
          |main() -> int = proc_argc()
          |""".stripMargin
      ) shouldBe 0
    }

    "proc_argv out of bounds returns empty" in {
      eval(
        """import "std/process"
          |import "std/string"
          |main() -> int = str_len(proc_argv(0))
          |""".stripMargin
      ) shouldBe 0
    }

    "proc_getenv returns empty for nonexistent var" in {
      eval(
        """import "std/process"
          |import "std/string"
          |main() -> int = str_len(proc_getenv("NONEXISTENT_VAR_XYZ_123"))
          |""".stripMargin
      ) shouldBe 0
    }

    "proc_getenv returns value for PATH" in {
      // PATH should always be set
      eval(
        """import "std/process"
          |import "std/string"
          |main() -> int
          |    path = proc_getenv("PATH")
          |    if str_len(path) > 0
          |        return 1
          |    0
          |""".stripMargin
      ) shouldBe 1
    }

    "proc_exit throws" in {
      an[Exception] should be thrownBy eval(
        """import "std/process"
          |main() -> int
          |    proc_exit(42)
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
          """import "util"
            |import "std/string"
            |main() -> int
            |    greet(str_concat("hi ", "there"))
            |    0
            |""".stripMargin,
      ))
      out shouldBe "hi there"
    }

    "multiple stdlib imports in one file" in {
      output(
        """import "std/io"
          |import "std/string"
          |import "std/fs"
          |main() -> int
          |    exists = int(fs_exists("."))
          |    msg = str_concat("exists=", str_from_int(i64(exists)))
          |    io_write_string(STDOUT, msg)
          |    0
          |""".stripMargin
      ) shouldBe "exists=1"
    }
  }
}
