package io.github.edadma.trisc

class SyslAttributeTests extends SyslTestHelpers {

  private def parse(src: String): ProgramAST =
    val Right(ast) = (new SyslParser).parseProgram(src): @unchecked
    ast

  "attribute parser" - {

    "flag attribute on function" in {
      val ast = parse(
        """#test
          |foo() -> unit = 0
          |""".stripMargin)
      val f = ast.decls.collect { case f: FunDeclAST => f }.head
      f.attributes.map(_.name) shouldBe List("test")
      f.attributes.head.args shouldBe Nil
    }

    "string argument" in {
      val ast = parse(
        """#test("basic copy")
          |foo() -> unit = 0
          |""".stripMargin)
      val attr = ast.decls.collect { case f: FunDeclAST => f }.head.attributes.head
      attr.name shouldBe "test"
      attr.args shouldBe List(AttrPositional(AttrLitString("basic copy")))
    }

    "bare identifier as flag arg" in {
      val ast = parse(
        """#test(should_panic)
          |foo() -> unit = 0
          |""".stripMargin)
      val attr = ast.decls.collect { case f: FunDeclAST => f }.head.attributes.head
      attr.args shouldBe List(AttrPositional(AttrLitIdent("should_panic")))
    }

    "named string argument" in {
      val ast = parse(
        """#test(should_panic: "bad input")
          |foo() -> unit = 0
          |""".stripMargin)
      val attr = ast.decls.collect { case f: FunDeclAST => f }.head.attributes.head
      attr.args shouldBe List(AttrNamed("should_panic", AttrLitString("bad input")))
    }

    "multiple stacked attributes" in {
      val ast = parse(
        """#inline
          |#test
          |foo() -> unit = 0
          |""".stripMargin)
      val f = ast.decls.collect { case f: FunDeclAST => f }.head
      f.attributes.map(_.name) shouldBe List("inline", "test")
    }

    "attribute on struct" in {
      val ast = parse(
        """#deprecated
          |struct S
          |    x: int
          |""".stripMargin)
      val s = ast.decls.collect { case s: StructDeclAST => s }.head
      s.attributes.map(_.name) shouldBe List("deprecated")
    }

    "unknown attribute name is stored as-is" in {
      val ast = parse(
        """#quirk(42, true)
          |foo() -> unit = 0
          |""".stripMargin)
      val f = ast.decls.collect { case f: FunDeclAST => f }.head
      f.attributes.head.name shouldBe "quirk"
      f.attributes.head.args shouldBe List(
        AttrPositional(AttrLitInt(42L)),
        AttrPositional(AttrLitBool(true)),
      )
    }

    "conditional compilation still works (#if is not an attribute)" in {
      val src =
        """#if PLATFORM == "native"
          |foo() -> int = 1
          |#endif
          |""".stripMargin
      val ast = parse(src)
      ast.decls.collect { case c: CondDeclAST => c }.size shouldBe 1
    }
  }

  "#test analyzer validation" - {

    def analyze(src: String): Either[String, TProgram] =
      val Right(ast) = (new SyslParser).parseProgram(src): @unchecked
      try Right((new SyslAnalyzer).analyze(ast))
      catch case e: RuntimeException => Left(e.getMessage)

    "test function with params is rejected" in {
      val err = analyze(
        """#test
          |bad(x: int) -> unit = 0
          |""".stripMargin).left.toOption.get
      err should include ("zero parameters")
    }

    "test function with non-unit return is rejected" in {
      val err = analyze(
        """#test
          |bad() -> int = 42
          |""".stripMargin).left.toOption.get
      err should include ("unit")
    }

    "valid zero-arg unit test is accepted" in {
      analyze(
        """#test
          |good() -> unit = 0
          |
          |main() -> int = 0
          |""".stripMargin).isRight shouldBe true
    }
  }

  "panic builtin" - {
    "panic throws RuntimeError with the given message" in {
      val thrown = intercept[RuntimeException] {
        eval("""main() -> int
               |    panic("boom")
               |    0
               |""".stripMargin)
      }
      thrown.getMessage should include ("boom")
    }
  }

  "assert builtin" - {
    "passes through when condition is true" in {
      eval("""main() -> int
             |    assert(1 == 1, "nope")
             |    42
             |""".stripMargin) shouldBe 42
    }

    "panics when condition is false" in {
      val thrown = intercept[RuntimeException] {
        eval("""main() -> int
               |    assert(1 == 2, "math failed")
               |    0
               |""".stripMargin)
      }
      thrown.getMessage should include ("math failed")
    }
  }

  "#deprecated warnings" - {
    def captureStderr(f: => Unit): String =
      val out = new java.io.ByteArrayOutputStream
      val saved = System.err
      System.setErr(new java.io.PrintStream(out))
      try f finally System.setErr(saved)
      out.toString

    "emits warning with reason when deprecated function is called" in {
      val err = captureStderr {
        eval("""#deprecated("use foo2 instead")
               |foo() -> int = 1
               |
               |main() -> int = foo()
               |""".stripMargin)
      }
      err should include ("foo")
      err should include ("deprecated")
      err should include ("use foo2 instead")
    }

    "emits warning without reason for bare #deprecated" in {
      val err = captureStderr {
        eval("""#deprecated
               |old() -> int = 1
               |
               |main() -> int = old()
               |""".stripMargin)
      }
      err should include ("old")
      err should include ("deprecated")
    }

    "only warns once per deprecated function" in {
      val err = captureStderr {
        eval("""#deprecated("gone")
               |f() -> int = 1
               |
               |main() -> int
               |    f()
               |    f()
               |    f()
               |""".stripMargin)
      }
      err.split("\n").count(_.contains("deprecated")) shouldBe 1
    }

    "no warning when function is not called" in {
      val err = captureStderr {
        eval("""#deprecated
               |unused() -> int = 1
               |
               |main() -> int = 42
               |""".stripMargin)
      }
      err should not include ("deprecated")
    }
  }

  "runNamed invocation" - {
    "calls a zero-arg function by name without invoking main" in {
      val Right(ast) = (new SyslParser).parseProgram(
        """target() -> unit
          |    panic("hit target")
          |
          |main() -> int = 0
          |""".stripMargin): @unchecked
      val typed = (new SyslAnalyzer).analyze(ast)
      val interp = new SyslInterpreter(_ => ())
      interp.load(typed)
      val thrown = intercept[RuntimeException] { interp.runNamed("target") }
      thrown.getMessage should include ("hit target")
    }
  }
}
