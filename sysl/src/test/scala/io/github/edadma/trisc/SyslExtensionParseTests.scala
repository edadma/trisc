package io.github.edadma.trisc

class SyslExtensionParseTests extends SyslTestHelpers {

  private def parse(src: String): ProgramAST =
    val Right(ast) = (new SyslParser).parseProgram(src): @unchecked
    ast

  "extension parser" - {

    "single parameterless method on string receiver" in {
      val ast = parse(
        """extension (s: string)
          |    def shout -> string = s
          |""".stripMargin)
      val ext = ast.decls.collect { case e: ExtensionDeclAST => e }.head
      ext.typeParams shouldBe Nil
      ext.receiver.name shouldBe "s"
      ext.methods.length shouldBe 1
      val m = ext.methods.head
      m.name shouldBe "shout"
      m.params shouldBe Nil
      m.returnType.map(_.toString).getOrElse("") should include("string")
      m.isDef shouldBe true
    }

    "multiple methods in one block" in {
      val ast = parse(
        """extension (s: string)
          |    def first -> string = s
          |    def second -> string = s
          |""".stripMargin)
      val ext = ast.decls.collect { case e: ExtensionDeclAST => e }.head
      ext.methods.map(_.name) shouldBe List("first", "second")
    }

    "type parameters bind in receiver" in {
      val ast = parse(
        """extension [T](xs: []T)
          |    def first(idx: int) -> T = xs[idx]
          |""".stripMargin)
      val ext = ast.decls.collect { case e: ExtensionDeclAST => e }.head
      ext.typeParams shouldBe List("T")
      ext.receiver.name shouldBe "xs"
    }

    "attribute on extension method" in {
      val ast = parse(
        """extension (s: string)
          |    #deprecated
          |    def old -> string = s
          |""".stripMargin)
      val ext = ast.decls.collect { case e: ExtensionDeclAST => e }.head
      val m = ext.methods.head
      m.name shouldBe "old"
      m.attributes.map(_.name) shouldBe List("deprecated")
    }

    "attribute on extension block itself" in {
      val ast = parse(
        """#deprecated
          |extension (s: string)
          |    def x -> string = s
          |""".stripMargin)
      val ext = ast.decls.collect { case e: ExtensionDeclAST => e }.head
      ext.attributes.map(_.name) shouldBe List("deprecated")
    }

    "end marker accepted" in {
      val ast = parse(
        """extension (s: string)
          |    def x -> string = s
          |end extension
          |""".stripMargin)
      val ext = ast.decls.collect { case e: ExtensionDeclAST => e }.head
      ext.methods.length shouldBe 1
    }
  }
}
