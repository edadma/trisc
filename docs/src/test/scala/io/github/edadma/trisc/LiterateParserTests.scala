package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class LiterateParserTests extends AnyFreeSpec with Matchers {

  val parser = new LiterateParser

  "code blocks" - {
    "indented lines become a code block" in {
      val doc = parser.parse("    x = 1\n    y = 2")
      doc.blocks should have size 1
      doc.blocks.head shouldBe a[CodeBlock]
      doc.blocks.head.asInstanceOf[CodeBlock].content shouldBe "x = 1\ny = 2"
    }

    "tab-indented lines become a code block" in {
      val doc = parser.parse("\tx = 1\n\ty = 2")
      doc.blocks should have size 1
      doc.blocks.head.asInstanceOf[CodeBlock].content shouldBe "x = 1\ny = 2"
    }

    "blank lines within code are preserved" in {
      val doc = parser.parse("    x = 1\n\n    y = 2")
      doc.blocks should have size 1
      doc.blocks.head.asInstanceOf[CodeBlock].content shouldBe "x = 1\n\ny = 2"
    }

    "trailing blank lines are trimmed from code blocks" in {
      val doc = parser.parse("    x = 1\n\n")
      doc.blocks should have size 1
      doc.blocks.head.asInstanceOf[CodeBlock].content shouldBe "x = 1"
    }
  }

  "prose blocks" - {
    "non-indented lines become prose with default tech tag" in {
      val doc = parser.parse("This is prose.\nMore prose.")
      doc.blocks should have size 1
      val prose = doc.blocks.head.asInstanceOf[ProseBlock]
      prose.tag shouldBe "tech"
      prose.format shouldBe None
      prose.content shouldBe "This is prose.\nMore prose."
    }
  }

  "inline tags" - {
    "single-line tagged prose" in {
      val doc = parser.parse("@main This is user-facing text")
      doc.blocks should have size 1
      val prose = doc.blocks.head.asInstanceOf[ProseBlock]
      prose.tag shouldBe "main"
      prose.content shouldBe "This is user-facing text"
    }

    "tag with format suffix" in {
      val doc = parser.parse("@main-latex \\section{Introduction}")
      doc.blocks should have size 1
      val prose = doc.blocks.head.asInstanceOf[ProseBlock]
      prose.tag shouldBe "main"
      prose.format shouldBe Some("latex")
    }
  }

  "block tags" - {
    "multi-line tagged block" in {
      val doc = parser.parse("@main\nFirst paragraph.\n\nSecond paragraph.\n@")
      doc.blocks should have size 1
      val prose = doc.blocks.head.asInstanceOf[ProseBlock]
      prose.tag shouldBe "main"
      prose.content shouldBe "First paragraph.\n\nSecond paragraph."
    }

    "block tag with format" in {
      val doc = parser.parse("@tech-latex\n\\begin{equation}\nx = y\n\\end{equation}\n@")
      doc.blocks should have size 1
      val prose = doc.blocks.head.asInstanceOf[ProseBlock]
      prose.tag shouldBe "tech"
      prose.format shouldBe Some("latex")
    }
  }

  "api blocks" - {
    "inline api annotation" in {
      val doc = parser.parse("@api Adds two vectors")
      doc.blocks should have size 1
      val api = doc.blocks.head.asInstanceOf[ApiBlock]
      api.content shouldBe "Adds two vectors"
    }

    "multi-line api annotation" in {
      val doc = parser.parse("@api\nAdds two vectors.\n\n@param a First vector\n@param b Second vector\n@")
      doc.blocks should have size 1
      val api = doc.blocks.head.asInstanceOf[ApiBlock]
      api.content shouldBe "Adds two vectors.\n\n@param a First vector\n@param b Second vector"
    }
  }

  "mixed documents" - {
    "prose then code" in {
      val doc = parser.parse("This explains the function.\n    fn add(a: int, b: int) -> int = a + b")
      doc.blocks should have size 2
      doc.blocks(0) shouldBe a[ProseBlock]
      doc.blocks(1) shouldBe a[CodeBlock]
    }

    "code then prose then code" in {
      val doc = parser.parse("    x = 1\nSome explanation.\n    y = 2")
      doc.blocks should have size 3
      doc.blocks(0) shouldBe a[CodeBlock]
      doc.blocks(1) shouldBe a[ProseBlock]
      doc.blocks(2) shouldBe a[CodeBlock]
    }

    "api then code" in {
      val doc = parser.parse("@api Adds two vectors\n    fn add(a: int, b: int) -> int = a + b")
      doc.blocks should have size 2
      doc.blocks(0) shouldBe a[ApiBlock]
      doc.blocks(1) shouldBe a[CodeBlock]
    }

    "full literate document" in {
      val source =
        """@main
          |This module provides vector math operations.
          |@
          |
          |We implement addition first.
          |    fn add(a: int, b: int) -> int
          |        a + b
          |
          |@api Multiplies two numbers
          |    fn mul(a: int, b: int) -> int
          |        a * b""".stripMargin

      val doc = parser.parse(source)
      val types = doc.blocks.map(_.getClass.getSimpleName)
      types shouldBe List("ProseBlock", "ProseBlock", "CodeBlock", "ApiBlock", "CodeBlock")
    }
  }

  "source line map" - {
    "maps tangled code lines to original source lines" in {
      val source = "Some prose.\n    x = 1\n    y = 2"
      val doc = parser.parse(source)
      // Code starts at source line 2 (1-indexed)
      doc.sourceLineMap(0) shouldBe 2
      doc.sourceLineMap(1) shouldBe 3
    }

    "accounts for interleaved prose" in {
      val source = "    x = 1\nProse.\n    y = 2"
      val doc = parser.parse(source)
      doc.sourceLineMap(0) shouldBe 1 // x = 1 is line 1
      doc.sourceLineMap(1) shouldBe 3 // y = 2 is line 3
    }
  }

  "edge cases" - {
    "empty input" in {
      val doc = parser.parse("")
      doc.blocks shouldBe empty
    }

    "only blank lines" in {
      val doc = parser.parse("\n\n\n")
      doc.blocks shouldBe empty
    }

    "only code" in {
      val doc = parser.parse("    x = 1\n    y = 2")
      doc.blocks should have size 1
      doc.blocks.head shouldBe a[CodeBlock]
    }

    "only prose" in {
      val doc = parser.parse("Hello world.\nGoodbye.")
      doc.blocks should have size 1
      doc.blocks.head shouldBe a[ProseBlock]
    }
  }
}
