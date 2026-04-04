package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class LiterateParserTests extends AnyFreeSpec with Matchers:
  val parser = new LiterateParser

  "empty document" in {
    val doc = parser.parse("")
    doc.codeBlocks shouldBe empty
  }

  "prose only" in {
    val doc = parser.parse("# Hello\n\nSome text here.\n")
    doc.codeBlocks shouldBe empty
  }

  "single indented code block" in {
    val doc = parser.parse("Some prose.\n\n    val x = 42\n    val y = 10\n")
    doc.codeBlocks should have size 1
    doc.codeBlocks.head should include("val x = 42")
    doc.codeBlocks.head should include("val y = 10")
  }

  "multiple indented code blocks in order" in {
    val doc = parser.parse(
      """Some prose.
        |
        |    val x = 1
        |
        |More prose.
        |
        |    val y = 2
        |""".stripMargin
    )
    doc.codeBlocks should have size 2
    doc.codeBlocks(0) should include("val x = 1")
    doc.codeBlocks(1) should include("val y = 2")
  }

  "fenced code blocks are ignored" in {
    val doc = parser.parse(
      """Some prose.
        |
        |```python
        |print("hello")
        |```
        |
        |    val x = 42
        |""".stripMargin
    )
    doc.codeBlocks should have size 1
    doc.codeBlocks.head should include("val x = 42")
  }

  "headings then code" in {
    val doc = parser.parse(
      """# Title
        |
        |Some text.
        |
        |    val code = true
        |""".stripMargin
    )
    doc.codeBlocks should have size 1
    doc.codeBlocks.head should include("val code = true")
  }

  "list does not consume subsequent indented code" in {
    // Need a blank line gap after the list for the indented block to be top-level
    val doc = parser.parse(
      """- item 1
        |- item 2
        |
        |<!-- -->
        |
        |    val code = true
        |""".stripMargin
    )
    doc.codeBlocks should have size 1
    doc.codeBlocks.head should include("val code = true")
  }

  "tabs rejected" in {
    assertThrows[LiterateParser#ParseError] {
      parser.parse("hello\tworld")
    }
  }

  "mixed content extracts only indented code" in {
    val doc = parser.parse(
      """# Module Header
        |
        |This module provides utilities.
        |
        |    extern syscall(n: int) -> i64
        |
        |## Usage
        |
        |Here is an example:
        |
        |```sysl
        |syscall(0)
        |```
        |
        |    val SYS_SLEEP = 0
        |    val SYS_PUTC = 1
        |
        |That's the API.
        |""".stripMargin
    )
    doc.codeBlocks should have size 2
    doc.codeBlocks(0) should include("extern syscall(n: int) -> i64")
    doc.codeBlocks(1) should include("val SYS_SLEEP = 0")
    doc.codeBlocks(1) should include("val SYS_PUTC = 1")
  }

  "indented code inside blockquote not extracted" in {
    val doc = parser.parse(
      """> Some quote
        |>     not code
        |
        |    real code
        |""".stripMargin
    )
    doc.codeBlocks should have size 1
    doc.codeBlocks.head should include("real code")
  }

  "only blank lines" in {
    val doc = parser.parse("\n\n\n")
    doc.codeBlocks shouldBe empty
  }

  "only code" in {
    val doc = parser.parse("    x = 1\n    y = 2\n")
    doc.codeBlocks should have size 1
    doc.codeBlocks.head should include("x = 1")
    doc.codeBlocks.head should include("y = 2")
  }

  "list then blank then indented code — diagnostic" in {
    val input = "- item 1\n- item 2\n\n    code here\n"
    val doc = parser.parse(input)
    info(s"codeBlocks count: ${doc.codeBlocks.size}")
    doc.codeBlocks.foreach(b => info(s"block: [$b]"))
    // This test documents CommonMark behavior — list may consume the code
  }

  "services.lsysl round-trip" in {
    val source = scala.io.Source.fromFile("oskit/services/services.lsysl").mkString
    val doc = parser.parse(source)
    val tangled = LiterateRenderer.tangle(doc)
    info(s"codeBlocks: ${doc.codeBlocks.size}")
    info(s"tangled length: ${tangled.length}")
    tangled should include("SYS_TLS_SET")
    tangled should include("extern syscall")
    tangled should include("pimutex_unlock_sys")
  }
