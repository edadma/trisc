package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class LiterateRendererTests extends AnyFreeSpec with Matchers:
  val parser = new LiterateParser

  "tangle" - {
    "concatenates code blocks" in {
      val doc = parser.parse("Prose.\n\n    block1\n\nMore.\n\n    block2\n")
      val code = LiterateRenderer.tangle(doc)
      code should include("block1")
      code should include("block2")
      // blocks separated by newline
      code.indexOf("block1") should be < code.indexOf("block2")
    }

    "returns empty string for prose-only" in {
      val doc = parser.parse("Just prose here.\n")
      LiterateRenderer.tangle(doc) shouldBe ""
    }

    "single block" in {
      val doc = parser.parse("    only code\n")
      LiterateRenderer.tangle(doc) should include("only code")
    }
  }

  "renderHTML" - {
    "produces html with code blocks" in {
      val html = LiterateRenderer.renderHTML("# Title\n\n    val x = 42\n")
      html should include("<h1>Title</h1>")
      html should include("<code")
      html should include("42")
    }

    "fenced code blocks with language get highlighted" in {
      val html = LiterateRenderer.renderHTML("```python\nprint(42)\n```\n")
      html should include("<code")
      html should include("print")
    }

    "indented code blocks get sysl highlighting with spans" in {
      val html = LiterateRenderer.renderHTML("Some text.\n\n    val x = 42\n")
      html should include("<span")
      html should include("val")
      // 'val' is a keyword, should be wrapped in a span
      html should include regex "<span[^>]*>val</span>"
    }

    "prose renders as html" in {
      val html = LiterateRenderer.renderHTML("Hello **world**.\n")
      html should include("<strong>world</strong>")
    }

    "math blocks pass through" in {
      val html = LiterateRenderer.renderHTML("Inline \\(x^2\\) math.\n")
      html should include("x^2")
    }
  }
