package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class LiterateRendererTests extends AnyFreeSpec with Matchers {

  val parser = new LiterateParser

  "tangle" - {
    "extracts only code blocks" in {
      val doc = parser.parse("Some prose.\n    x = 1\nMore prose.\n    y = 2")
      LiterateRenderer.tangle(doc) shouldBe "x = 1\ny = 2"
    }

    "preserves blank lines within code" in {
      val doc = parser.parse("    x = 1\n\n    y = 2")
      LiterateRenderer.tangle(doc) shouldBe "x = 1\n\ny = 2"
    }

    "returns empty string for prose-only document" in {
      val doc = parser.parse("Just prose here.\nNothing else.")
      LiterateRenderer.tangle(doc) shouldBe ""
    }
  }

  "renderLiterate" - {
    "includes code and matching prose for dev channel" in {
      val doc = parser.parse("Technical note.\n    x = 1")
      val result = LiterateRenderer.renderLiterate(doc, ChannelConfig.dev)
      result should include("Technical note.")
      result should include("x = 1")
    }

    "excludes api blocks from dev channel" in {
      val doc = parser.parse("@api Some api note\n    x = 1")
      val result = LiterateRenderer.renderLiterate(doc, ChannelConfig.dev)
      result should not include "Some api note"
      result should include("x = 1")
    }
  }

  "renderProse" - {
    "website channel includes main but excludes tech" in {
      val source = "@main\nUser-facing docs.\n@\nTechnical detail.\n    code here"
      val doc = parser.parse(source)
      val result = LiterateRenderer.renderProse(doc, ChannelConfig.website)
      result should include("User-facing docs.")
      result should not include "Technical detail."
      result should not include "code here"
    }

    "dev channel includes both main and tech" in {
      val source = "@main User-facing.\nTechnical detail."
      val doc = parser.parse(source)
      val result = LiterateRenderer.renderProse(doc, ChannelConfig.dev)
      result should include("User-facing.")
      result should include("Technical detail.")
    }
  }

  "renderApi" - {
    "extracts only api blocks" in {
      val source = "Some prose.\n@api Adds two vectors\n    fn add() = 0\n@api Multiplies"
      val doc = parser.parse(source)
      val result = LiterateRenderer.renderApi(doc)
      result should include("Adds two vectors")
      result should include("Multiplies")
      result should not include "Some prose."
      result should not include "fn add"
    }

    "handles multi-line api blocks" in {
      val source = "@api\nAdds two vectors.\n\n@param a First\n@"
      val doc = parser.parse(source)
      val result = LiterateRenderer.renderApi(doc)
      result should include("Adds two vectors.")
      result should include("@param a First")
    }
  }
}
