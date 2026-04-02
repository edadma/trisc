package io.github.edadma.trisc

import io.github.edadma.markdown.*
import io.github.edadma.highlighter.*

object LiterateRenderer:

  def tangle(doc: LiterateDocument): String =
    doc.codeBlocks.mkString("\n")

  private lazy val highlighterCache: Map[String, Highlighter] =
    Grammars.grammars.flatMap { case (lang, json) =>
      Highlighter.fromJson(json, InlineMode(Theme.OneDark)).toOption.map(lang -> _)
    }

  private val codeHighlighter: (String, String) => Option[String] = (code, lang) =>
    val resolved = Grammars.aliases.getOrElse(lang, lang)
    highlighterCache.get(resolved).map(_.highlight(code))

  private val htmlConfig = MarkdownConfig.all.copy(
    codeHighlighter = Some(codeHighlighter),
    indentedCodeLanguage = Some("sysl"),
    indentedCodeBreaksList = true,
  )

  def renderHTML(source: String): String =
    renderToHTML(source, htmlConfig)
