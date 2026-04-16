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

  /** Generate an index page linking to source HTML files.
    * @param title module/directory name
    * @param files sequence of (name, relativePath) pairs, sorted
    */
  def renderIndex(title: String, files: Seq[(String, String)]): String =
    val links = files.map { case (name, href) =>
      s"""<li><a href="$href">$name</a></li>"""
    }.mkString("\n")
    val body = s"""<h1>$title</h1>
                  |<ul class="file-list">
                  |$links
                  |</ul>""".stripMargin
    renderPageShell(title, body)

  def renderPage(source: String, title: String, backLink: Option[String] = None): String =
    val nav = backLink.map(href => s"""<nav class="back"><a href="$href">&larr; Index</a></nav>\n""").getOrElse("")
    val body = nav + renderHTML(source)
    renderPageShell(title, body)

  private def renderPageShell(title: String, body: String): String =
    s"""<!DOCTYPE html>
       |<html lang="en">
       |<head>
       |<meta charset="UTF-8">
       |<meta name="viewport" content="width=device-width, initial-scale=1.0">
       |<title>$title</title>
       |<style>
       |:root {
       |  --bg: #ffffff;
       |  --fg: #24292e;
       |  --code-bg: #282c34;
       |  --code-fg: #abb2bf;
       |  --border: #e1e4e8;
       |  --link: #0366d6;
       |  --block-bg: #f6f8fa;
       |}
       |@media (prefers-color-scheme: dark) {
       |  :root {
       |    --bg: #1e1e1e;
       |    --fg: #d4d4d4;
       |    --border: #3e3e3e;
       |    --link: #58a6ff;
       |    --block-bg: #2d2d2d;
       |  }
       |}
       |* { margin: 0; padding: 0; box-sizing: border-box; }
       |body {
       |  font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Helvetica, Arial, sans-serif;
       |  font-size: 16px;
       |  line-height: 1.6;
       |  color: var(--fg);
       |  background: var(--bg);
       |  max-width: 52em;
       |  margin: 0 auto;
       |  padding: 2em 1.5em;
       |}
       |h1, h2, h3, h4, h5, h6 {
       |  margin-top: 1.5em;
       |  margin-bottom: 0.5em;
       |  line-height: 1.25;
       |}
       |h1 { font-size: 2em; border-bottom: 1px solid var(--border); padding-bottom: 0.3em; }
       |h2 { font-size: 1.5em; border-bottom: 1px solid var(--border); padding-bottom: 0.3em; }
       |p { margin-bottom: 1em; }
       |a { color: var(--link); text-decoration: none; }
       |a:hover { text-decoration: underline; }
       |ul, ol { margin-bottom: 1em; padding-left: 2em; }
       |li { margin-bottom: 0.25em; }
       |blockquote {
       |  border-left: 4px solid var(--border);
       |  padding: 0.5em 1em;
       |  margin-bottom: 1em;
       |  color: var(--fg);
       |  background: var(--block-bg);
       |}
       |pre {
       |  background: var(--code-bg);
       |  color: var(--code-fg);
       |  border-radius: 6px;
       |  padding: 1em;
       |  overflow-x: auto;
       |  margin-bottom: 1em;
       |  font-size: 0.9em;
       |  line-height: 1.45;
       |}
       |code {
       |  font-family: "SFMono-Regular", Consolas, "Liberation Mono", Menlo, monospace;
       |}
       |p code, li code {
       |  background: var(--block-bg);
       |  padding: 0.2em 0.4em;
       |  border-radius: 3px;
       |  font-size: 0.9em;
       |}
       |table {
       |  border-collapse: collapse;
       |  margin-bottom: 1em;
       |  width: 100%;
       |}
       |th, td {
       |  border: 1px solid var(--border);
       |  padding: 0.5em 0.75em;
       |  text-align: left;
       |}
       |th { background: var(--block-bg); }
       |hr { border: none; border-top: 1px solid var(--border); margin: 2em 0; }
       |.math.display { text-align: center; margin: 1em 0; }
       |.back { margin-bottom: 1.5em; }
       |.back a { font-size: 0.9em; }
       |.file-list { list-style: none; padding-left: 0; }
       |.file-list li { padding: 0.4em 0; border-bottom: 1px solid var(--border); }
       |.file-list li:last-child { border-bottom: none; }
       |.file-list a { font-family: "SFMono-Regular", Consolas, "Liberation Mono", Menlo, monospace; }
       |</style>
       |<link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.css">
       |<script defer src="https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.js"></script>
       |<script defer src="https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/contrib/auto-render.min.js"
       |  onload="renderMathInElement(document.body, {delimiters:[
       |    {left:'\\\\[',right:'\\\\]',display:true},
       |    {left:'\\\\(',right:'\\\\)',display:false}
       |  ]})"></script>
       |</head>
       |<body>
       |$body
       |</body>
       |</html>""".stripMargin
