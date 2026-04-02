package io.github.edadma.trisc

import io.github.edadma.markdown.*

class LiterateParser:

  case class ParseError(msg: String) extends RuntimeException(msg)

  def parse(source: String): LiterateDocument =
    if source.contains('\t') then
      throw ParseError("tabs are not allowed in literate source files")

    val config = MarkdownConfig(indentedCodeBreaksList = true)
    val doc = parseDocumentContent(source, config)
    val codeBlocks = doc.children.collect {
      case Code(content, _, true) => content
    }

    LiterateDocument(codeBlocks)
