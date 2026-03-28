package io.github.edadma.trisc

object LiterateRenderer:

  def tangle(doc: LiterateDocument): String =
    doc.blocks.collect { case CodeBlock(content, _) => content }.mkString("\n")

  def renderLiterate(doc: LiterateDocument, channel: ChannelConfig): String =
    val parts = doc.blocks.flatMap {
      case CodeBlock(content, _) if channel.includeTags.contains("code") =>
        Some(content)
      case ProseBlock(tag, _, content, _) if channel.includeTags.contains(tag) =>
        Some(content)
      case _ => None
    }
    parts.mkString("\n\n")

  def renderProse(doc: LiterateDocument, channel: ChannelConfig): String =
    val parts = doc.blocks.collect {
      case ProseBlock(tag, _, content, _) if channel.includeTags.contains(tag) => content
    }
    parts.mkString("\n\n")

  def renderApi(doc: LiterateDocument): String =
    val parts = doc.blocks.collect {
      case ApiBlock(content, _) => content
    }
    parts.mkString("\n\n")
