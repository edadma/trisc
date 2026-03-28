package io.github.edadma.trisc

import scala.collection.mutable

class LiterateParser:

  case class ParseError(msg: String, line: Int) extends RuntimeException(s"line $line: $msg")

  def parse(source: String): LiterateDocument =
    val lines = source.split("\n", -1).toIndexedSeq
    val blocks = mutable.ListBuffer[LiterateBlock]()
    val sourceLineMap = mutable.Map[Int, Int]() // tangled code line -> .lsysl line
    var codeLineCounter = 0
    var i = 0

    while i < lines.length do
      val line = lines(i)

      if line.isEmpty then
        // Blank line: look ahead to determine context
        i += 1

      else if isIndented(line) then
        // Code block: collect contiguous indented lines (blank lines included)
        val codeLines = mutable.ListBuffer[String]()
        val startLine = i + 1

        while i < lines.length && (isIndented(lines(i)) || lines(i).isEmpty) do
          val codeLine = if lines(i).isEmpty then "" else stripOneLevel(lines(i))
          codeLines += codeLine
          sourceLineMap(codeLineCounter) = i + 1
          codeLineCounter += 1
          i += 1

        // Trim trailing blank lines from code block
        while codeLines.nonEmpty && codeLines.last.isEmpty do
          codeLines.dropRightInPlace(1)
          codeLineCounter -= 1

        if codeLines.nonEmpty then
          blocks += CodeBlock(codeLines.mkString("\n"), startLine)

      else if line.startsWith("@") then
        i = parseTaggedBlock(lines, i, blocks)

      else
        // Prose block (default tag: tech)
        val proseLines = mutable.ListBuffer[String]()
        val startLine = i + 1

        while i < lines.length && !isIndented(lines(i)) && !lines(i).startsWith("@") && lines(i).nonEmpty do
          proseLines += lines(i)
          i += 1

        blocks += ProseBlock("tech", None, proseLines.mkString("\n"), startLine)

    LiterateDocument(blocks.toList, sourceLineMap.toMap)

  private def parseTaggedBlock(lines: IndexedSeq[String], startIdx: Int, blocks: mutable.ListBuffer[LiterateBlock]): Int =
    val line = lines(startIdx)
    val startLine = startIdx + 1
    var i = startIdx

    // Parse tag name from @tag or @tag content
    val tagMatch = """^@(\w[\w-]*)(.*)""".r.findFirstMatchIn(line) match
      case Some(m) => m
      case None =>
        // Lone @ or invalid tag — treat as prose
        blocks += ProseBlock("tech", None, line, startLine)
        return i + 1

    val rawTag = tagMatch.group(1)
    val rest = tagMatch.group(2).trim
    val (tag, format) = parseTagName(rawTag)

    if tag == "api" then
      i = parseApiBlock(lines, i, rest, tag, startLine, blocks)
    else if rest.nonEmpty then
      // Inline tag: @tag some content here
      blocks += ProseBlock(tag, format, rest, startLine)
      i += 1
    else
      // Block tag: @tag\n...\n@
      i += 1
      val contentLines = mutable.ListBuffer[String]()

      while i < lines.length && lines(i).trim != "@" do
        contentLines += lines(i)
        i += 1

      if i < lines.length then
        i += 1 // skip closing @

      blocks += ProseBlock(tag, format, contentLines.mkString("\n"), startLine)

    i

  private def parseApiBlock(
      lines: IndexedSeq[String],
      startIdx: Int,
      rest: String,
      tag: String,
      startLine: Int,
      blocks: mutable.ListBuffer[LiterateBlock],
  ): Int =
    var i = startIdx

    if rest.nonEmpty then
      // Inline @api: @api Some description
      blocks += ApiBlock(rest, startLine)
      i += 1
    else
      // Block @api: @api\n...\n@
      i += 1
      val contentLines = mutable.ListBuffer[String]()

      while i < lines.length && lines(i).trim != "@" do
        contentLines += lines(i)
        i += 1

      if i < lines.length then
        i += 1 // skip closing @

      blocks += ApiBlock(contentLines.mkString("\n"), startLine)

    i

  private def parseTagName(raw: String): (String, Option[String]) =
    val dashIdx = raw.indexOf('-')
    if dashIdx > 0 then (raw.substring(0, dashIdx), Some(raw.substring(dashIdx + 1)))
    else (raw, None)

  private def isIndented(line: String): Boolean =
    line.nonEmpty && (line.charAt(0) == ' ' || line.charAt(0) == '\t')

  private def stripOneLevel(line: String): String =
    if line.startsWith("\t") then line.substring(1)
    else if line.startsWith("    ") then line.substring(4)
    else if line.startsWith("  ") then line.substring(2)
    else if line.startsWith(" ") then line.substring(1)
    else line
