package io.github.edadma.trisc

import scala.util.parsing.combinator.RegexParsers

case class MemoryRegion(name: String, base: Long, size: Long)

enum SectionPlacement:
  case At(address: Long)
  case After(sectionName: String)

case class SectionDef(name: String, placement: SectionPlacement)

case class LinkerScript(
    memory: Seq[MemoryRegion] = Nil,
    sections: Seq[SectionDef] = Nil,
    entry: Option[String] = None,
)

object LinkerScriptParser extends RegexParsers:
  override def skipWhitespace = false

  private def ws: Parser[String] = "[ \t]*".r
  private def ws1: Parser[String] = "[ \t]+".r
  private def nl: Parser[Any] = ws ~> "\n"
  private def blankLines: Parser[Any] = rep(ws ~> "\n")
  private def comment: Parser[Any] = ws ~> "#" ~> ".*".r
  private def emptyLine: Parser[Any] = comment | (ws ~> "\n")
  private def skipLines: Parser[Any] = rep(emptyLine)

  private def hexNum: Parser[Long] =
    "0x[0-9a-fA-F]+".r ^^ (s => java.lang.Long.parseLong(s.drop(2), 16))

  private def decNum: Parser[Long] =
    "[0-9]+".r ^^ (_.toLong)

  private def num: Parser[Long] = hexNum | decNum

  private def ident: Parser[String] = "[a-zA-Z_.][a-zA-Z0-9_.]*".r

  private def memoryRegion: Parser[MemoryRegion] =
    ws1 ~> ident ~ (ws ~> ":" ~> ws ~> num) ~ (ws ~> "," ~> ws ~> num) <~ (comment | nl) ^^ {
      case name ~ base ~ size => MemoryRegion(name, base, size)
    }

  private def memoryBlock: Parser[Seq[MemoryRegion]] =
    ws ~> "MEMORY" ~> nl ~> rep(memoryRegion)

  private def sectionPlacement: Parser[SectionPlacement] =
    "AFTER" ~> ws1 ~> ident ^^ SectionPlacement.After.apply |
      num ^^ SectionPlacement.At.apply

  private def sectionDef: Parser[SectionDef] =
    ws1 ~> ident ~ (ws ~> ":" ~> ws ~> sectionPlacement) <~ (comment | nl) ^^ {
      case name ~ placement => SectionDef(name, placement)
    }

  private def sectionsBlock: Parser[Seq[SectionDef]] =
    ws ~> "SECTIONS" ~> nl ~> rep(sectionDef)

  private def entryLine: Parser[String] =
    ws ~> "ENTRY" ~> ws1 ~> ident <~ (comment | nl)

  private def block: Parser[Any] =
    memoryBlock | sectionsBlock | entryLine | emptyLine

  private def script: Parser[LinkerScript] =
    skipLines ~> rep(block) <~ ws ^^ { blocks =>
      var memory = Seq.empty[MemoryRegion]
      var sections = Seq.empty[SectionDef]
      var entry: Option[String] = None

      for b <- blocks do
        b match
          case ms: Seq[?] if ms.headOption.exists(_.isInstanceOf[MemoryRegion]) =>
            memory = ms.asInstanceOf[Seq[MemoryRegion]]
          case ss: Seq[?] if ss.headOption.exists(_.isInstanceOf[SectionDef]) =>
            sections = ss.asInstanceOf[Seq[SectionDef]]
          case s: String => entry = Some(s)
          case _         =>

      LinkerScript(memory, sections, entry)
    }

  def parse(input: String): Either[String, LinkerScript] =
    // Ensure input ends with newline for parser
    val src = if input.endsWith("\n") then input else input + "\n"
    parseAll(script, src) match
      case Success(result, _) => Right(result)
      case failure: NoSuccess => Left(failure.msg)
