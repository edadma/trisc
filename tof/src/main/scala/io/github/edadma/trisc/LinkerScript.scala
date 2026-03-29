package io.github.edadma.trisc

import scala.collection.mutable
import scala.util.parsing.combinator.RegexParsers

case class MemoryRegion(name: String, base: Long, size: Long)

enum SectionPlacement:
  case At(address: Long)
  case After(sectionName: String)

case class SectionDef(name: String, placement: SectionPlacement)

enum SymbolValue:
  case Absolute(address: Long)
  case AfterSection(sectionName: String)

case class SymbolDef(name: String, value: SymbolValue)

case class LinkerScript(
    memory: Seq[MemoryRegion] = Nil,
    sections: Seq[SectionDef] = Nil,
    symbols: Seq[SymbolDef] = Nil,
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

  private def symbolValue: Parser[SymbolValue] =
    "AFTER" ~> ws1 ~> ident ^^ SymbolValue.AfterSection.apply |
      num ^^ SymbolValue.Absolute.apply

  private def symbolLine: Parser[SymbolDef] =
    ws ~> "SYMBOL" ~> ws1 ~> ident ~ (ws ~> "=" ~> ws ~> symbolValue) <~ (comment | nl) ^^ {
      case name ~ value => SymbolDef(name, value)
    }

  private def entryLine: Parser[String] =
    ws ~> "ENTRY" ~> ws1 ~> ident <~ (comment | nl)

  private def block: Parser[Any] =
    memoryBlock | sectionsBlock | symbolLine | entryLine | emptyLine

  private def script: Parser[LinkerScript] =
    skipLines ~> rep(block) <~ ws ^^ { blocks =>
      var memory = Seq.empty[MemoryRegion]
      var sections = Seq.empty[SectionDef]
      val symbols = new mutable.ListBuffer[SymbolDef]
      var entry: Option[String] = None

      for b <- blocks do
        b match
          case ms: Seq[?] if ms.headOption.exists(_.isInstanceOf[MemoryRegion]) =>
            memory = ms.asInstanceOf[Seq[MemoryRegion]]
          case ss: Seq[?] if ss.headOption.exists(_.isInstanceOf[SectionDef]) =>
            sections = ss.asInstanceOf[Seq[SectionDef]]
          case sd: SymbolDef => symbols += sd
          case s: String => entry = Some(s)
          case _         =>

      LinkerScript(memory, sections, symbols.toSeq, entry)
    }

  def parse(input: String): Either[String, LinkerScript] =
    // Ensure input ends with newline for parser
    val src = if input.endsWith("\n") then input else input + "\n"
    parseAll(script, src) match
      case Success(result, _) => Right(result)
      case failure: NoSuccess => Left(failure.msg)
