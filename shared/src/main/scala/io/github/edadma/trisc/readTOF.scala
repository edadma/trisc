package io.github.edadma.trisc

import scala.collection.immutable

case class TOFReadError(line: Int, msg: String) extends RuntimeException(s"line $line: $msg")

private val SupportedTOFVersions = immutable.TreeSet("1", "2")

def readTOF(source: String): TOF =
  val b = TOF.builder
  var version = 0
  var lineNum = 0

  def err(msg: String): Nothing = throw TOFReadError(lineNum, msg)

  for rawLine <- source.linesIterator do
    lineNum += 1
    val line = rawLine.trim

    if line.nonEmpty then
      if version == 0 then
        line match
          case s"TOF v$n" if SupportedTOFVersions(n) => version = n.toInt
          case s"TOF v$n"                             => err(s"unsupported TOF version '$n' (supported: ${SupportedTOFVersions.mkString(", ")})")
          case _                                      => err("expected TOF version header")
      else
        line match
          case s"SEGMENT:$rest" => parseSegment(b, rest)
          case s"SYMBOL:$rest"  => parseSymbol(b, rest)
          case s"EXTERN:$name"  => b.addExtern(name)
          case s"RELOC:$rest"   => parseReloc(b, rest)
          case s"DATA:$rest"    => parseData(b, rest)
          case s"RES:$rest"     => parseRes(b, rest)
          case _                => err(s"unrecognized line: $line")

  def parseSegment(b: TOF.TOFBuilder, rest: String): Unit =
    rest.split(",", 2).toSeq match
      case Seq(name, org) =>
        if b.segmentDefined(name) then err(s"duplicate segment '$name'")
        b.segment(name, parseLong(org))
      case _ => err(s"bad SEGMENT line, expected SEGMENT:name,org")

  def parseSymbol(b: TOF.TOFBuilder, rest: String): Unit =
    rest.split(",").toSeq match
      case Seq(name, offset, "func") =>
        b.addSymbol(name, parseLong(offset), SymbolType.Func)
      case Seq(name, offset, "data", size) =>
        b.addSymbol(name, parseLong(offset), SymbolType.Data, Some(parseLong(size)))
      case Seq(name, offset, "data") =>
        b.addSymbol(name, parseLong(offset), SymbolType.Data)
      case Seq(name, offset, "const") =>
        b.addSymbol(name, parseLong(offset), SymbolType.Const)
      case _ => err(s"bad SYMBOL line, expected SYMBOL:name,offset,type[,size]")

  def parseReloc(b: TOF.TOFBuilder, rest: String): Unit =
    rest.split(",", 3).toSeq match
      case Seq(typStr, offset, name) =>
        val typ = typStr match
          case "MOVI2" => RelocType.MOVI2
          case "MOVI3" => RelocType.MOVI3
          case "MOVI4" => RelocType.MOVI4
          case "ABS32" => RelocType.ABS32
          case _       => err(s"unknown relocation type '$typStr'")
        b.addReloc(typ, parseLong(offset), name)
      case _ => err(s"bad RELOC line, expected RELOC:type,offset,symbol")

  def parseData(b: TOF.TOFBuilder, hex: String): Unit =
    if hex.length % 2 != 0 then err("DATA hex string has odd length")
    b ++= hex.grouped(2).map(s => Integer.parseInt(s, 16).toByte)

  def parseRes(b: TOF.TOFBuilder, size: String): Unit =
    b.addRes(Integer.parseInt(size, 16))

  def parseLong(s: String): Long =
    try java.lang.Long.parseLong(s, 16)
    catch case _: NumberFormatException => err(s"invalid hex number '$s'")

  if version == 0 then throw TOFReadError(lineNum, "empty or missing TOF header")

  b.tof
