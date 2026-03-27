package io.github.edadma.trisc

class Disassembler(mem: Addressable, symbols: Map[Long, String] = Map.empty):

  private val dummyMem: Memory = mem match
    case m: Memory => m
    case a: Addressable => new Memory("disasm", a)
  private val dummyCpu = new CPU(dummyMem, Nil)

  def disassembleAt(addr: Long): (String, Int) =
    val inst = mem.readShortUnsigned(addr)
    val decoded = Decode(inst)
    dummyCpu.pc = addr + 2
    val raw = decoded.disassemble(dummyCpu)
    val formatted = resolveSymbols(raw)
    val prefix = symbols.get(addr).map(name => s"$name:\n").getOrElse("")
    val addrStr = f"$addr%04x"
    (s"$prefix  $addrStr  $formatted", 2)

  def disassembleRange(start: Long, end: Long): String =
    val buf = new StringBuilder
    var addr = start
    while addr < end do
      val (line, size) = disassembleAt(addr)
      buf ++= line
      buf += '\n'
      addr += size
    buf.toString

  def disassembleFunction(name: String): Option[String] =
    val addrOpt = symbols.collectFirst { case (addr, n) if n == name => addr }
    addrOpt.map { start =>
      val buf = new StringBuilder
      var addr = start
      var done = false
      while !done do
        val (line, size) = disassembleAt(addr)
        buf ++= line
        buf += '\n'
        addr += size
        // Stop at halt or jalr r0, rN (return)
        val inst = mem.readShortUnsigned(addr - size)
        val decoded = Decode(inst)
        // halt = jalr r0, r0 (encoding 0xC000)
        // return = jalr r0, rN where first 3 operand bits are 000
        decoded match
          case HALT => done = true
          case _: JALR if (inst & 0xE000) == 0xC000 && ((inst >> 10) & 7) == 0 => done = true
          case _ =>
      buf.toString
    }

  private def resolveSymbols(text: String): String =
    // Replace hex addresses like 0x001a with symbol names if known
    val hexPattern = "0x([0-9a-fA-F]{2,})".r
    hexPattern.replaceAllIn(text, m =>
      val addr = java.lang.Long.parseLong(m.group(1), 16)
      symbols.get(addr) match
        case Some(name) => name
        case None => m.matched
    )

object Disassembler:

  def fromTOF(mem: Addressable, tof: TOF): Disassembler =
    val syms = for
      seg <- tof.segments
      sym <- seg.symbols
    yield (seg.org + sym.offset) -> sym.name
    new Disassembler(mem, syms.toMap)
