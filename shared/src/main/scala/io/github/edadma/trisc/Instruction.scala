package io.github.edadma.trisc

abstract class Instruction extends (CPU => Unit):
  val mnemonic: String

  def disassemble(cpu: CPU): String

abstract class SimpleInstruction extends Instruction:
  def disassemble(cpu: CPU): String = mnemonic

object IllegalInstruction extends SimpleInstruction:
  val mnemonic = "Illegal"

  def apply(cpu: CPU): Unit = sys.error("illegal instruction")

abstract class ImmediateInstruction(r: Int, imm: Int) extends Instruction:
  def disassemble(cpu: CPU): String = f"$mnemonic r$r, 0x$imm%02x ($imm)"

class LDI(r: Int, imm: Int) extends ImmediateInstruction(r, imm):
  val mnemonic = "ldi"

  def apply(cpu: CPU): Unit = cpu.r(r).write(imm)

class SLI(r: Int, imm: Int) extends ImmediateInstruction(r, imm):
  val mnemonic = "sli"

  def apply(cpu: CPU): Unit = cpu.r(r).write((cpu.r(r).read << 8) | imm)

class STI(r: Int, imm: Int) extends ImmediateInstruction(r, imm):
  val mnemonic = "sti"

  def apply(cpu: CPU): Unit = cpu.writeByte(cpu.r(r).read, imm)

class JALR(a: Int, b: Int) extends SimpleInstruction:
  val mnemonic = "jalr"

  def apply(cpu: CPU): Unit =
    cpu.r(a).write(cpu.pc)
    cpu.pc = cpu.r(b).read

class TRAP(imm: Int) extends Instruction:
  val mnemonic = "trap"

  def disassemble(cpu: CPU): String = s"$mnemonic $imm"

  def apply(cpu: CPU): Unit = cpu.state = State.fromOrdinal(State.Trap0.ordinal + imm)

object HALT extends SimpleInstruction:
  val mnemonic = "halt"

  def apply(cpu: CPU): Unit = cpu.state = State.Halt

object RTE extends SimpleInstruction:
  val mnemonic = "rte"

  def apply(cpu: CPU): Unit =
    for i <- 1 to 7 do cpu.r(i).write(cpu.sr(i))
    cpu.pc = cpu.spc
    cpu.psr = cpu.spsr

class SPSR(r: Int) extends SimpleInstruction:
  val mnemonic = "spsr"

  def apply(cpu: CPU): Unit = cpu.psr = cpu.r(r).read.toInt

class GPSR(r: Int) extends SimpleInstruction:
  val mnemonic = "gpsr"

  def apply(cpu: CPU): Unit = cpu.r(r).write(cpu.psr & 0xffffffff)

abstract class ImmediateSignedInstruction(a: Int, b: Int, imm: Int) extends Instruction:
  def disassemble(cpu: CPU): String = f"$mnemonic r$a, r$b, 0x$imm%02x ($imm)"

class ADDI(a: Int, b: Int, imm: Int) extends ImmediateSignedInstruction(a, b, imm):
  val mnemonic = "addi"

  def apply(cpu: CPU): Unit = cpu.r(a).write(cpu.r(b).read + imm)

abstract class BranchInstruction(a: Int, b: Int, imm: Int) extends Instruction:
  def disassemble(cpu: CPU): String = f"$mnemonic r$a, r$b, 0x${cpu.pc + 2 + imm * 2}%04x ($imm)"

class BLS(a: Int, b: Int, imm: Int) extends BranchInstruction(a, b, imm):
  val mnemonic = "bls"

  def apply(cpu: CPU): Unit = if cpu.r(a).read < cpu.r(b).read then cpu.pc += imm * 2

class BLU(a: Int, b: Int, imm: Int) extends BranchInstruction(a, b, imm):
  val mnemonic = "blu"

  def apply(cpu: CPU): Unit =
    val ua = cpu.r(a).read + Long.MinValue
    val ub = cpu.r(b).read + Long.MinValue
    if ua < ub then cpu.pc += imm * 2

class BEQ(a: Int, b: Int, imm: Int) extends BranchInstruction(a, b, imm):
  val mnemonic = "beq"

  def apply(cpu: CPU): Unit = if cpu.r(a).read == cpu.r(b).read then cpu.pc += imm * 2

abstract class RRRInstruction(a: Int, b: Int, c: Int) extends Instruction:
  def disassemble(cpu: CPU): String = s"$mnemonic r$a, r$b, r$c"

class ADD(d: Int, a: Int, b: Int) extends RRRInstruction(d, a, b):
  val mnemonic = "add"

  def apply(cpu: CPU): Unit = cpu.r(d).write(cpu.r(a).read + cpu.r(b).read)

class SUB(d: Int, a: Int, b: Int) extends RRRInstruction(d, a, b):
  val mnemonic = "sub"

  def apply(cpu: CPU): Unit = cpu.r(d).write(cpu.r(a).read - cpu.r(b).read)

class MUL(d: Int, a: Int, b: Int) extends RRRInstruction(d, a, b):
  val mnemonic = "mul"

  def apply(cpu: CPU): Unit = cpu.r(d).write(cpu.r(a).read * cpu.r(b).read)

class DIV(d: Int, a: Int, b: Int) extends RRRInstruction(d, a, b):
  val mnemonic = "div"

  def apply(cpu: CPU): Unit = cpu.r(d).write(cpu.r(a).read / cpu.r(b).read)

class REM(d: Int, a: Int, b: Int) extends RRRInstruction(d, a, b):
  val mnemonic = "rem"

  def apply(cpu: CPU): Unit = cpu.r(d).write(cpu.r(a).read % cpu.r(b).read)

class AND(d: Int, a: Int, b: Int) extends RRRInstruction(d, a, b):
  val mnemonic = "and"

  def apply(cpu: CPU): Unit = cpu.r(d).write(cpu.r(a).read & cpu.r(b).read)

class OR(d: Int, a: Int, b: Int) extends RRRInstruction(d, a, b):
  val mnemonic = "or"

  def apply(cpu: CPU): Unit = cpu.r(d).write(cpu.r(a).read | cpu.r(b).read)

class XOR(d: Int, a: Int, b: Int) extends RRRInstruction(d, a, b):
  val mnemonic = "xor"

  def apply(cpu: CPU): Unit = cpu.r(d).write(cpu.r(a).read ^ cpu.r(b).read)

class SLT(d: Int, a: Int, b: Int) extends RRRInstruction(d, a, b):
  val mnemonic = "slt"

  def apply(cpu: CPU): Unit = cpu.r(d).write(if cpu.r(a).read < cpu.r(b).read then 1 else 0)

class SLTU(d: Int, a: Int, b: Int) extends RRRInstruction(d, a, b):
  val mnemonic = "sltu"

  def apply(cpu: CPU): Unit =
    val ua = cpu.r(a).read + Long.MinValue
    val ub = cpu.r(b).read + Long.MinValue
    cpu.r(d).write(if ua < ub then 1 else 0)

// Shifts (RRR 001 block)

class ASR(d: Int, a: Int, b: Int) extends RRRInstruction(d, a, b):
  val mnemonic = "asr"

  def apply(cpu: CPU): Unit = cpu.r(d).write(cpu.r(a).read >> cpu.r(b).read.toInt)

class LSR(d: Int, a: Int, b: Int) extends RRRInstruction(d, a, b):
  val mnemonic = "lsr"

  def apply(cpu: CPU): Unit = cpu.r(d).write(cpu.r(a).read >>> cpu.r(b).read.toInt)

class LSL(d: Int, a: Int, b: Int) extends RRRInstruction(d, a, b):
  val mnemonic = "lsl"

  def apply(cpu: CPU): Unit = cpu.r(d).write(cpu.r(a).read << cpu.r(b).read.toInt)

// Unary ops (RR 110 block)

abstract class RRInstruction(a: Int, b: Int) extends Instruction:
  def disassemble(cpu: CPU): String = s"$mnemonic r$a, r$b"

class ZEB(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "zeb"

  def apply(cpu: CPU): Unit = cpu.r(a).write(cpu.r(b).read & 0xff)

class ZES(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "zes"

  def apply(cpu: CPU): Unit = cpu.r(a).write(cpu.r(b).read & 0xffff)

class ZEW(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "zew"

  def apply(cpu: CPU): Unit = cpu.r(a).write(cpu.r(b).read & 0xffffffffL)

class SEB(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "seb"

  def apply(cpu: CPU): Unit = cpu.r(a).write(cpu.r(b).read.toByte.toLong)

class SES(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "ses"

  def apply(cpu: CPU): Unit = cpu.r(a).write(cpu.r(b).read.toShort.toLong)

class SEW(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "sew"

  def apply(cpu: CPU): Unit = cpu.r(a).write(cpu.r(b).read.toInt.toLong)

class NEG(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "neg"

  def apply(cpu: CPU): Unit = cpu.r(a).write(-cpu.r(b).read)

class NOT(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "not"

  def apply(cpu: CPU): Unit = cpu.r(a).write(~cpu.r(b).read)

// Stack ops (R format — use r7 as stack pointer)

abstract class RInstruction(r: Int) extends Instruction:
  def disassemble(cpu: CPU): String = s"$mnemonic r$r"

class PSHB(r: Int) extends RInstruction(r):
  val mnemonic = "pshb"

  def apply(cpu: CPU): Unit =
    cpu.r(7).write(cpu.r(7).read - 1)
    cpu.writeByte(cpu.r(7).read, cpu.r(r).read)

class POPB(r: Int) extends RInstruction(r):
  val mnemonic = "popb"

  def apply(cpu: CPU): Unit =
    cpu.r(r).write(cpu.readByte(cpu.r(7).read))
    cpu.r(7).write(cpu.r(7).read + 1)

class PSHS(r: Int) extends RInstruction(r):
  val mnemonic = "pshs"

  def apply(cpu: CPU): Unit =
    cpu.r(7).write(cpu.r(7).read - 2)
    cpu.writeShort(cpu.r(7).read, cpu.r(r).read)

class POPS(r: Int) extends RInstruction(r):
  val mnemonic = "pops"

  def apply(cpu: CPU): Unit =
    cpu.r(r).write(cpu.readShort(cpu.r(7).read))
    cpu.r(7).write(cpu.r(7).read + 2)

class PSHW(r: Int) extends RInstruction(r):
  val mnemonic = "pshw"

  def apply(cpu: CPU): Unit =
    cpu.r(7).write(cpu.r(7).read - 4)
    cpu.writeInt(cpu.r(7).read, cpu.r(r).read)

class POPW(r: Int) extends RInstruction(r):
  val mnemonic = "popw"

  def apply(cpu: CPU): Unit =
    cpu.r(r).write(cpu.readInt(cpu.r(7).read))
    cpu.r(7).write(cpu.r(7).read + 4)

class PSHD(r: Int) extends RInstruction(r):
  val mnemonic = "pshd"

  def apply(cpu: CPU): Unit =
    cpu.r(7).write(cpu.r(7).read - 8)
    cpu.writeLong(cpu.r(7).read, cpu.r(r).read)

class POPD(r: Int) extends RInstruction(r):
  val mnemonic = "popd"

  def apply(cpu: CPU): Unit =
    cpu.r(r).write(cpu.readLong(cpu.r(7).read))
    cpu.r(7).write(cpu.r(7).read + 8)

// Floating point RRR (001 block)

class FADD(d: Int, a: Int, b: Int) extends RRRInstruction(d, a, b):
  val mnemonic = "fadd"

  def apply(cpu: CPU): Unit = cpu.r(d).write(cpu.r(a).readf + cpu.r(b).readf)

class FSUB(d: Int, a: Int, b: Int) extends RRRInstruction(d, a, b):
  val mnemonic = "fsub"

  def apply(cpu: CPU): Unit = cpu.r(d).write(cpu.r(a).readf - cpu.r(b).readf)

class FMUL(d: Int, a: Int, b: Int) extends RRRInstruction(d, a, b):
  val mnemonic = "fmul"

  def apply(cpu: CPU): Unit = cpu.r(d).write(cpu.r(a).readf * cpu.r(b).readf)

class FDIV(d: Int, a: Int, b: Int) extends RRRInstruction(d, a, b):
  val mnemonic = "fdiv"

  def apply(cpu: CPU): Unit = cpu.r(d).write(cpu.r(a).readf / cpu.r(b).readf)

// Floating point RR (110 block)

class FNEG(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "fneg"

  def apply(cpu: CPU): Unit = cpu.r(a).write(-cpu.r(b).readf)

class FINV(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "finv"

  def apply(cpu: CPU): Unit = cpu.r(a).write(1.0 / cpu.r(b).readf)

class AUIPC(r: Int, imm: Int) extends ImmediateInstruction(r, imm):
  val mnemonic = "auipc"

  def apply(cpu: CPU): Unit = cpu.r(r).write(cpu.pc - 2 + (imm << 8))

class LDB(d: Int, a: Int, b: Int) extends RRRInstruction(d, a, b):
  val mnemonic = "ldb"

  def apply(cpu: CPU): Unit = cpu.r(d).write(cpu.readByte(cpu.r(a).read + cpu.r(b).read))

abstract class LDSTInstruction(a: Int, b: Int, imm: Int) extends Instruction:
  def disassemble(cpu: CPU): String = f"$mnemonic r$a, r$b, 0x${imm * 2}%02x (${imm * 2})"

class LD(a: Int, b: Int, imm: Int) extends LDSTInstruction(a, b, imm):
  val mnemonic = "ld"

  def apply(cpu: CPU): Unit = cpu.r(a).write(cpu.readInt(cpu.r(b).read + imm * 2))

class ST(a: Int, b: Int, imm: Int) extends LDSTInstruction(a, b, imm):
  val mnemonic = "st"

  def apply(cpu: CPU): Unit = cpu.writeInt(cpu.r(b).read + imm * 2, cpu.r(a).read)

class STB(a: Int, b: Int, c: Int) extends RRRInstruction(a, b, c):
  val mnemonic = "stb"

  def apply(cpu: CPU): Unit = cpu.writeByte(cpu.r(b).read + cpu.r(c).read, cpu.r(a).read)

class LDS(d: Int, a: Int, b: Int) extends RRRInstruction(d, a, b):
  val mnemonic = "lds"

  def apply(cpu: CPU): Unit = cpu.r(d).write(cpu.readShort(cpu.r(a).read + cpu.r(b).read))

class STS(a: Int, b: Int, c: Int) extends RRRInstruction(a, b, c):
  val mnemonic = "sts"

  def apply(cpu: CPU): Unit = cpu.writeShort(cpu.r(b).read + cpu.r(c).read, cpu.r(a).read)

class LDW(d: Int, a: Int, b: Int) extends RRRInstruction(d, a, b):
  val mnemonic = "ldw"

  def apply(cpu: CPU): Unit = cpu.r(d).write(cpu.readInt(cpu.r(a).read + cpu.r(b).read))

class STW(a: Int, b: Int, c: Int) extends RRRInstruction(a, b, c):
  val mnemonic = "stw"

  def apply(cpu: CPU): Unit = cpu.writeInt(cpu.r(b).read + cpu.r(c).read, cpu.r(a).read)

class LDD(d: Int, a: Int, b: Int) extends RRRInstruction(d, a, b):
  val mnemonic = "ldd"

  def apply(cpu: CPU): Unit = cpu.r(d).write(cpu.readLong(cpu.r(a).read + cpu.r(b).read))

class STD(a: Int, b: Int, c: Int) extends RRRInstruction(a, b, c):
  val mnemonic = "std"

  def apply(cpu: CPU): Unit = cpu.writeLong(cpu.r(b).read + cpu.r(c).read, cpu.r(a).read)
