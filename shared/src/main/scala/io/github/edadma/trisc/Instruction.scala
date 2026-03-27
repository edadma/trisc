package io.github.edadma.trisc

abstract class Instruction extends (CPU => Unit):
  val mnemonic: String

  def disassemble(cpu: CPU): String

abstract class SimpleInstruction extends Instruction:
  def disassemble(cpu: CPU): String = mnemonic

object IllegalInstruction extends SimpleInstruction:
  val mnemonic = "Illegal"

  def apply(cpu: CPU): Unit = cpu.state = State.UnimplementedOpcode

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

  def apply(cpu: CPU): Unit =
    if !cpu.test(Status.Mode) then cpu.state = State.PrivilegeViolation
    else cpu.state = State.Halt

object RTE extends SimpleInstruction:
  val mnemonic = "rte"

  def apply(cpu: CPU): Unit =
    if !cpu.test(Status.Mode) then cpu.state = State.PrivilegeViolation
    else
      // Pop PC then PSR from supervisor stack (reverse of push)
      cpu.pc = cpu.readLong(cpu.r(7).read)
      cpu.r(7).write(cpu.r(7).read + 8)
      cpu.psr = cpu.readLong(cpu.r(7).read).toInt
      cpu.r(7).write(cpu.r(7).read + 8)
      // Swap r7 <-> usp if returning to user mode
      if !cpu.test(Status.Mode) then
        val tmp = cpu.r(7).read
        cpu.r(7).write(cpu.usp)
        cpu.usp = tmp

class SPSR(r: Int) extends SimpleInstruction:
  val mnemonic = "spsr"

  def apply(cpu: CPU): Unit =
    if !cpu.test(Status.Mode) then cpu.state = State.PrivilegeViolation
    else cpu.psr = cpu.r(r).read.toInt

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

  def apply(cpu: CPU): Unit =
    val va = cpu.r(a).read
    val vb = cpu.r(b).read
    val result = va + vb
    cpu.r(d).write(result)
    // Carry: unsigned overflow if result < either operand
    cpu.set(Status.C, (result + Long.MinValue) < (va + Long.MinValue))
    // Overflow: signed overflow if same-sign inputs produce different-sign result
    cpu.set(Status.V, ((~(va ^ vb) & (va ^ result)) < 0))

class SUB(d: Int, a: Int, b: Int) extends RRRInstruction(d, a, b):
  val mnemonic = "sub"

  def apply(cpu: CPU): Unit =
    val va = cpu.r(a).read
    val vb = cpu.r(b).read
    val result = va - vb
    cpu.r(d).write(result)
    // Borrow: unsigned underflow if a < b
    cpu.set(Status.C, (va + Long.MinValue) < (vb + Long.MinValue))
    // Overflow: signed overflow if different-sign inputs and result differs from va
    cpu.set(Status.V, (((va ^ vb) & (va ^ result)) < 0))

class MUL(d: Int, a: Int, b: Int) extends RRRInstruction(d, a, b):
  val mnemonic = "mul"

  def apply(cpu: CPU): Unit =
    val va = cpu.r(a).read
    val vb = cpu.r(b).read
    cpu.r(d).write(va * vb)
    cpu.r((d + 1) & 7).write(java.lang.Math.multiplyHigh(va, vb))

class DIV(d: Int, a: Int, b: Int) extends RRRInstruction(d, a, b):
  val mnemonic = "div"

  def apply(cpu: CPU): Unit =
    if cpu.r(b).read == 0 then cpu.state = State.IllegalDivide
    else cpu.r(d).write(cpu.r(a).read / cpu.r(b).read)

class REM(d: Int, a: Int, b: Int) extends RRRInstruction(d, a, b):
  val mnemonic = "rem"

  def apply(cpu: CPU): Unit =
    if cpu.r(b).read == 0 then cpu.state = State.IllegalDivide
    else cpu.r(d).write(cpu.r(a).read % cpu.r(b).read)

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

// Carry arithmetic (RRR 001 block)

class ADC(d: Int, a: Int, b: Int) extends RRRInstruction(d, a, b):
  val mnemonic = "adc"

  def apply(cpu: CPU): Unit =
    val va = cpu.r(a).read
    val vb = cpu.r(b).read
    val carry = if cpu.test(Status.C) then 1L else 0L
    val result = va + vb + carry
    cpu.r(d).write(result)
    // Carry if (va + vb) overflowed, or (va + vb + carry) overflowed
    val sum1 = va + vb
    val overflow1 = (sum1 + Long.MinValue) < (va + Long.MinValue)
    val overflow2 = (result + Long.MinValue) < (sum1 + Long.MinValue)
    cpu.set(Status.C, overflow1 || overflow2)
    // Signed overflow: same-sign inputs produce different-sign result
    cpu.set(Status.V, ((~(va ^ vb) & (va ^ result)) < 0))

class SBC(d: Int, a: Int, b: Int) extends RRRInstruction(d, a, b):
  val mnemonic = "sbc"

  def apply(cpu: CPU): Unit =
    val va = cpu.r(a).read
    val vb = cpu.r(b).read
    val borrow = if cpu.test(Status.C) then 1L else 0L
    val result = va - vb - borrow
    cpu.r(d).write(result)
    // Borrow if a < b, or a == b and borrow was set
    val ua = va + Long.MinValue
    val ub = vb + Long.MinValue
    cpu.set(Status.C, ua < ub || (ua == ub && borrow != 0))
    // Signed overflow: different-sign inputs and result differs from va
    cpu.set(Status.V, (((va ^ vb) & (va ^ result)) < 0))

// Unsigned arithmetic (RRR 001 block)

class MULU(d: Int, a: Int, b: Int) extends RRRInstruction(d, a, b):
  val mnemonic = "mulu"

  def apply(cpu: CPU): Unit =
    val va = cpu.r(a).read
    val vb = cpu.r(b).read
    cpu.r(d).write(va * vb)
    // Unsigned multiply high: multiplyHigh gives signed high, correct for unsigned
    val hi = java.lang.Math.multiplyHigh(va, vb) + (if va < 0 then vb else 0L) + (if vb < 0 then va else 0L)
    cpu.r((d + 1) & 7).write(hi)

class DIVU(d: Int, a: Int, b: Int) extends RRRInstruction(d, a, b):
  val mnemonic = "divu"

  def apply(cpu: CPU): Unit =
    if cpu.r(b).read == 0 then cpu.state = State.IllegalDivide
    else cpu.r(d).write(java.lang.Long.divideUnsigned(cpu.r(a).read, cpu.r(b).read))

class REMU(d: Int, a: Int, b: Int) extends RRRInstruction(d, a, b):
  val mnemonic = "remu"

  def apply(cpu: CPU): Unit =
    if cpu.r(b).read == 0 then cpu.state = State.IllegalDivide
    else cpu.r(d).write(java.lang.Long.remainderUnsigned(cpu.r(a).read, cpu.r(b).read))

// Float comparison (RRR 001 block)

class FSLT(d: Int, a: Int, b: Int) extends RRRInstruction(d, a, b):
  val mnemonic = "fslt"

  def apply(cpu: CPU): Unit = cpu.r(d).write(if cpu.r(a).readf < cpu.r(b).readf then 1 else 0)

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

  def apply(cpu: CPU): Unit =
    val v = cpu.r(b).read
    cpu.r(a).write(-v)
    // Overflow only when negating Long.MinValue (result == Long.MinValue)
    cpu.set(Status.V, v == Long.MinValue)

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

class FPOW(d: Int, a: Int, b: Int) extends RRRInstruction(d, a, b):
  val mnemonic = "fpow"

  def apply(cpu: CPU): Unit = cpu.r(d).write(math.pow(cpu.r(a).readf, cpu.r(b).readf))

// Floating point RR (110 block)

class FNEG(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "fneg"

  def apply(cpu: CPU): Unit = cpu.r(a).write(-cpu.r(b).readf)

class FINV(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "finv"

  def apply(cpu: CPU): Unit = cpu.r(a).write(1.0 / cpu.r(b).readf)

class CVT(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "cvt"

  def apply(cpu: CPU): Unit = cpu.r(a).write(cpu.r(b).read.toDouble)

class FINT(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "fint"

  def apply(cpu: CPU): Unit = cpu.r(a).write(cpu.r(b).readf.toLong)

class FSQRT(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "fsqrt"

  def apply(cpu: CPU): Unit = cpu.r(a).write(math.sqrt(cpu.r(b).readf))

class FABS(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "fabs"

  def apply(cpu: CPU): Unit = cpu.r(a).write(math.abs(cpu.r(b).readf))

// Atomics (RR 110 block)

class LL(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "ll"

  def apply(cpu: CPU): Unit =
    val addr = cpu.r(b).read
    cpu.r(a).write(cpu.readLong(addr))
    cpu.reservationAddr = addr
    cpu.reservationValid = true

class SC(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "sc"

  def apply(cpu: CPU): Unit =
    val addr = cpu.r(b).read
    if cpu.reservationValid && cpu.reservationAddr == addr then
      cpu.writeLong(addr, cpu.r(a).read)
      cpu.r(a).write(1)
    else
      cpu.r(a).write(0)
    cpu.reservationValid = false

// Bit counting (RR 110 block)

class CLZ(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "clz"

  def apply(cpu: CPU): Unit = cpu.r(a).write(java.lang.Long.numberOfLeadingZeros(cpu.r(b).read))

class CTZ(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "ctz"

  def apply(cpu: CPU): Unit = cpu.r(a).write(java.lang.Long.numberOfTrailingZeros(cpu.r(b).read))

// System (R format)

object FENCE extends SimpleInstruction:
  val mnemonic = "fence"

  def apply(cpu: CPU): Unit = () // memory ordering barrier — NOP for single-core emulator

object TRAPV extends SimpleInstruction:
  val mnemonic = "trapv"

  def apply(cpu: CPU): Unit =
    if cpu.test(Status.V) then cpu.state = State.Overflow

class CHK(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "chk"

  def apply(cpu: CPU): Unit =
    val va = cpu.r(a).read
    val vb = cpu.r(b).read
    if va < 0 || va > vb then cpu.state = State.BoundsCheck

object WFI extends SimpleInstruction:
  val mnemonic = "wfi"

  def apply(cpu: CPU): Unit =
    if !cpu.test(Status.Mode) then cpu.state = State.PrivilegeViolation
    else cpu.state = State.Wfi

class GUSP(r: Int) extends RInstruction(r):
  val mnemonic = "gusp"

  def apply(cpu: CPU): Unit =
    if !cpu.test(Status.Mode) then cpu.state = State.PrivilegeViolation
    else cpu.r(r).write(cpu.usp)

class SUSP(r: Int) extends RInstruction(r):
  val mnemonic = "susp"

  def apply(cpu: CPU): Unit =
    if !cpu.test(Status.Mode) then cpu.state = State.PrivilegeViolation
    else cpu.usp = cpu.r(r).read

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
