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
  def disassemble(cpu: CPU): String = f"$mnemonic r$a, r$b, 0x${cpu.pc + imm * 2}%04x ($imm)"

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
    else
      val va = cpu.r(a).read
      val vb = cpu.r(b).read
      cpu.r(d).write(va / vb)
      cpu.r((d + 1) & 7).write(va % vb)

class CAS(d: Int, a: Int, b: Int) extends RRRInstruction(d, a, b):
  val mnemonic = "cas"

  def apply(cpu: CPU): Unit =
    val addr = cpu.r(a).read
    val old = cpu.readLong(addr)
    if old == cpu.r(d).read then cpu.writeLong(addr, cpu.r(b).read)
    cpu.r(d).write(old)

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
    else
      val va = cpu.r(a).read
      val vb = cpu.r(b).read
      cpu.r(d).write(java.lang.Long.divideUnsigned(va, vb))
      cpu.r((d + 1) & 7).write(java.lang.Long.remainderUnsigned(va, vb))

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

class FSEQ(d: Int, a: Int, b: Int) extends RRRInstruction(d, a, b):
  val mnemonic = "fseq"

  def apply(cpu: CPU): Unit = cpu.r(d).write(if cpu.r(a).readf == cpu.r(b).readf then 1 else 0)

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

// Floating point RR 01 (110 block, sub-format 01)

class FPOW(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "fpow"

  def apply(cpu: CPU): Unit = cpu.r(a).write(math.pow(cpu.r(a).readf, cpu.r(b).readf))

class FSIN(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "fsin"

  def apply(cpu: CPU): Unit = cpu.r(a).write(math.sin(cpu.r(b).readf))

class FCOS(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "fcos"

  def apply(cpu: CPU): Unit = cpu.r(a).write(math.cos(cpu.r(b).readf))

class FTAN(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "ftan"

  def apply(cpu: CPU): Unit = cpu.r(a).write(math.tan(cpu.r(b).readf))

class FASIN(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "fasin"

  def apply(cpu: CPU): Unit = cpu.r(a).write(math.asin(cpu.r(b).readf))

class FACOS(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "facos"

  def apply(cpu: CPU): Unit = cpu.r(a).write(math.acos(cpu.r(b).readf))

class FATAN(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "fatan"

  def apply(cpu: CPU): Unit = cpu.r(a).write(math.atan(cpu.r(b).readf))

class FATAN2(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "fatan2"

  def apply(cpu: CPU): Unit = cpu.r(a).write(math.atan2(cpu.r(a).readf, cpu.r(b).readf))

class FEXP(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "fexp"

  def apply(cpu: CPU): Unit = cpu.r(a).write(math.exp(cpu.r(b).readf))

class FLOG(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "flog"

  def apply(cpu: CPU): Unit = cpu.r(a).write(math.log(cpu.r(b).readf))

// MMU instructions (RR 01 sub-format, supervisor only)

class TLBI(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "tlbi"

  def apply(cpu: CPU): Unit =
    if !cpu.test(Status.Mode) then cpu.state = State.PrivilegeViolation
    else cpu.mmu match
      case Some(m) => m.tlbInvalidate(cpu.r(a).read)
      case None    => () // no-op when MMU not present

class TLBIA(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "tlbia"

  def apply(cpu: CPU): Unit =
    if !cpu.test(Status.Mode) then cpu.state = State.PrivilegeViolation
    else cpu.mmu match
      case Some(m) => m.tlbInvalidateAll()
      case None    => () // no-op when MMU not present

class SPTBR(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "sptbr"

  def apply(cpu: CPU): Unit =
    if !cpu.test(Status.Mode) then cpu.state = State.PrivilegeViolation
    else cpu.mmu match
      case Some(m) =>
        val v = cpu.r(a).read
        val old = m.ptbr
        m.setPtbr(v)
        m.setEnabled(v != 0) // PTBR=0 disables MMU (bare mode)
        if v != old then m.tlbInvalidateAll() // only flush on actual PTBR change
      case None => () // no-op when MMU not present

class GPTBR(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "gptbr"

  def apply(cpu: CPU): Unit =
    if !cpu.test(Status.Mode) then cpu.state = State.PrivilegeViolation
    else cpu.mmu match
      case Some(m) => cpu.r(a).write(m.ptbr)
      case None    => cpu.r(a).write(0) // return 0 when MMU not present

class GFAULT(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "gfault"

  def apply(cpu: CPU): Unit =
    if !cpu.test(Status.Mode) then cpu.state = State.PrivilegeViolation
    else cpu.r(a).write(cpu.faultAddr)

class SASID(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "sasid"

  def apply(cpu: CPU): Unit =
    if !cpu.test(Status.Mode) then cpu.state = State.PrivilegeViolation
    else cpu.mmu match
      case Some(m: SimpleMMU) => m.setAsid(cpu.r(a).read.toInt)
      case _                  => () // no-op when MMU not present

class GASID(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "gasid"

  def apply(cpu: CPU): Unit =
    if !cpu.test(Status.Mode) then cpu.state = State.PrivilegeViolation
    else cpu.mmu match
      case Some(m: SimpleMMU) => cpu.r(a).write(m.asid)
      case _                  => cpu.r(a).write(0) // return 0 when MMU not present

class GFCAUSE(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "gfcause"

  def apply(cpu: CPU): Unit =
    if !cpu.test(Status.Mode) then cpu.state = State.PrivilegeViolation
    else cpu.r(a).write(cpu.faultCause.ordinal)

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

// Multi-register push/pop (R format)
// r field = upper register bound (1-6), pushes/pops r1 through rN

class PSHR(r: Int) extends Instruction:
  val mnemonic = "pshr"

  def disassemble(cpu: CPU): String = s"$mnemonic r$r"

  def apply(cpu: CPU): Unit =
    // Push r1 through rN onto stack (r1 first = deepest)
    for i <- 1 to r do
      cpu.r(7).write(cpu.r(7).read - 8)
      cpu.writeLong(cpu.r(7).read, cpu.r(i).read)

class POPR(r: Int) extends Instruction:
  val mnemonic = "popr"

  def disassemble(cpu: CPU): String = s"$mnemonic r$r"

  def apply(cpu: CPU): Unit =
    // Pop rN through r1 from stack (rN first = shallowest)
    for i <- r to 1 by -1 do
      cpu.r(i).write(cpu.readLong(cpu.r(7).read))
      cpu.r(7).write(cpu.r(7).read + 8)

// Bit manipulation (RR format)

class BTST(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "btst"

  def apply(cpu: CPU): Unit =
    val bit = cpu.r(b).read.toInt & 63
    cpu.r(a).write(if (cpu.r(a).read & (1L << bit)) != 0 then 1 else 0)

class BSET(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "bset"

  def apply(cpu: CPU): Unit =
    val bit = cpu.r(b).read.toInt & 63
    cpu.r(a).write(cpu.r(a).read | (1L << bit))

class BCLR(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "bclr"

  def apply(cpu: CPU): Unit =
    val bit = cpu.r(b).read.toInt & 63
    cpu.r(a).write(cpu.r(a).read & ~(1L << bit))

// Rotate (RR format)

class ROL(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "rol"

  def apply(cpu: CPU): Unit =
    val shift = cpu.r(b).read.toInt & 63
    val v = cpu.r(a).read
    cpu.r(a).write((v << shift) | (v >>> (64 - shift)))

class ROR(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "ror"

  def apply(cpu: CPU): Unit =
    val shift = cpu.r(b).read.toInt & 63
    val v = cpu.r(a).read
    cpu.r(a).write((v >>> shift) | (v << (64 - shift)))

// Population count (RR format)

class CNT(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "cnt"

  def apply(cpu: CPU): Unit = cpu.r(a).write(java.lang.Long.bitCount(cpu.r(b).read))

// Byte-reverse (RR format)

class REV(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "rev"

  def apply(cpu: CPU): Unit = cpu.r(a).write(java.lang.Long.reverseBytes(cpu.r(b).read))

// Sign-extend from bit width (RR format)

class SEXT(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "sext"

  def apply(cpu: CPU): Unit =
    val w = cpu.r(b).read.toInt & 63
    if w > 0 then
      val shift = 64 - w
      cpu.r(a).write((cpu.r(a).read << shift) >> shift)

// Dedicated move (RR format)

class MOV(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "mov"

  def apply(cpu: CPU): Unit = cpu.r(a).write(cpu.r(b).read)

class MIN(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "min"

  def apply(cpu: CPU): Unit =
    if cpu.r(b).read < cpu.r(a).read then cpu.r(a).write(cpu.r(b).read)

class MAX(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "max"

  def apply(cpu: CPU): Unit =
    if cpu.r(b).read > cpu.r(a).read then cpu.r(a).write(cpu.r(b).read)

class EXG(a: Int, b: Int) extends RRInstruction(a, b):
  val mnemonic = "exg"

  def apply(cpu: CPU): Unit =
    val tmp = cpu.r(a).read
    cpu.r(a).write(cpu.r(b).read)
    cpu.r(b).write(tmp)

// CLI — disable interrupts (set Ind flag)

object CLI extends SimpleInstruction:
  val mnemonic = "cli"

  def apply(cpu: CPU): Unit =
    if !cpu.test(Status.Mode) then cpu.state = State.PrivilegeViolation
    else cpu.set(Status.Ind, true)

// STI — enable interrupts (clear Ind flag)

object STI extends SimpleInstruction:
  val mnemonic = "sti"

  def apply(cpu: CPU): Unit =
    if !cpu.test(Status.Mode) then cpu.state = State.PrivilegeViolation
    else cpu.set(Status.Ind, false)

// SWSP — swap r7 and usp

object SWSP extends SimpleInstruction:
  val mnemonic = "swsp"

  def apply(cpu: CPU): Unit =
    if !cpu.test(Status.Mode) then cpu.state = State.PrivilegeViolation
    else
      val tmp = cpu.r(7).read
      cpu.r(7).write(cpu.usp)
      cpu.usp = tmp

// TSR — read cycle counter

class TSR(r: Int) extends RInstruction(r):
  val mnemonic = "tsr"

  def apply(cpu: CPU): Unit = cpu.r(r).write(cpu.cycles)

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
