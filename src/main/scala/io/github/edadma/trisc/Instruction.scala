package io.github.edadma.trisc

abstract class Instruction extends (CPU => Unit):
  def disassemble(cpu: CPU): String = null

object IllegalInstruction extends Instruction:
  def apply(cpu: CPU): Unit = sys.error("illegal instruction")

class LDI(r: Int, imm: Int) extends Instruction:
  def apply(cpu: CPU): Unit = cpu.r(r) write imm

class SLI(r: Int, imm: Int) extends Instruction:
  def apply(cpu: CPU): Unit = cpu.r(r).write((cpu.r(r).read << 8) | imm)

class JALR(a: Int, b: Int) extends Instruction:
  def apply(cpu: CPU): Unit =
    cpu.r(a) write cpu.pc
    cpu.pc = cpu.r(b).read

class TRAP(imm: Int) extends Instruction:
  def apply(cpu: CPU): Unit = cpu.state = State.fromOrdinal(State.Trap0.ordinal + imm)

object HALT extends Instruction:
  def apply(cpu: CPU): Unit = cpu.state = State.Halt

object RTE extends Instruction:
  def apply(cpu: CPU): Unit =
    for i <- 1 to 7 do cpu.r(i) write cpu.sr(i)
    cpu.pc = cpu.spc
    cpu.psr = cpu.spsr

class SPSR(r: Int) extends Instruction:
  def apply(cpu: CPU): Unit = cpu.psr = cpu.r(r).read.toInt

class GPSR(r: Int) extends Instruction:
  def apply(cpu: CPU): Unit = cpu.r(r) write cpu.psr & 0xffffffff

class ADDI(a: Int, b: Int, imm: Int) extends Instruction:
  def apply(cpu: CPU): Unit = cpu.r(a).write(cpu.r(b).read + imm)

class BLS(a: Int, b: Int, imm: Int) extends Instruction:
  def apply(cpu: CPU): Unit = if cpu.r(a).read < cpu.r(b).read then cpu.pc += imm * 2

class BEQ(a: Int, b: Int, imm: Int) extends Instruction:
  def apply(cpu: CPU): Unit = if cpu.r(a).read == cpu.r(b).read then cpu.pc += imm * 2

class STI(r: Int, imm: Int) extends Instruction:
  def apply(cpu: CPU): Unit = cpu.writeByte(cpu.r(r).read, imm)

class LDB(d: Int, a: Int, b: Int) extends Instruction:
  def apply(cpu: CPU): Unit = cpu.r(d) write cpu.readByte(cpu.r(a).read + cpu.r(b).read)

class LD(a: Int, b: Int, imm: Int) extends Instruction:
  def apply(cpu: CPU): Unit = cpu.r(a) write cpu.readInt(cpu.r(b).read + imm * 2)

class ST(a: Int, b: Int, imm: Int) extends Instruction:
  def apply(cpu: CPU): Unit = cpu.writeInt(cpu.r(b).read + imm * 2, cpu.r(a).read)

class STB(d: Int, a: Int, b: Int) extends Instruction:
  def apply(cpu: CPU): Unit = cpu.writeByte(cpu.r(d).read + cpu.r(a).read, cpu.r(b).read)

class LDS(d: Int, a: Int, b: Int) extends Instruction:
  def apply(cpu: CPU): Unit = cpu.r(d) write cpu.readShort(cpu.r(a).read + cpu.r(b).read)

class STS(d: Int, a: Int, b: Int) extends Instruction:
  def apply(cpu: CPU): Unit = cpu.writeShort(cpu.r(d).read + cpu.r(a).read, cpu.r(b).read)

class LDW(d: Int, a: Int, b: Int) extends Instruction:
  def apply(cpu: CPU): Unit = cpu.r(d) write cpu.readInt(cpu.r(a).read + cpu.r(b).read)

class STW(d: Int, a: Int, b: Int) extends Instruction:
  def apply(cpu: CPU): Unit = cpu.writeInt(cpu.r(d).read + cpu.r(a).read, cpu.r(b).read)

class LDD(d: Int, a: Int, b: Int) extends Instruction:
  def apply(cpu: CPU): Unit = cpu.r(d) write cpu.readLong(cpu.r(a).read + cpu.r(b).read)

class STD(d: Int, a: Int, b: Int) extends Instruction:
  def apply(cpu: CPU): Unit = cpu.writeLong(cpu.r(d).read + cpu.r(a).read, cpu.r(b).read)
