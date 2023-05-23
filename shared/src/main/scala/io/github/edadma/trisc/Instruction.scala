package io.github.edadma.trisc

abstract class Instruction extends (CPU => Unit):
  def disassemble(cpu: CPU): String = null

object IllegalInstruction extends Instruction:
  def apply(cpu: CPU): Unit = sys.error("illegal instruction")

class LDI(r: Int, imm: Int) extends Instruction:
  def apply(cpu: CPU): Unit = cpu.r(r) write imm

class SLI(r: Int, imm: Int) extends Instruction:
  def apply(cpu: CPU): Unit = cpu.r(r).write((cpu.r(r).read << 8) | imm)

object BRK extends Instruction:
  def apply(cpu: CPU): Unit = cpu.running = false

class ADDI(a: Int, b: Int, imm: Int) extends Instruction:
  def apply(cpu: CPU): Unit = cpu.r(a).write(cpu.r(b).read + imm)

class BLS(a: Int, b: Int, imm: Int) extends Instruction:
  def apply(cpu: CPU): Unit = if cpu.r(a).read < cpu.r(b).read then cpu.pc += imm

class BEQ(a: Int, b: Int, imm: Int) extends Instruction:
  def apply(cpu: CPU): Unit = if cpu.r(a).read == cpu.r(b).read then cpu.pc += imm

class STI(r: Int, imm: Int) extends Instruction:
  def apply(cpu: CPU): Unit = cpu.mem.writeByte(cpu.r(r).read, imm)

class STB(d: Int, a: Int, b: Int) extends Instruction:
  def apply(cpu: CPU): Unit = cpu.mem.writeByte(cpu.r(d).read + cpu.r(a).read, cpu.r(a).read)
