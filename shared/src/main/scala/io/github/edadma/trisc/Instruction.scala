package io.github.edadma.trisc

abstract class Instruction extends (CPU => Unit):
  def disassemble(cpu: CPU): String = null

object IllegalInstruction extends Instruction:
  def apply(cpu: CPU): Unit = sys.error("illegal instruction")

class LDI(r: Int, imm: Int) extends Instruction:
  def apply(cpu: CPU): Unit = cpu.r(r) write imm

object BRK extends Instruction:
  def apply(cpu: CPU): Unit = cpu.running = false
