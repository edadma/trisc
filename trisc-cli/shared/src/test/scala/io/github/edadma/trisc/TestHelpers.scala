package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

trait TestHelpers extends AnyFreeSpec with Matchers {

  // Vector table: slot 0 = initial SSP, slot 1 = initial PC, slots 2-19 = exception/trap handlers
  // 20 slots × 8 bytes = 160 bytes, code starts at address 160
  val VECTORS =
    """
      |dd 0xFF0
      |dd 160
      |resb 144
      |""".stripMargin

  def mkCPU(program: String, memSize: Int = 0x1000, addresses: Int = 4): (CPU, StringBuilder) =
    val output = new StringBuilder
    val stdout = new Device with WriteOnlyAddressable {
      val name = "stdout"
      val base: Long = memSize - 8
      val size: Long = 1
      def writeByte(addr: Long, data: Long): Unit = output += data.toChar
      override def loadByte(addr: Long, data: Long): Unit = ()
    }
    val mem = new Memory("Memory", new RAM(0, memSize - 8), stdout)
    val tof = assemble(program, addresses = addresses)
    tof.load(mem)
    val cpu = new CPU(mem) { limit = 10000 }
    cpu.reset()
    (cpu, output)

  def runCPU(program: String, memSize: Int = 0x1000, addresses: Int = 4): CPU =
    val (cpu, _) = mkCPU(program, memSize, addresses)
    cpu.run()
    cpu

  def runCPU(program: String, orgs: Map[String, Long]): CPU =
    val mem = new Memory("Memory", new RAM(0, 0x1000))
    val tof = assemble(program, orgs = orgs)
    tof.load(mem)
    val cpu = new CPU(mem) { limit = 10000 }
    cpu.reset()
    cpu.run()
    cpu

  def runProgram(program: String, memSize: Int = 0x1000, addresses: Int = 4): String =
    val (cpu, output) = mkCPU(program, memSize, addresses)
    cpu.run()
    output.toString
}
