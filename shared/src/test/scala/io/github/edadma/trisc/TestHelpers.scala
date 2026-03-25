package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

trait TestHelpers extends AnyFreeSpec with Matchers {

  val VECTORS =
    """
      |dw 8
      |dw 0
      |dw 0
      |dw 0
      |""".stripMargin

  def mkCPU(program: String, memSize: Int = 0x1000, addresses: Int = 2): (CPU, StringBuilder) =
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
    val cpu = new CPU(mem, Nil) { limit = 10000 }
    cpu.reset()
    (cpu, output)

  def runCPU(program: String, memSize: Int = 0x1000, addresses: Int = 2): CPU =
    val (cpu, _) = mkCPU(program, memSize, addresses)
    cpu.run()
    cpu

  def runCPU(program: String, orgs: Map[String, Long]): CPU =
    val mem = new Memory("Memory", new RAM(0, 0x1000))
    val tof = assemble(program, orgs = orgs)
    tof.load(mem)
    val cpu = new CPU(mem, Nil) { limit = 10000 }
    cpu.reset()
    cpu.run()
    cpu

  def runProgram(program: String, memSize: Int = 0x1000, addresses: Int = 2): String =
    val (cpu, output) = mkCPU(program, memSize, addresses)
    cpu.run()
    output.toString
}
