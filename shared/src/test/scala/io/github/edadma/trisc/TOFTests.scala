package io.github.edadma.trisc

class TOFTests extends TestHelpers {

  "round-trips through serialize/deserialize" in {
    val tof = assemble(VECTORS + "ldi r1, 42\nhalt\n")
    val serialized = tof.serialize
    val deserialized = TOF.deserialize(serialized)
    deserialized.serialize shouldBe serialized
  }

  "loads data into memory correctly" in {
    val ram = new RAM(0, 256)
    val tof = assemble("db 0x12, 0x34\n")
    tof.load(ram)
    ram.readByteUnsigned(0) shouldBe 0x12
    ram.readByteUnsigned(1) shouldBe 0x34
  }

  "handles multiple segments" in {
    val tof = assemble(
      """segment code
        |db 0x01
        |segment data
        |db 0x02
        |""".stripMargin,
      orgs = Map("code" -> 0L, "data" -> 0x100L))
    val ram = new RAM(0, 0x200)
    tof.load(ram)
    ram.readByteUnsigned(0) shouldBe 0x01
    ram.readByteUnsigned(0x100) shouldBe 0x02
  }
}
