package io.github.edadma.trisc

class SoundChipTests extends TestHelpers {

  def mkChip(): (SoundChip, Int) =
    var updates = 0
    val chip = new SoundChip(0x100, () => updates += 1)
    (chip, updates)

  /** Set frequency for channel ch */
  def setFreq(chip: SoundChip, ch: Int, hz: Int): Unit =
    val off = 0x100 + ch * 8
    chip.writeByte(off, (hz >> 8) & 0xFF)
    chip.writeByte(off + 1, hz & 0xFF)

  /** Set full channel: freq, amp, ADSR */
  def setChannel(chip: SoundChip, ch: Int, hz: Int, amp: Int, atk: Int = 10, dec: Int = 50, sus: Int = 128, rel: Int = 100): Unit =
    val off = 0x100 + ch * 8
    chip.writeByte(off, (hz >> 8) & 0xFF)
    chip.writeByte(off + 1, hz & 0xFF)
    chip.writeByte(off + 2, amp)
    chip.writeByte(off + 3, atk)
    chip.writeByte(off + 4, dec)
    chip.writeByte(off + 5, sus)
    chip.writeByte(off + 6, rel)

  // ===== Basic properties =====

  "SoundChip has correct size" in {
    val (chip, _) = mkChip()
    chip.size shouldBe 2114
  }

  "SoundChip has 256 channels" in {
    val (chip, _) = mkChip()
    chip.numChannels shouldBe 256
  }

  "master volume defaults to 255" in {
    val (chip, _) = mkChip()
    chip.masterVolume shouldBe 255
  }

  // ===== Channel register readback =====

  "frequency register readback" in {
    val (chip, _) = mkChip()
    setFreq(chip, 0, 440)
    chip.readByte(0x100) shouldBe 0x01 // 440 >> 8
    chip.readByte(0x101) shouldBe 0xB8 // 440 & 0xFF
  }

  "amplitude register readback" in {
    val (chip, _) = mkChip()
    chip.writeByte(0x102, 200)
    chip.readByte(0x102) shouldBe 200
  }

  "ADSR registers readback" in {
    val (chip, _) = mkChip()
    chip.writeByte(0x103, 10)  // attack
    chip.writeByte(0x104, 50)  // decay
    chip.writeByte(0x105, 128) // sustain
    chip.writeByte(0x106, 100) // release
    chip.readByte(0x103) shouldBe 10
    chip.readByte(0x104) shouldBe 50
    chip.readByte(0x105) shouldBe 128
    chip.readByte(0x106) shouldBe 100
  }

  "channel 255 is accessible" in {
    val (chip, _) = mkChip()
    setChannel(chip, 255, 1000, 200)
    val ch = chip.getChannel(255)
    ch.freq shouldBe 1000
    ch.amp shouldBe 200
  }

  // ===== Master volume =====

  "master volume readback" in {
    val (chip, _) = mkChip()
    chip.writeByte(0x100 + 2048, 100)
    chip.masterVolume shouldBe 100
    chip.readByte(0x100 + 2048) shouldBe 100
  }

  // ===== Global trigger =====

  "global trigger starts all channels with amp > 0" in {
    val (chip, _) = mkChip()
    setChannel(chip, 0, 440, 200)
    setChannel(chip, 1, 880, 150)
    setChannel(chip, 2, 0, 0) // inactive
    chip.writeByte(0x100 + 2049, 1) // TRIGGER = 1

    chip.getChannel(0).triggered shouldBe true
    chip.getChannel(1).triggered shouldBe true
    chip.getChannel(2).triggered shouldBe false
  }

  "global release stops all channels" in {
    val (chip, _) = mkChip()
    setChannel(chip, 0, 440, 200)
    setChannel(chip, 1, 880, 150)
    chip.writeByte(0x100 + 2049, 1) // trigger all
    chip.writeByte(0x100 + 2049, 2) // release all
    chip.getChannel(0).triggered shouldBe false
    chip.getChannel(1).triggered shouldBe false
  }

  // ===== Per-channel trigger bitmask =====

  "CH_TRIG sets specific channels" in {
    val (chip, _) = mkChip()
    setChannel(chip, 0, 440, 200)
    setChannel(chip, 1, 880, 150)
    setChannel(chip, 2, 1320, 100)
    // Trigger channels 0 and 2 (bits 0 and 2 = 0x05)
    chip.writeByte(0x100 + 2050, 0x05)
    chip.getChannel(0).triggered shouldBe true
    chip.getChannel(1).triggered shouldBe false
    chip.getChannel(2).triggered shouldBe true
  }

  "CH_TRIG readback shows trigger state" in {
    val (chip, _) = mkChip()
    setChannel(chip, 0, 440, 200)
    chip.writeByte(0x100 + 2050, 0x01)
    chip.readByte(0x100 + 2050) shouldBe 0x01
  }

  "CH_REL releases specific channels" in {
    val (chip, _) = mkChip()
    setChannel(chip, 0, 440, 200)
    setChannel(chip, 1, 880, 150)
    chip.writeByte(0x100 + 2050, 0x03) // trigger 0 and 1
    chip.getChannel(0).triggered shouldBe true
    chip.getChannel(1).triggered shouldBe true
    // Release only channel 0 (bit 0 = 0x01)
    chip.writeByte(0x100 + 2082, 0x01)
    chip.getChannel(0).triggered shouldBe false
    chip.getChannel(1).triggered shouldBe true
  }

  "CH_TRIG byte 1 controls channels 8-15" in {
    val (chip, _) = mkChip()
    setChannel(chip, 8, 440, 200)
    chip.writeByte(0x100 + 2051, 0x01) // bit 0 of byte 1 = channel 8
    chip.getChannel(8).triggered shouldBe true
    chip.getChannel(0).triggered shouldBe false
  }

  // ===== getActiveChannels =====

  "getActiveChannels returns only active triggered channels" in {
    val (chip, _) = mkChip()
    setChannel(chip, 0, 440, 200)
    setChannel(chip, 1, 0, 0)   // freq 0
    setChannel(chip, 2, 880, 0) // amp 0
    setChannel(chip, 3, 660, 100)
    chip.writeByte(0x100 + 2049, 1) // trigger all

    val active = chip.getActiveChannels
    active.length shouldBe 2
    active.map(_._1) should contain(0)
    active.map(_._1) should contain(3)
  }

  // ===== One-shot vs sustained =====

  "channel with sustain=0 is one-shot" in {
    val (chip, _) = mkChip()
    setChannel(chip, 0, 440, 200, sus = 0)
    chip.getChannel(0).isOneShot shouldBe true
  }

  "channel with sustain>0 is sustained" in {
    val (chip, _) = mkChip()
    setChannel(chip, 0, 440, 200, sus = 128)
    chip.getChannel(0).isOneShot shouldBe false
  }

  // ===== Harmonic series (timbre building) =====

  "set up harmonic series for piano-like timbre" in {
    val (chip, _) = mkChip()
    val fundamental = 440
    // Harmonics with decreasing amplitude
    setChannel(chip, 0, fundamental, 255, atk = 5, dec = 200, sus = 0, rel = 0)
    setChannel(chip, 1, fundamental * 2, 180, atk = 5, dec = 150, sus = 0, rel = 0)
    setChannel(chip, 2, fundamental * 3, 120, atk = 5, dec = 100, sus = 0, rel = 0)
    setChannel(chip, 3, fundamental * 4, 80, atk = 5, dec = 80, sus = 0, rel = 0)
    setChannel(chip, 4, fundamental * 5, 50, atk = 5, dec = 60, sus = 0, rel = 0)
    setChannel(chip, 5, fundamental * 6, 30, atk = 5, dec = 40, sus = 0, rel = 0)

    chip.writeByte(0x100 + 2049, 1) // trigger all

    val active = chip.getActiveChannels
    active.length shouldBe 6
    active.head._2.freq shouldBe 440
    active.last._2.freq shouldBe 2640
    active.foreach(_._2.isOneShot shouldBe true) // piano = one-shot
  }

  // ===== onUpdate callback =====

  "onUpdate fires on register writes" in {
    var updates = 0
    val chip = new SoundChip(0x100, () => updates += 1)
    chip.writeByte(0x102, 200)  // amp
    chip.writeByte(0x100 + 2049, 1) // trigger
    updates should be >= 2
  }
}
