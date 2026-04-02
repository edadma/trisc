package io.github.edadma.trisc

class SoundOutputTests extends TestHelpers {

  "JvmSoundOutput starts and stops without error" ignore {
    val chip = new SoundChip(0x100)
    val output = new JvmSoundOutput(chip)
    output.start()
    Thread.sleep(50)
    output.stop()
  }

  "JvmSoundOutput plays a short beep" ignore {
    val chip = new SoundChip(0x100)
    val output = new JvmSoundOutput(chip)
    output.start()

    // Set up channel 0: 440Hz, full amplitude, instant attack, 200ms decay, one-shot
    chip.writeByte(0x100, 0x01) // freq high = 1
    chip.writeByte(0x101, 0xB8) // freq low = 0xB8 → 440
    chip.writeByte(0x102, 255)  // amp
    chip.writeByte(0x103, 5)    // attack 5ms
    chip.writeByte(0x104, 200)  // decay 200ms
    chip.writeByte(0x105, 0)    // sustain 0 = one-shot
    chip.writeByte(0x106, 0)    // release 0

    // Trigger
    chip.writeByte(0x100 + 2049, 1)

    // Let it play
    Thread.sleep(300)

    // Should have finished (one-shot, 200ms decay)
    output.stop()
  }

  "JvmSoundOutput plays a chord" ignore {
    val chip = new SoundChip(0x100)
    val output = new JvmSoundOutput(chip)
    output.start()

    // C major chord: C4(262), E4(330), G4(392)
    // Channel 0: C4
    chip.writeByte(0x100, 0x01); chip.writeByte(0x101, 0x06) // 262
    chip.writeByte(0x102, 200); chip.writeByte(0x103, 5)
    chip.writeByte(0x104, 250); chip.writeByte(0x105, 0); chip.writeByte(0x106, 0)

    // Channel 1: E4
    chip.writeByte(0x108, 0x01); chip.writeByte(0x109, 0x4A) // 330
    chip.writeByte(0x10A, 200); chip.writeByte(0x10B, 5)
    chip.writeByte(0x10C, 250); chip.writeByte(0x10D, 0); chip.writeByte(0x10E, 0)

    // Channel 2: G4
    chip.writeByte(0x110, 0x01); chip.writeByte(0x111, 0x88) // 392
    chip.writeByte(0x112, 200); chip.writeByte(0x113, 5)
    chip.writeByte(0x114, 250); chip.writeByte(0x115, 0); chip.writeByte(0x116, 0)

    // Trigger all at once
    chip.writeByte(0x100 + 2049, 1)

    Thread.sleep(350)
    output.stop()
  }

  "JvmSoundOutput plays harmonic series (piano timbre)" ignore {
    val chip = new SoundChip(0x100)
    val output = new JvmSoundOutput(chip)
    output.start()

    val fundamental = 440
    val harmonics = Seq((1, 255), (2, 180), (3, 120), (4, 80), (5, 50), (6, 30))

    for ((mult, amplitude), ch) <- harmonics.zipWithIndex do
      val freq = fundamental * mult
      val off = 0x100 + ch * 8
      chip.writeByte(off, (freq >> 8) & 0xFF)
      chip.writeByte(off + 1, freq & 0xFF)
      chip.writeByte(off + 2, amplitude)
      chip.writeByte(off + 3, 2)    // attack 2ms
      chip.writeByte(off + 4, 255)  // decay 255ms
      chip.writeByte(off + 5, 0)    // one-shot
      chip.writeByte(off + 6, 0)

    chip.writeByte(0x100 + 2049, 1) // trigger all

    Thread.sleep(400)
    output.stop()
  }
}
