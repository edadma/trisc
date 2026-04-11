package io.github.edadma.trisc

/** Interactive sound demo for the TRISC SoundChip + JvmSoundOutput.
  * Run: sbt "triscCliJVM/runMain io.github.edadma.trisc.SoundDemoMain"
  * Optional arg: beep | chord | piano | all (default: all)
  */
object SoundDemoMain:
  private def playBeep(): Unit =
    println("Playing 440Hz beep (200ms)...")
    val chip = new SoundChip(0x100)
    val output = new JvmSoundOutput(chip)
    output.start()
    chip.writeByte(0x100, 0x01)
    chip.writeByte(0x101, 0xB8) // 440Hz
    chip.writeByte(0x102, 255)  // amp
    chip.writeByte(0x103, 5)    // attack 5ms
    chip.writeByte(0x104, 200)  // decay 200ms
    chip.writeByte(0x105, 0)    // one-shot
    chip.writeByte(0x106, 0)
    chip.writeByte(0x100 + 2049, 1) // trigger
    Thread.sleep(300)
    output.stop()

  private def playChord(): Unit =
    println("Playing C major chord (C4+E4+G4, 250ms)...")
    val chip = new SoundChip(0x100)
    val output = new JvmSoundOutput(chip)
    output.start()
    // C4 (262Hz)
    chip.writeByte(0x100, 0x01); chip.writeByte(0x101, 0x06)
    chip.writeByte(0x102, 200); chip.writeByte(0x103, 5)
    chip.writeByte(0x104, 250); chip.writeByte(0x105, 0); chip.writeByte(0x106, 0)
    // E4 (330Hz)
    chip.writeByte(0x108, 0x01); chip.writeByte(0x109, 0x4A)
    chip.writeByte(0x10A, 200); chip.writeByte(0x10B, 5)
    chip.writeByte(0x10C, 250); chip.writeByte(0x10D, 0); chip.writeByte(0x10E, 0)
    // G4 (392Hz)
    chip.writeByte(0x110, 0x01); chip.writeByte(0x111, 0x88)
    chip.writeByte(0x112, 200); chip.writeByte(0x113, 5)
    chip.writeByte(0x114, 250); chip.writeByte(0x115, 0); chip.writeByte(0x116, 0)
    chip.writeByte(0x100 + 2049, 1) // trigger
    Thread.sleep(350)
    output.stop()

  private def playPiano(): Unit =
    println("Playing piano timbre A4 (6 harmonics, 255ms)...")
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
    chip.writeByte(0x100 + 2049, 1) // trigger
    Thread.sleep(400)
    output.stop()

  def main(args: Array[String]): Unit =
    val mode = args.headOption.getOrElse("all")
    if mode == "beep" || mode == "all" then playBeep()
    if mode == "chord" || mode == "all" then
      if mode == "all" then Thread.sleep(200)
      playChord()
    if mode == "piano" || mode == "all" then
      if mode == "all" then Thread.sleep(200)
      playPiano()
    println("Done.")
