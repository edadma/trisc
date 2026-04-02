package io.github.edadma.trisc

import javax.sound.sampled.{AudioFormat, AudioSystem, SourceDataLine}

/**
 * JVM audio output for the SoundChip. Runs a background thread that polls
 * the SoundChip's active channels, generates sine wave samples, applies
 * ADSR envelopes, mixes, and writes to the system audio output.
 *
 * @param chip       The SoundChip to read channel state from
 * @param sampleRate Audio sample rate in Hz (default 44100)
 * @param bufferSize Samples per buffer fill (default 512)
 */
class JvmSoundOutput(chip: SoundChip, sampleRate: Int = 44100, bufferSize: Int = 512):

  private val format = new AudioFormat(sampleRate.toFloat, 16, 1, true, false)
  private var line: SourceDataLine = null
  private var thread: Thread = null
  @volatile private var running = false

  // Per-channel envelope state
  private val phases = new Array[Double](chip.numChannels)
  private val envLevel = new Array[Double](chip.numChannels)
  private val envStage = new Array[Int](chip.numChannels) // 0=idle, 1=attack, 2=decay, 3=sustain, 4=release
  private val wasTrig = new Array[Boolean](chip.numChannels)

  private val STAGE_IDLE = 0
  private val STAGE_ATTACK = 1
  private val STAGE_DECAY = 2
  private val STAGE_SUSTAIN = 3
  private val STAGE_RELEASE = 4

  def start(): Unit =
    if running then return
    line = AudioSystem.getSourceDataLine(format)
    line.open(format, bufferSize * 4) // extra buffer for smoothness
    line.start()
    running = true
    thread = new Thread(() => audioLoop(), "SoundChip-Audio")
    thread.setDaemon(true)
    thread.start()

  def stop(): Unit =
    running = false
    if thread != null then
      thread.join(1000)
      thread = null
    if line != null then
      line.stop()
      line.close()
      line = null

  private def audioLoop(): Unit =
    val buf = new Array[Byte](bufferSize * 2) // 16-bit = 2 bytes per sample
    val samplesPerMs = sampleRate / 1000.0

    while running do
      // Generate samples
      var i = 0
      while i < bufferSize do
        var mix = 0.0

        var ch = 0
        while ch < chip.numChannels do
          val state = chip.getChannel(ch)
          val trig = state.triggered
          val prev = wasTrig(ch)

          // Detect trigger/release transitions
          if trig && !prev then
            // Note on
            envStage(ch) = STAGE_ATTACK
            envLevel(ch) = 0.0
            phases(ch) = 0.0
          else if !trig && prev then
            // Note off — begin release
            if envStage(ch) != STAGE_IDLE then
              envStage(ch) = STAGE_RELEASE

          wasTrig(ch) = trig

          if envStage(ch) != STAGE_IDLE && state.freq > 0 && state.amp > 0 then
            // Advance envelope
            val peakAmp = state.amp / 255.0
            val susLevel = state.sustain / 255.0 * peakAmp

            envStage(ch) match
              case STAGE_ATTACK =>
                val rate = if state.attack > 0 then 1.0 / (state.attack * samplesPerMs) else 1.0
                envLevel(ch) += rate * peakAmp
                if envLevel(ch) >= peakAmp then
                  envLevel(ch) = peakAmp
                  envStage(ch) = STAGE_DECAY
              case STAGE_DECAY =>
                val rate = if state.decay > 0 then 1.0 / (state.decay * samplesPerMs) else 1.0
                val target = if state.sustain > 0 then susLevel else 0.0
                envLevel(ch) -= rate * (peakAmp - target)
                if envLevel(ch) <= target then
                  envLevel(ch) = target
                  if state.sustain > 0 then envStage(ch) = STAGE_SUSTAIN
                  else envStage(ch) = STAGE_IDLE // one-shot done
              case STAGE_SUSTAIN =>
                envLevel(ch) = susLevel
              case STAGE_RELEASE =>
                val rate = if state.release > 0 then 1.0 / (state.release * samplesPerMs) else 1.0
                envLevel(ch) -= rate * envLevel(ch).max(0.001)
                if envLevel(ch) <= 0.001 then
                  envLevel(ch) = 0.0
                  envStage(ch) = STAGE_IDLE
              case _ =>

            // Generate sine sample
            val sample = Math.sin(phases(ch) * 2.0 * Math.PI) * envLevel(ch)
            mix += sample

            // Advance phase
            phases(ch) += state.freq.toDouble / sampleRate
            if phases(ch) >= 1.0 then phases(ch) -= 1.0

          ch += 1

        // Apply master volume and clamp
        val masterScale = chip.masterVolume / 255.0
        mix *= masterScale

        // Soft clamp to avoid harsh clipping
        val clamped = if mix > 1.0 then 1.0
          else if mix < -1.0 then -1.0
          else mix

        // Convert to 16-bit signed little-endian
        val sample16 = (clamped * 32767).toInt
        buf(i * 2) = (sample16 & 0xFF).toByte
        buf(i * 2 + 1) = ((sample16 >> 8) & 0xFF).toByte

        i += 1

      // Write to audio output (blocks if buffer is full — natural rate limiting)
      if line != null && running then
        line.write(buf, 0, buf.length)
