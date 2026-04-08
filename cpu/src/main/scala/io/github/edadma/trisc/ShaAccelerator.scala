package io.github.edadma.trisc

/** ESP32-style SHA-256 hardware accelerator.
  *
  * Register map (big-endian 32-bit words):
  *   0x00–0x3C  SHA_TEXT[0..15]  R/W  512-bit message block / hash state
  *   0x40       SHA_START        W    Hash first block (resets to IV)
  *   0x44       SHA_CONTINUE     W    Hash next block (uses current state)
  *   0x48       SHA_LOAD         W    Load state from TEXT[0..7] into engine
  *   0x4C       SHA_BUSY         R    Always 0 (computation is synchronous)
  */
class ShaAccelerator(val base: Long) extends Device:
  val name = "SHA256"
  val size = 0x50L

  // 16 x 32-bit text registers (message block in, hash state out)
  private val text = new Array[Int](16)

  // Internal hash state (8 x 32-bit words)
  private val state = new Array[Int](8)

  // SHA-256 initial hash values
  private val IV = Array(
    0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a,
    0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19,
  )

  // SHA-256 round constants
  private val K = Array(
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
    0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
    0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
    0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
    0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
  )

  private def rotr(x: Int, n: Int): Int = (x >>> n) | (x << (32 - n))

  /** Run one SHA-256 compression: 64 rounds on text[0..15], mixing into state[0..7]. */
  private def compress(): Unit =

    // Message schedule
    val w = new Array[Int](64)
    System.arraycopy(text, 0, w, 0, 16)
    for i <- 16 until 64 do
      val s0 = rotr(w(i - 15), 7) ^ rotr(w(i - 15), 18) ^ (w(i - 15) >>> 3)
      val s1 = rotr(w(i - 2), 17) ^ rotr(w(i - 2), 19) ^ (w(i - 2) >>> 10)
      w(i) = w(i - 16) + s0 + w(i - 7) + s1

    // Working variables
    var a = state(0); var b = state(1); var c = state(2); var d = state(3)
    var e = state(4); var f = state(5); var g = state(6); var h = state(7)

    // 64 rounds
    for i <- 0 until 64 do
      val S1  = rotr(e, 6) ^ rotr(e, 11) ^ rotr(e, 25)
      val ch  = (e & f) ^ (~e & g)
      val t1  = h + S1 + ch + K(i) + w(i)
      val S0  = rotr(a, 2) ^ rotr(a, 13) ^ rotr(a, 22)
      val maj = (a & b) ^ (a & c) ^ (b & c)
      val t2  = S0 + maj
      h = g; g = f; f = e; e = d + t1; d = c; c = b; b = a; a = t1 + t2

    // Add back to state
    state(0) += a; state(1) += b; state(2) += c; state(3) += d
    state(4) += e; state(5) += f; state(6) += g; state(7) += h

    // Write state back to text[0..7] so software can read it
    System.arraycopy(state, 0, text, 0, 8)

  // Byte-level access to 32-bit big-endian registers
  def readByte(addr: Long): Int =
    val off = (addr - base).toInt
    if off >= 0x4c && off < 0x50 then
      0 // SHA_BUSY — always idle
    else if off >= 0 && off < 0x40 then
      val word = off / 4
      val byteIdx = off % 4
      (text(word) >>> (24 - byteIdx * 8)) & 0xff
    else 0

  def writeByte(addr: Long, data: Long): Unit =
    val off = (addr - base).toInt
    if off >= 0 && off < 0x40 then
      // Write to SHA_TEXT registers
      val word = off / 4
      val byteIdx = off % 4
      val shift = 24 - byteIdx * 8
      val mask = ~(0xff << shift)
      text(word) = (text(word) & mask) | ((data.toInt & 0xff) << shift)
    else if off >= 0x40 && off < 0x44 then
      // SHA_START — last byte triggers
      if off == 0x43 then
        System.arraycopy(IV, 0, state, 0, 8)
        compress()
    else if off >= 0x44 && off < 0x48 then
      // SHA_CONTINUE — last byte triggers
      if off == 0x47 then compress()
    else if off >= 0x48 && off < 0x4c then
      // SHA_LOAD — last byte triggers: copy text[0..7] into state
      if off == 0x4b then System.arraycopy(text, 0, state, 0, 8)
