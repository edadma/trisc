package io.github.edadma.sfs

/** IEEE 802.3 CRC-32 (reflected polynomial 0xEDB88320).
  *
  * SFS protects every critical on-disk structure with a CRC field over a
  * specified byte range — this is the function that produces those CRC
  * values. The polynomial choice matches ext4 metadata checksums, so the
  * bit-for-bit results are interoperable with off-the-shelf utilities.
  *
  * Implementation is the standard byte-at-a-time table lookup. A single
  * 256-entry table is built lazily on first use and shared thereafter.
  */
object Crc32:

  /** Reversed (reflected) IEEE polynomial. */
  private final val Polynomial: Int = 0xedb88320

  /** Standard CRC-32 init / xorOut value. */
  private final val Init: Int = 0xffffffff

  private val table: Array[Int] =
    val t = new Array[Int](256)
    var n = 0
    while n < 256 do
      var c = n
      var k = 0
      while k < 8 do
        c = if (c & 1) != 0 then Polynomial ^ (c >>> 1) else c >>> 1
        k += 1
      t(n) = c
      n += 1
    t

  /** CRC-32 over the slice `buf[off, off + len)`. */
  def compute(buf: Array[Byte], off: Int, len: Int): Int =
    var c = Init
    var i = off
    val end = off + len
    while i < end do
      c = table((c ^ buf(i)) & 0xff) ^ (c >>> 8)
      i += 1
    c ^ Init

  /** CRC-32 over an entire array. */
  def compute(buf: Array[Byte]): Int =
    compute(buf, 0, buf.length)

  /** Continue a CRC computation across multiple slices. Pass the result of
    * a previous `update` (or [[Init]] for the first call) as `seed`; pass
    * the final return value through [[finish]] to xor with the standard
    * `0xffffffff` finalization constant.
    *
    * Used by the journal commit-block CRC, which spans the descriptor +
    * the metadata blocks + the commit block payload. */
  def update(seed: Int, buf: Array[Byte], off: Int, len: Int): Int =
    var c = seed
    var i = off
    val end = off + len
    while i < end do
      c = table((c ^ buf(i)) & 0xff) ^ (c >>> 8)
      i += 1
    c

  /** Initial CRC seed for incremental [[update]] usage. */
  def start: Int = Init

  /** Finalize an incremental CRC: xor with the IEEE finalization constant. */
  def finish(seed: Int): Int = seed ^ Init
