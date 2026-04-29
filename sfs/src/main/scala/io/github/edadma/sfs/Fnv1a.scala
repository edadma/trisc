package io.github.edadma.sfs

import java.nio.charset.StandardCharsets.UTF_8

/** FNV-1a 32-bit hash, the only hash algorithm used by SFS HTree directories
  * (`hash_version = 0` in the directory root block).
  *
  * Reference: http://www.isthe.com/chongo/tech/comp/fnv/
  *
  * The algorithm:
  * {{{
  *   h = OFFSET
  *   for each byte b in input:
  *     h ^= b
  *     h *= PRIME   (32-bit truncating)
  * }}}
  *
  * `*` here is unsigned 32-bit multiplication; in Scala/JVM this is just
  * regular `Int` multiplication — the low 32 bits are what FNV specifies.
  *
  * Used for both leaf-block keying and index-entry sort keys. Hashing
  * happens on the UTF-8 byte representation of names.
  */
object Fnv1a:

  /** FNV-1a 32-bit offset basis (2166136261). */
  val Offset: Int = 0x811c9dc5

  /** FNV-1a 32-bit prime (16777619). */
  val Prime: Int = 0x01000193

  /** Hash a slice of a byte array. */
  def hash(buf: Array[Byte], off: Int, len: Int): Int =
    var h = Offset
    var i = off
    val end = off + len
    while i < end do
      h ^= (buf(i) & 0xff)
      h *= Prime
      i += 1
    h

  /** Hash an entire byte array. */
  def hash(buf: Array[Byte]): Int = hash(buf, 0, buf.length)

  /** Hash a string by its UTF-8 byte representation — the form SFS uses
    * for directory entry names. */
  def hash(s: String): Int = hash(s.getBytes(UTF_8))
