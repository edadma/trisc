package io.github.edadma.sfs

import Constants.*

/** Knobs that vary at format time. Geometry — total blocks — comes from
  * the device, so this struct only carries the format-time *choices*:
  * inode count, journal size, identity (UUID + volume name), and the
  * format timestamp.
  *
  * Defaults match the spec: ~1 inode per 2 MiB at the 2 TB ceiling
  * (1 Mi inodes), 128 MiB journal (32 768 blocks), empty volume name.
  *
  * `uuid` defaults to a freshly-generated v4 UUID; pass an explicit one
  * to support reproducible tests or volume cloning. `formatTime` is
  * Unix seconds — defaults to the current wall clock.
  */
final case class FormatOptions(
    totalInodes: Int = DefaultInodeCount,
    journalBlocks: Int = DefaultJournalBlocks,
    volumeName: String = "",
    uuid: IndexedSeq[Byte] = FormatOptions.randomUuid(),
    formatTime: Long = System.currentTimeMillis() / 1000L,
):
  require(totalInodes >= 3, s"totalInodes must be ≥ 3, got $totalInodes")
  require(journalBlocks >= 1, s"journalBlocks must be ≥ 1, got $journalBlocks")
  require(uuid.length == 16, s"uuid must be 16 bytes, got ${uuid.length}")
  require(
    volumeName.getBytes("UTF-8").length <= Superblock.VolumeNameMax,
    s"volumeName too long for ${Superblock.VolumeNameMax}-byte usable region",
  )

object FormatOptions:

  /** Generate a fresh v4 UUID and return its 16 raw bytes. Uses
    * `scala.util.Random` rather than `java.util.UUID.randomUUID()` so
    * Scala Native links cleanly (it omits `java.security.SecureRandom`).
    * Filesystem UUIDs are identifiers, not security tokens — non-crypto
    * randomness is fine. */
  def randomUuid(): IndexedSeq[Byte] =
    val out = new Array[Byte](16)
    scala.util.Random.nextBytes(out)
    out(6) = ((out(6) & 0x0f) | 0x40).toByte // version 4
    out(8) = ((out(8) & 0x3f) | 0x80).toByte // variant RFC 4122
    out.toIndexedSeq
