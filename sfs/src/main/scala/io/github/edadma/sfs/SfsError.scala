package io.github.edadma.sfs

/** Raised when an on-disk structure fails an integrity check (bad magic, CRC
  * mismatch, owning-inode mismatch, …). The filesystem refuses to use a
  * structure that fails to authenticate, since the spec's correctness story
  * depends on these checksums.
  */
final class SfsCorruptError(message: String) extends RuntimeException(message)
