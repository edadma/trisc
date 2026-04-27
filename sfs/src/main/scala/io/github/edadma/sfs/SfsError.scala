package io.github.edadma.sfs

/** Raised when an on-disk structure fails an integrity check (bad magic, CRC
  * mismatch, owning-inode mismatch, …). The filesystem refuses to use a
  * structure that fails to authenticate, since the spec's correctness story
  * depends on these checksums.
  */
final class SfsCorruptError(message: String) extends RuntimeException(message)

/** Raised by directory operations when the named entry already exists. */
final class SfsExistsError(message: String) extends RuntimeException(message)

/** Raised by directory operations when the named entry does not exist. */
final class SfsNotFoundError(message: String) extends RuntimeException(message)

/** Raised by file operations that refuse to operate on a directory
  * (e.g. `unlink` on a directory, `link` of a directory). */
final class SfsIsDirectoryError(message: String) extends RuntimeException(message)

/** Raised when an allocator can't satisfy a request (out of free
  * inodes or free data blocks). */
final class SfsNoSpaceError(message: String) extends RuntimeException(message)
