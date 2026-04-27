package io.github.edadma.sfs

object Constants:

  // ---- Geometry ---------------------------------------------------------

  /** Filesystem block size in bytes. Fixed at 4 KiB. */
  val BlockSize: Int = 4096

  /** Maximum addressable blocks: 512 M blocks → 2 TB volume. */
  val TotalBlocks: Long = 1L << 29

  /** Default inode count chosen at format time (1 Mi). */
  val DefaultInodeCount: Int = 1 << 20

  /** Inode record size on disk. */
  val InodeSize: Int = 256

  /** Extent record size on disk. */
  val ExtentSize: Int = 8

  /** Inline extents stored directly in the inode. */
  val InlineExtents: Int = 16

  /** Extents per single-indirect block (BlockSize / ExtentSize). */
  val ExtentsPerIndirect: Int = BlockSize / ExtentSize // 512

  /** Block addresses per double/triple-indirect block (BlockSize / 4). */
  val PtrsPerIndirect2: Int = BlockSize / 4 // 1024

  /** Maximum filename length, in bytes. */
  val NameMax: Int = 255

  /** Maximum path length, in bytes. */
  val PathMax: Int = 4096

  /** Inline symlink threshold (127 bytes + NUL fits in the 128-byte inode union). */
  val InlineSymlinkMax: Int = 127

  /** Default journal size in blocks (128 MiB). */
  val DefaultJournalBlocks: Int = 32768

  /** Maximum metadata blocks per transaction; caps descriptor blocks at 2. */
  val MaxBlocksPerTransaction: Int = 1024

  // ---- Reserved inode numbers ------------------------------------------

  val InoNull: Int = 0
  val InoBadBlocks: Int = 1
  val InoRoot: Int = 2

  // ---- Magic numbers ----------------------------------------------------

  /** Superblock magic: ASCII "SFS\0". */
  val MagicSuperblock: Int = 0x53465300

  /** Journal superblock magic: ASCII "SFSJ". */
  val MagicJournalSuperblock: Int = 0x5346534a

  /** Transaction descriptor magic: ASCII "SFST". */
  val MagicTxnDescriptor: Int = 0x53465354

  /** Commit block magic: ASCII "SFSC". */
  val MagicCommit: Int = 0x53465343

  /** Directory block tail magic: ASCII "SFSD". */
  val MagicDirTail: Int = 0x53465344

  // ---- Filesystem state -------------------------------------------------

  val FsClean: Int = 0
  val FsDirty: Int = 1
  val FsError: Int = 2

  // ---- Inode flag bits --------------------------------------------------

  val InodeFlagInlineSymlink: Int = 1 << 0
  val InodeFlagHasXattr: Int = 1 << 1
  val InodeFlagHasIndirect1: Int = 1 << 2
  val InodeFlagHasIndirect2: Int = 1 << 3
  val InodeFlagHasIndirect3: Int = 1 << 4

  // ---- Extent flag bits (top of 32-bit word) ---------------------------

  val ExtentFlagUninitialized: Int = 1 << 31
  val ExtentFlagSparse: Int = 1 << 30

  /** Mask isolating the 29-bit start_block from the flag bits. */
  val ExtentStartMask: Int = 0x1fffffff

  // ---- Hash algorithm ids ----------------------------------------------

  val HashFnv1a: Int = 0
