package io.github.edadma.sfs

import Constants.*

/** Region layout of an SFS volume — the static map of where each metadata
  * region lives on disk, computed once from the device size and the
  * format-time inode/journal sizing options.
  *
  * Disk layout:
  * {{{
  *   block 0                    : superblock (primary)
  *   block 1                    : superblock (backup)
  *   blockBitmapStart..          : block bitmap        (`blockBitmapLen` blocks)
  *   inodeBitmapStart..          : inode bitmap        (`inodeBitmapLen` blocks)
  *   inodeTableStart..           : inode table         (`inodeTableLen` blocks)
  *   journalStart..              : journal region      (`journalLen` blocks)
  *   dataStart..(totalBlocks-1)  : data blocks
  * }}}
  *
  * The superblock has matching `*_start` / `*_len` fields, so the layout
  * can be reconstructed from a mounted volume by reading the SB.
  */
final case class Layout(
    totalBlocks: Int,
    totalInodes: Int,
    blockBitmapStart: Int,
    blockBitmapLen: Int,
    inodeBitmapStart: Int,
    inodeBitmapLen: Int,
    inodeTableStart: Int,
    inodeTableLen: Int,
    journalStart: Int,
    journalLen: Int,
    dataStart: Int,
):
  require(totalBlocks > 0, s"totalBlocks must be > 0, got $totalBlocks")
  require(
    totalInodes >= 3,
    s"totalInodes must be ≥ 3 (null + bad-blocks + root), got $totalInodes",
  )
  require(
    dataStart < totalBlocks,
    s"data region empty: dataStart=$dataStart >= totalBlocks=$totalBlocks",
  )

  /** Disk block + byte offset where inode `n` lives. */
  def inodeLocation(n: Int): (Long, Int) =
    require(n >= 0 && n < totalInodes, s"inode $n outside [0, $totalInodes)")
    val perBlock = BlockSize / InodeSize // 16
    val blk = inodeTableStart + (n / perBlock)
    val off = (n % perBlock) * InodeSize
    (blk.toLong, off)

  /** Number of blocks reserved for metadata (everything before `dataStart`). */
  def metadataBlocks: Int = dataStart

  /** Number of blocks usable for file/directory data. */
  def dataBlocks: Int = totalBlocks - dataStart

object Layout:

  /** Round-up integer division: `ceil(a / b)` for non-negative `a` and `b > 0`. */
  private def ceilDiv(a: Int, b: Int): Int =
    require(a >= 0 && b > 0)
    (a + b - 1) / b

  /** Inodes packed per inode-table block. */
  val InodesPerBlock: Int = BlockSize / InodeSize // 16

  /** Reconstruct the layout from a parsed [[Superblock]]. */
  def fromSuperblock(sb: Superblock): Layout =
    Layout(
      totalBlocks = sb.totalBlocks,
      totalInodes = sb.totalInodes,
      blockBitmapStart = sb.blockBitmapStart,
      blockBitmapLen = sb.blockBitmapLen,
      inodeBitmapStart = sb.inodeBitmapStart,
      inodeBitmapLen = sb.inodeBitmapLen,
      inodeTableStart = sb.inodeTableStart,
      inodeTableLen = sb.inodeTableLen,
      journalStart = sb.journalStart,
      journalLen = sb.journalLen,
      dataStart = sb.dataStart,
    )

  /** Compute the layout for a volume with the given geometry. */
  def compute(totalBlocks: Int, totalInodes: Int, journalBlocks: Int): Layout =
    require(totalBlocks > 0, s"totalBlocks must be > 0, got $totalBlocks")
    require(totalInodes >= 3, s"totalInodes must be ≥ 3, got $totalInodes")
    require(journalBlocks >= 1, s"journalBlocks must be ≥ 1, got $journalBlocks")

    val blockBitmapLen = ceilDiv(totalBlocks, 8 * BlockSize)
    val inodeBitmapLen = ceilDiv(totalInodes, 8 * BlockSize)
    val inodeTableLen = ceilDiv(totalInodes, InodesPerBlock)

    val blockBitmapStart = 2
    val inodeBitmapStart = blockBitmapStart + blockBitmapLen
    val inodeTableStart = inodeBitmapStart + inodeBitmapLen
    val journalStart = inodeTableStart + inodeTableLen
    val dataStart = journalStart + journalBlocks

    Layout(
      totalBlocks = totalBlocks,
      totalInodes = totalInodes,
      blockBitmapStart = blockBitmapStart,
      blockBitmapLen = blockBitmapLen,
      inodeBitmapStart = inodeBitmapStart,
      inodeBitmapLen = inodeBitmapLen,
      inodeTableStart = inodeTableStart,
      inodeTableLen = inodeTableLen,
      journalStart = journalStart,
      journalLen = journalBlocks,
      dataStart = dataStart,
    )
