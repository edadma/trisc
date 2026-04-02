package io.github.edadma.trisc

class TFSBitmapTests extends TFSTestHelpers {

  val prefill: String =
    """/dev/tty0 char 0 0
      |/etc/motd file "Hello TOS"
      |""".stripMargin

  // ===== bitmap_test =====

  "bitmap_test returns 1 for allocated inode" in {
    val (_, output) = runTFS(
      """import tfs.*
        |main() -> int
        |    tfs_init()
        |    // Inode 0 (reserved) and 1 (root) should be allocated
        |    if bitmap_test(sb_inode_bitmap, 0) == 1
        |        putchar(65)
        |    if bitmap_test(sb_inode_bitmap, 1) == 1
        |        putchar(66)
        |    0
        |""".stripMargin, prefill = prefill)
    output shouldBe "AB"
  }

  "bitmap_test returns 0 for free inode" in {
    val (_, output) = runTFS(
      """import tfs.*
        |main() -> int
        |    tfs_init()
        |    // High inode numbers should be free
        |    if bitmap_test(sb_inode_bitmap, 100) == 0
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  // ===== bitmap_set / bitmap_clear =====

  "bitmap_set then test" in {
    val (_, output) = runTFS(
      """import tfs.*
        |main() -> int
        |    tfs_init()
        |    // Verify bit 50 is free, set it, verify it's set
        |    if bitmap_test(sb_inode_bitmap, 50) == 0
        |        putchar(65)
        |    bitmap_set(sb_inode_bitmap, 50)
        |    if bitmap_test(sb_inode_bitmap, 50) == 1
        |        putchar(66)
        |    0
        |""".stripMargin, prefill = prefill)
    output shouldBe "AB"
  }

  "bitmap_clear then test" in {
    val (_, output) = runTFS(
      """import tfs.*
        |main() -> int
        |    tfs_init()
        |    bitmap_set(sb_inode_bitmap, 50)
        |    bitmap_clear(sb_inode_bitmap, 50)
        |    if bitmap_test(sb_inode_bitmap, 50) == 0
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  // ===== alloc_inode =====

  "alloc_inode returns valid inode" in {
    val (_, output) = runTFS(
      """import tfs.*
        |main() -> int
        |    tfs_init()
        |    val ino = alloc_inode()
        |    if ino >= 2
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  "alloc_inode marks bit in bitmap" in {
    val (_, output) = runTFS(
      """import tfs.*
        |main() -> int
        |    tfs_init()
        |    val ino = alloc_inode()
        |    if bitmap_test(sb_inode_bitmap, ino) == 1
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  "alloc_inode twice returns different inodes" in {
    val (_, output) = runTFS(
      """import tfs.*
        |main() -> int
        |    tfs_init()
        |    val a = alloc_inode()
        |    val b = alloc_inode()
        |    if a != b
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  // ===== alloc_block =====

  "alloc_block returns valid block" in {
    val (_, output) = runTFS(
      """import tfs.*
        |main() -> int
        |    tfs_init()
        |    val blk = alloc_block()
        |    if blk >= sb_first_data
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  // ===== free_inode / free_block =====

  "free_inode makes inode reusable" in {
    val (_, output) = runTFS(
      """import tfs.*
        |main() -> int
        |    tfs_init()
        |    val ino = alloc_inode()
        |    if bitmap_test(sb_inode_bitmap, ino) == 1
        |        putchar(65)
        |    free_inode(ino)
        |    if bitmap_test(sb_inode_bitmap, ino) == 0
        |        putchar(66)
        |    // Can re-allocate the same inode
        |    val ino2 = alloc_inode()
        |    if ino2 == ino
        |        putchar(67)
        |    0
        |""".stripMargin, prefill = prefill)
    output shouldBe "ABC"
  }

  "free_block makes block reusable" in {
    val (_, output) = runTFS(
      """import tfs.*
        |main() -> int
        |    tfs_init()
        |    val blk = alloc_block()
        |    if bitmap_test(sb_block_bitmap, blk) == 1
        |        putchar(65)
        |    free_block(blk)
        |    if bitmap_test(sb_block_bitmap, blk) == 0
        |        putchar(66)
        |    val blk2 = alloc_block()
        |    if blk2 == blk
        |        putchar(67)
        |    0
        |""".stripMargin, prefill = prefill)
    output shouldBe "ABC"
  }

  // ===== tfs_freeblocks / tfs_freeinodes =====

  "tfs_freeblocks positive after init" in {
    val (_, output) = runTFS(
      """import tfs.*
        |main() -> int
        |    tfs_init()
        |    if tfs_freeblocks() > 0
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }

  "tfs_freeinodes positive after init" in {
    val (_, output) = runTFS(
      """import tfs.*
        |main() -> int
        |    tfs_init()
        |    if tfs_freeinodes() > 0
        |        putchar(89)
        |    else
        |        putchar(78)
        |    0
        |""".stripMargin, prefill = prefill)
    output shouldBe "Y"
  }
}
