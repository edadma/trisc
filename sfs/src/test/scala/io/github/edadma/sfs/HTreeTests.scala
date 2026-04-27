package io.github.edadma.sfs

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Constants.*

/** Tests for the directory-level HTree operations. Chunk 9c handles only
  * `tree_depth = 0` with arbitrarily many leaves directly under the root.
  * Splits are deferred to chunk 9d. */
class HTreeTests extends AnyFreeSpec with Matchers:

  // ---- Test setup -----------------------------------------------------

  private def smallOpts: FormatOptions =
    FormatOptions(
      totalInodes = 32,
      journalBlocks = 16,
      volumeName = "htree",
      uuid = (1 to 16).map(_.toByte).toIndexedSeq,
      formatTime = 0x6800_4321L,
    )

  /** Format a fresh device + load its block bitmap. */
  private def fresh(): (RamBlockDevice, Layout, Bitmap) =
    val dev = RamBlockDevice.default()
    val layout = Sfs.format(dev, smallOpts)
    val bm = new Bitmap(
      dev,
      startBlock = layout.blockBitmapStart.toLong,
      lengthBlocks = layout.blockBitmapLen,
      totalBits = layout.totalBlocks,
    )
    bm.load()
    (dev, layout, bm)

  /** Format a larger 32 MiB device for tests that need a directory big
    * enough to overflow the root index_entries (~510+ data blocks for
    * one directory). */
  private def freshLarge(): (RamBlockDevice, Layout, Bitmap) =
    val dev = new RamBlockDevice(8192L)
    val layout = Sfs.format(dev, smallOpts)
    val bm = new Bitmap(
      dev,
      startBlock = layout.blockBitmapStart.toLong,
      lengthBlocks = layout.blockBitmapLen,
      totalBits = layout.totalBlocks,
    )
    bm.load()
    (dev, layout, bm)

  /** Even larger device: 64 MiB. Used for tests that need a directory
    * large enough to fill an interior block at depth 1 (~1000+ data
    * blocks for one directory). */
  private def freshHuge(): (RamBlockDevice, Layout, Bitmap) =
    val dev = new RamBlockDevice(16384L)
    val layout = Sfs.format(dev, smallOpts)
    val bm = new Bitmap(
      dev,
      startBlock = layout.blockBitmapStart.toLong,
      lengthBlocks = layout.blockBitmapLen,
      totalBits = layout.totalBlocks,
    )
    bm.load()
    (dev, layout, bm)

  /** Read the directory's logical block 0 (root) into a fresh buffer. */
  private def readRoot(dev: BlockDevice, ino: Inode, ownerInode: Int): Array[Byte] =
    val reader = new ExtentReader(dev, ino)
    reader.physicalBlock(0L) match
      case BlockMapping.Concrete(p) =>
        val buf = new Array[Byte](BlockSize)
        dev.readBlock(p, buf)
        buf
      case other => fail(s"root block not concrete: $other")

  /** Open a freshly formatted 32 MiB device, mount it, and return both
    * the device and the live Sfs instance. */
  private def freshLargeMounted(): (RamBlockDevice, Sfs) =
    val dev = new RamBlockDevice(8192L)
    Sfs.format(dev, smallOpts)
    val sfs = Sfs.mount(dev)
    (dev, sfs)

  /** Resolve a directory's logical block number to its physical
    * address through the inode's extent map. */
  private def physical(dev: BlockDevice, ino: Inode, logical: Long): Long =
    val reader = new ExtentReader(dev, ino)
    reader.physicalBlock(logical) match
      case BlockMapping.Concrete(p) => p
      case other                    => fail(s"logical $logical not concrete: $other")

  /** A fresh, empty directory-shaped inode with no extents. */
  private def blankDirInode(): Inode = Inode(
    mode = 0x41ed,
    linkCount = 2,
    uid = 0,
    gid = 0,
    flags = 0,
    size = 0L,
    blockCount = 0,
    generation = 0,
    atimeSec = 0,
    atimeNsec = 0,
    mtimeSec = 0,
    mtimeNsec = 0,
    ctimeSec = 0,
    ctimeNsec = 0,
    crtimeSec = 0,
    crtimeNsec = 0,
    body = InodeBody.EmptyExtents,
    indirect1 = 0,
    indirect2 = 0,
    indirect3 = 0,
    xattrBlock = 0,
  )

  /** Test fixture: device, bitmap, owner inode number, parent inode
    * number, and a freshly-initialized directory inode. */
  private case class Fixture(
      dev: RamBlockDevice,
      bm: Bitmap,
      owner: Int,
      parent: Int,
      ino: Inode,
  )

  /** Format + initDirectory in one shot so each test starts with a
    * working empty directory. */
  private def fixture(owner: Int = 5, parent: Int = 2): Fixture =
    val (dev, _, bm) = fresh()
    val ino = HTree.initDirectory(blankDirInode(), dev, bm, owner, parent)
    Fixture(dev, bm, owner, parent, ino)

  // ---- initDirectory --------------------------------------------------

  "initDirectory" - {

    "appends two data blocks (root + one leaf)" in {
      val f = fixture()
      ExtentAllocator.totalBlockCount(f.ino, f.dev) shouldBe 2L
    }

    "writes a parseable root with dot/dotdot pointing at owner/parent" in {
      val f = fixture(owner = 7, parent = 9)
      val rootBuf = new Array[Byte](BlockSize)
      val reader = new ExtentReader(f.dev, f.ino)
      reader.physicalBlock(0L) match
        case BlockMapping.Concrete(p) => f.dev.readBlock(p, rootBuf)
        case _                        => fail("root block not concrete")
      val root = DirRootBlock.unpack(rootBuf, f.owner)
      root.dot.inode shouldBe 7
      root.dotdot.inode shouldBe 9
      root.treeDepth shouldBe 0
      root.hashVersion shouldBe HashFnv1a
      HTree.liveIndexEntries(root) shouldBe IndexedSeq((0, 1))
    }

    "writes a parseable empty leaf block" in {
      val f = fixture()
      val leafBuf = new Array[Byte](BlockSize)
      val reader = new ExtentReader(f.dev, f.ino)
      reader.physicalBlock(1L) match
        case BlockMapping.Concrete(p) => f.dev.readBlock(p, leafBuf)
        case _                        => fail("leaf block not concrete")
      DirTail.verify(leafBuf, f.owner)
      val xs = DirLeaf.entries(leafBuf)
      xs.length shouldBe 1
      xs.head._2.inode shouldBe 0
    }
  }

  // ---- lookup ---------------------------------------------------------

  "lookup" - {

    """resolves "." and ".."""" in {
      val f = fixture(owner = 7, parent = 9)
      HTree.lookup(f.ino, f.dev, f.owner, ".") shouldBe Some((7, DirEntry.TypeDirectory))
      HTree.lookup(f.ino, f.dev, f.owner, "..") shouldBe Some((9, DirEntry.TypeDirectory))
    }

    "returns None for an absent name" in {
      val f = fixture()
      HTree.lookup(f.ino, f.dev, f.owner, "ghost") shouldBe None
    }

    "finds a name after insert" in {
      val f = fixture()
      val grown = HTree.insert(f.ino, f.dev, f.bm, f.owner, "hello", 100, DirEntry.TypeRegular)
      HTree.lookup(grown, f.dev, f.owner, "hello") shouldBe Some((100, DirEntry.TypeRegular))
    }
  }

  // ---- insert ---------------------------------------------------------

  "insert" - {

    "ten distinct names round-trip through lookup" in {
      val f = fixture()
      var ino = f.ino
      val names = (0 until 10).map(i => f"name_$i%02d")
      var inode = 100
      for n <- names do
        ino = HTree.insert(ino, f.dev, f.bm, f.owner, n, inode, DirEntry.TypeRegular)
        inode += 1

      var n2 = 100
      for nm <- names do
        HTree.lookup(ino, f.dev, f.owner, nm) shouldBe Some((n2, DirEntry.TypeRegular))
        n2 += 1
    }

    """rejects "." and "..""" in {
      val f = fixture()
      an[IllegalArgumentException] should be thrownBy
        HTree.insert(f.ino, f.dev, f.bm, f.owner, ".", 100, DirEntry.TypeRegular)
      an[IllegalArgumentException] should be thrownBy
        HTree.insert(f.ino, f.dev, f.bm, f.owner, "..", 100, DirEntry.TypeRegular)
    }

    "raises SfsExistsError on duplicate name" in {
      val f = fixture()
      val ino1 = HTree.insert(f.ino, f.dev, f.bm, f.owner, "hello", 100, DirEntry.TypeRegular)
      an[SfsExistsError] should be thrownBy
        HTree.insert(ino1, f.dev, f.bm, f.owner, "hello", 200, DirEntry.TypeRegular)
    }

    "splits a full leaf and keeps every name reachable" in {
      val f = fixture()
      var ino = f.ino
      // 60-byte names → recLen 72. ~56 entries fit before split.
      val names = (0 until 80).map(i => "x" * 60 + f"$i%04d")
      var inode = 100
      for n <- names do
        ino = HTree.insert(ino, f.dev, f.bm, f.owner, n, inode, DirEntry.TypeRegular)
        inode += 1

      // Directory has root + at least 2 leaves after split.
      ExtentAllocator.totalBlockCount(ino, f.dev) should be >= 3L

      var n2 = 100
      for nm <- names do
        HTree.lookup(ino, f.dev, f.owner, nm) shouldBe Some((n2, DirEntry.TypeRegular))
        n2 += 1
    }

    "survives many splits across diverse names" in {
      val f = fixture()
      var ino = f.ino
      val names = (0 until 500).map(i => f"file_$i%04d_with_some_padding")
      var inode = 100
      for n <- names do
        ino = HTree.insert(ino, f.dev, f.bm, f.owner, n, inode, DirEntry.TypeRegular)
        inode += 1

      var n2 = 100
      for nm <- names do
        HTree.lookup(ino, f.dev, f.owner, nm) shouldBe Some((n2, DirEntry.TypeRegular))
        n2 += 1
      val listed = HTree.list(ino, f.dev, f.owner).map(_.name).toSet
      listed shouldBe (names.toSet + "." + "..")
    }

    "split + delete + reinsert keeps everything consistent" in {
      val f = fixture()
      var ino = f.ino
      val names = (0 until 100).map(i => f"file_$i%04d_padding_to_force_splits")
      var inode = 100
      for n <- names do
        ino = HTree.insert(ino, f.dev, f.bm, f.owner, n, inode, DirEntry.TypeRegular)
        inode += 1
      // Delete every other entry, then reinsert.
      for i <- names.indices.filter(_ % 2 == 0) do
        ino = HTree.delete(ino, f.dev, f.bm, f.owner, names(i))
      for i <- names.indices.filter(_ % 2 == 0) do
        ino = HTree.insert(ino, f.dev, f.bm, f.owner, names(i), 1000 + i, DirEntry.TypeRegular)

      // Even-indexed entries now have inode 1000+i; odd-indexed have inode 100+i.
      for i <- names.indices do
        val expected = if i % 2 == 0 then 1000 + i else 100 + i
        HTree.lookup(ino, f.dev, f.owner, names(i)) shouldBe Some((expected, DirEntry.TypeRegular))
    }

    "promotes treeDepth 0 → 1 once root.indexEntries fills" in {
      val (dev, _, bm) = freshLarge()
      val owner = 5
      var ino = HTree.initDirectory(blankDirInode(), dev, bm, owner, 2)

      // Pad names so each leaf holds only ~15 entries (NAME_MAX-sized
      // records). Insert until the root promotes to treeDepth = 1, plus
      // a few extra to keep growing the tree.
      val pad = "x" * 248
      var i = 0
      val totalToInsert = 6000
      while i < totalToInsert do
        ino = HTree.insert(ino, dev, bm, owner, pad + f"$i%07d", 100 + i, DirEntry.TypeRegular)
        i += 1

      val root = DirRootBlock.unpack(readRoot(dev, ino, owner), owner)
      root.treeDepth shouldBe 1

      // Spot-check a handful of names spanning the insertion range.
      val samples = Vector(0, 1, 100, 1000, totalToInsert / 2, totalToInsert - 1)
      for s <- samples do
        val name = pad + f"$s%07d"
        HTree.lookup(ino, dev, owner, name) shouldBe Some((100 + s, DirEntry.TypeRegular))
    }

    "delete + reinsert after promotion preserves all entries" in {
      val (dev, _, bm) = freshLarge()
      val owner = 5
      var ino = HTree.initDirectory(blankDirInode(), dev, bm, owner, 2)

      val pad = "x" * 248
      val totalToInsert = 6000
      var i = 0
      while i < totalToInsert do
        ino = HTree.insert(ino, dev, bm, owner, pad + f"$i%07d", 100 + i, DirEntry.TypeRegular)
        i += 1

      DirRootBlock.unpack(readRoot(dev, ino, owner), owner).treeDepth shouldBe 1

      // Delete every 7th entry and reinsert with a different inode.
      val touched = (0 until totalToInsert by 7).toVector
      for k <- touched do
        ino = HTree.delete(ino, dev, bm, owner, pad + f"$k%07d")
      for k <- touched do
        ino = HTree.insert(ino, dev, bm, owner, pad + f"$k%07d", 9000000 + k, DirEntry.TypeRegular)

      for k <- 0 until totalToInsert do
        val expected = if k % 7 == 0 then 9000000 + k else 100 + k
        HTree.lookup(ino, dev, owner, pad + f"$k%07d") shouldBe
          Some((expected, DirEntry.TypeRegular))
    }

    "splits an interior block at depth 1 (root has > 2 entries)" in {
      val (dev, _, bm) = freshHuge()
      val owner = 5
      var ino = HTree.initDirectory(blankDirInode(), dev, bm, owner, 2)

      // ~12k NAME_MAX-padded inserts: forces promotion (~3.5k inserts in)
      // and then keeps loading until at least one interior block fills
      // and splits, growing root.indexEntries past 2.
      val pad = "x" * 248
      val totalToInsert = 12000
      var i = 0
      while i < totalToInsert do
        ino = HTree.insert(ino, dev, bm, owner, pad + f"$i%07d", 100 + i, DirEntry.TypeRegular)
        i += 1

      val root = DirRootBlock.unpack(readRoot(dev, ino, owner), owner)
      root.treeDepth shouldBe 1
      HTree.liveIndexEntries(root).length should be > 2

      // Spot-check that lookup still routes correctly through the
      // multiple-interior-block layout.
      val samples = Vector(0, 1, 100, 1000, 4000, 8000, totalToInsert - 1)
      for s <- samples do
        HTree.lookup(ino, dev, owner, pad + f"$s%07d") shouldBe
          Some((100 + s, DirEntry.TypeRegular))

      // List round-trips every inserted name plus dot/dotdot.
      HTree.list(ino, dev, owner).map(_.name).toSet shouldBe
        ((0 until totalToInsert).map(s => pad + f"$s%07d").toSet + "." + "..")
    }
  }

  // ---- delete ---------------------------------------------------------

  "delete" - {

    "removes an inserted name" in {
      val f = fixture()
      val ino1 = HTree.insert(f.ino, f.dev, f.bm, f.owner, "alpha", 100, DirEntry.TypeRegular)
      val ino2 = HTree.delete(ino1, f.dev, f.bm, f.owner, "alpha")
      HTree.lookup(ino2, f.dev, f.owner, "alpha") shouldBe None
    }

    """rejects "." and "..""" in {
      val f = fixture()
      an[IllegalArgumentException] should be thrownBy
        HTree.delete(f.ino, f.dev, f.bm, f.owner, ".")
      an[IllegalArgumentException] should be thrownBy
        HTree.delete(f.ino, f.dev, f.bm, f.owner, "..")
    }

    "raises SfsNotFoundError on missing name" in {
      val f = fixture()
      an[SfsNotFoundError] should be thrownBy
        HTree.delete(f.ino, f.dev, f.bm, f.owner, "ghost")
    }

    "tombstone slack is reused by a subsequent insert" in {
      val f = fixture()
      var ino = f.ino
      ino = HTree.insert(ino, f.dev, f.bm, f.owner, "first", 100, DirEntry.TypeRegular)
      ino = HTree.insert(ino, f.dev, f.bm, f.owner, "second", 101, DirEntry.TypeRegular)
      ino = HTree.delete(ino, f.dev, f.bm, f.owner, "first")
      ino = HTree.insert(ino, f.dev, f.bm, f.owner, "third_with_a_longer_name", 102, DirEntry.TypeRegular)
      HTree.lookup(ino, f.dev, f.owner, "second") shouldBe Some((101, DirEntry.TypeRegular))
      HTree.lookup(ino, f.dev, f.owner, "third_with_a_longer_name") shouldBe Some((102, DirEntry.TypeRegular))
      HTree.lookup(ino, f.dev, f.owner, "first") shouldBe None
    }
  }

  // ---- list -----------------------------------------------------------

  "list" - {

    "returns just dot/dotdot on an empty directory" in {
      val f = fixture(owner = 7, parent = 9)
      val xs = HTree.list(f.ino, f.dev, f.owner)
      xs.map(_.name).toSet shouldBe Set(".", "..")
      xs.find(_.name == ".").get.inode shouldBe 7
      xs.find(_.name == "..").get.inode shouldBe 9
    }

    "returns dot/dotdot + every inserted entry" in {
      val f = fixture()
      var ino = f.ino
      val names = Vector("alpha", "beta", "gamma", "delta", "epsilon")
      var inode = 100
      for n <- names do
        ino = HTree.insert(ino, f.dev, f.bm, f.owner, n, inode, DirEntry.TypeRegular)
        inode += 1

      val xs = HTree.list(ino, f.dev, f.owner)
      xs.map(_.name).toSet shouldBe (names :+ "." :+ "..").toSet
    }

    "respects deletes (tombstones are skipped)" in {
      val f = fixture()
      var ino = f.ino
      ino = HTree.insert(ino, f.dev, f.bm, f.owner, "alive", 100, DirEntry.TypeRegular)
      ino = HTree.insert(ino, f.dev, f.bm, f.owner, "dead", 101, DirEntry.TypeRegular)
      ino = HTree.delete(ino, f.dev, f.bm, f.owner, "dead")
      val xs = HTree.list(ino, f.dev, f.owner)
      xs.map(_.name).toSet shouldBe Set(".", "..", "alive")
    }

    "exact count: dot, dotdot, and N entries — no duplicates" in {
      val f = fixture()
      var ino = f.ino
      val names = (0 until 200).map(i => f"f_$i%04d_padding_to_force_splits")
      var inode = 100
      for n <- names do
        ino = HTree.insert(ino, f.dev, f.bm, f.owner, n, inode, DirEntry.TypeRegular)
        inode += 1
      val listed = HTree.list(ino, f.dev, f.owner)
      // Set membership AND length: catches duplicate yields from the walker
      // (e.g. tombstones leaking through, or a leaf being visited twice).
      listed.length shouldBe (names.length + 2)
      listed.map(_.name).toSet shouldBe (names.toSet + "." + "..")
    }

    "list contains correct file_type for each entry" in {
      val f = fixture()
      var ino = f.ino
      ino = HTree.insert(ino, f.dev, f.bm, f.owner, "regular_file", 100, DirEntry.TypeRegular)
      ino = HTree.insert(ino, f.dev, f.bm, f.owner, "subdir", 101, DirEntry.TypeDirectory)
      ino = HTree.insert(ino, f.dev, f.bm, f.owner, "symlink", 102, DirEntry.TypeSymlink)
      ino = HTree.insert(ino, f.dev, f.bm, f.owner, "device", 103, DirEntry.TypeOther)

      val byName = HTree.list(ino, f.dev, f.owner).map(e => e.name -> e.fileType).toMap
      byName("regular_file") shouldBe DirEntry.TypeRegular
      byName("subdir") shouldBe DirEntry.TypeDirectory
      byName("symlink") shouldBe DirEntry.TypeSymlink
      byName("device") shouldBe DirEntry.TypeOther
    }
  }

  // ---- persistence (Sfs.format → mount → writeInode → unmount → mount) ----

  "persistence" - {

    "directory state survives writeInode → unmount → mount → readInode" in {
      val (dev, sfs) = freshLargeMounted()
      var ino = sfs.readInode(InoRoot)

      val names = (0 until 50).map(i => f"persist_$i%03d")
      var childIno = 1000
      for n <- names do
        ino = HTree.insert(ino, sfs.device, sfs.blockBitmap, InoRoot, n, childIno, DirEntry.TypeRegular)
        childIno += 1
      sfs.writeInode(InoRoot, ino)
      sfs.unmount()

      val sfs2 = Sfs.mount(dev)
      val recovered = sfs2.readInode(InoRoot)
      var childIno2 = 1000
      for n <- names do
        HTree.lookup(recovered, sfs2.device, InoRoot, n) shouldBe
          Some((childIno2, DirEntry.TypeRegular))
        childIno2 += 1
      // Plus dot/dotdot.
      HTree.list(recovered, sfs2.device, InoRoot).length shouldBe (names.length + 2)
      sfs2.unmount()
    }

    "post-promotion directory survives a mount cycle" in {
      val (dev, sfs) = freshLargeMounted()
      var ino = sfs.readInode(InoRoot)

      // Force promotion to treeDepth = 1.
      val pad = "x" * 248
      val total = 6000
      var i = 0
      while i < total do
        ino = HTree.insert(ino, sfs.device, sfs.blockBitmap, InoRoot, pad + f"$i%07d", 100 + i, DirEntry.TypeRegular)
        i += 1
      sfs.writeInode(InoRoot, ino)
      sfs.unmount()

      val sfs2 = Sfs.mount(dev)
      val recovered = sfs2.readInode(InoRoot)
      val rootBuf = readRoot(sfs2.device, recovered, InoRoot)
      DirRootBlock.unpack(rootBuf, InoRoot).treeDepth shouldBe 1
      // Spot-check a handful of names.
      for s <- Vector(0, 1, 100, total / 2, total - 1) do
        HTree.lookup(recovered, sfs2.device, InoRoot, pad + f"$s%07d") shouldBe
          Some((100 + s, DirEntry.TypeRegular))
      sfs2.unmount()
    }

    "block bitmap re-load reflects directory blocks allocated mid-session" in {
      val (dev, sfs) = freshLargeMounted()
      var ino = sfs.readInode(InoRoot)
      val freeBefore = sfs.blockBitmap.freeCount

      val names = (0 until 200).map(i => f"alloc_$i%04d_padding_to_force_a_split")
      var childIno = 1000
      for n <- names do
        ino = HTree.insert(ino, sfs.device, sfs.blockBitmap, InoRoot, n, childIno, DirEntry.TypeRegular)
        childIno += 1
      val freeAfter = sfs.blockBitmap.freeCount
      val consumedThisSession = freeBefore - freeAfter
      consumedThisSession should be > 0

      sfs.writeInode(InoRoot, ino)
      sfs.unmount()

      val sfs2 = Sfs.mount(dev)
      // Bitmap re-loaded from disk should have the same free count.
      sfs2.blockBitmap.freeCount shouldBe freeAfter
      sfs2.unmount()
    }
  }

  // ---- tail-checksum protection (block-swap defense) ------------------

  "directory tail integrity" - {

    "lookup raises SfsCorruptError when a leaf's CRC is corrupted" in {
      val f = fixture()
      val ino = HTree.insert(f.ino, f.dev, f.bm, f.owner, "victim", 100, DirEntry.TypeRegular)
      // Flip a single bit in the CRC field (last 4 bytes of the leaf block).
      val phys = physical(f.dev, ino, 1L)
      val buf = new Array[Byte](BlockSize)
      f.dev.readBlock(phys, buf)
      buf(BlockSize - 1) = (buf(BlockSize - 1) ^ 0x80).toByte
      f.dev.writeBlock(phys, buf)
      a[SfsCorruptError] should be thrownBy HTree.lookup(ino, f.dev, f.owner, "victim")
    }

    "lookup raises SfsCorruptError when a leaf's tail magic is wrong" in {
      val f = fixture()
      val ino = HTree.insert(f.ino, f.dev, f.bm, f.owner, "victim", 100, DirEntry.TypeRegular)
      val phys = physical(f.dev, ino, 1L)
      val buf = new Array[Byte](BlockSize)
      f.dev.readBlock(phys, buf)
      // Magic lives at BlockSize - 12 (4 bytes).
      Le.putU32(buf, BlockSize - 12, 0xdeadbeef)
      f.dev.writeBlock(phys, buf)
      a[SfsCorruptError] should be thrownBy HTree.lookup(ino, f.dev, f.owner, "victim")
    }

    "lookup raises SfsCorruptError when a leaf claims the wrong owner inode (block-swap)" in {
      val f = fixture(owner = 7, parent = 2)
      val ino = HTree.insert(f.ino, f.dev, f.bm, f.owner, "victim", 100, DirEntry.TypeRegular)
      val phys = physical(f.dev, ino, 1L)
      val buf = new Array[Byte](BlockSize)
      f.dev.readBlock(phys, buf)
      // owner_inode lives at BlockSize - 8 (4 bytes).
      Le.putU32(buf, BlockSize - 8, 999) // some *other* inode number
      f.dev.writeBlock(phys, buf)
      // Note: we don't refresh the CRC, so this would also fail the CRC
      // check; either failure is acceptable as long as we refuse the
      // block. Test asserts SfsCorruptError specifically.
      a[SfsCorruptError] should be thrownBy HTree.lookup(ino, f.dev, f.owner, "victim")
    }

    "lookup raises SfsCorruptError when the root's CRC is corrupted" in {
      val f = fixture()
      val ino = HTree.insert(f.ino, f.dev, f.bm, f.owner, "victim", 100, DirEntry.TypeRegular)
      val phys = physical(f.dev, ino, 0L)
      val buf = new Array[Byte](BlockSize)
      f.dev.readBlock(phys, buf)
      buf(BlockSize - 1) = (buf(BlockSize - 1) ^ 0x80).toByte
      f.dev.writeBlock(phys, buf)
      a[SfsCorruptError] should be thrownBy HTree.lookup(ino, f.dev, f.owner, "victim")
    }
  }

  // ---- name encoding edge cases ---------------------------------------

  "name encoding" - {

    "round-trips a name at NAME_MAX (255 bytes ASCII)" in {
      val f = fixture()
      val name = "n" * NameMax
      val ino = HTree.insert(f.ino, f.dev, f.bm, f.owner, name, 100, DirEntry.TypeRegular)
      HTree.lookup(ino, f.dev, f.owner, name) shouldBe Some((100, DirEntry.TypeRegular))
      HTree.list(ino, f.dev, f.owner).find(_.name == name) shouldBe defined
    }

    "round-trips non-ASCII UTF-8 names" in {
      val f = fixture()
      val names = Vector(
        "héllo",
        "日本語",
        "русский",
        "café_au_lait",
        "naïve",
        "🦀_crab",
      )
      var ino = f.ino
      var inode = 100
      for n <- names do
        ino = HTree.insert(ino, f.dev, f.bm, f.owner, n, inode, DirEntry.TypeRegular)
        inode += 1

      var inode2 = 100
      for n <- names do
        HTree.lookup(ino, f.dev, f.owner, n) shouldBe Some((inode2, DirEntry.TypeRegular))
        inode2 += 1

      HTree.list(ino, f.dev, f.owner).map(_.name).toSet shouldBe
        (names.toSet + "." + "..")
    }

    "rejects a name longer than NAME_MAX at the codec layer" in {
      // DirEntry's ctor rejects nameLen > NAME_MAX, so we don't need
      // HTree to repeat the check — confirm the underlying invariant.
      an[IllegalArgumentException] should be thrownBy
        DirEntry(100, DirEntry.TypeRegular, "x" * (NameMax + 1))
    }
  }

  // ---- emptied-directory survival -------------------------------------

  "empty directories" - {

    "delete every entry then list returns just dot/dotdot, lookup misses" in {
      val f = fixture()
      var ino = f.ino
      val names = Vector("a", "b", "c", "d", "e")
      var inode = 100
      for n <- names do
        ino = HTree.insert(ino, f.dev, f.bm, f.owner, n, inode, DirEntry.TypeRegular)
        inode += 1
      for n <- names do
        ino = HTree.delete(ino, f.dev, f.bm, f.owner, n)

      HTree.list(ino, f.dev, f.owner).map(_.name) shouldBe Vector(".", "..")
      for n <- names do
        HTree.lookup(ino, f.dev, f.owner, n) shouldBe None
    }

    "fully drained directory accepts new entries afterwards" in {
      val f = fixture()
      var ino = f.ino
      ino = HTree.insert(ino, f.dev, f.bm, f.owner, "first", 100, DirEntry.TypeRegular)
      ino = HTree.delete(ino, f.dev, f.bm, f.owner, "first")
      ino = HTree.insert(ino, f.dev, f.bm, f.owner, "second", 200, DirEntry.TypeRegular)
      HTree.lookup(ino, f.dev, f.owner, "second") shouldBe Some((200, DirEntry.TypeRegular))
      HTree.lookup(ino, f.dev, f.owner, "first") shouldBe None
    }
  }

  // ---- random-churn stress on a depth-1 tree --------------------------

  "depth-1 churn" - {

    "many alternating insert/delete cycles preserve invariants" in {
      val (dev, _, bm) = freshLarge()
      val owner = 5
      var ino = HTree.initDirectory(blankDirInode(), dev, bm, owner, 2)

      // Force promotion via NAME_MAX names.
      val pad = "x" * 248
      val seedSize = 6000
      var i = 0
      while i < seedSize do
        ino = HTree.insert(ino, dev, bm, owner, pad + f"$i%07d", 100 + i, DirEntry.TypeRegular)
        i += 1
      DirRootBlock.unpack(readRoot(dev, ino, owner), owner).treeDepth shouldBe 1

      // Pseudo-random churn: delete + reinsert with new inode numbers,
      // interleaved across the existing keyspace. Use a deterministic
      // PRNG so failures reproduce.
      val rng = new scala.util.Random(0x5f51L)
      for _ <- 0 until 1000 do
        val k = rng.nextInt(seedSize)
        val name = pad + f"$k%07d"
        // delete may fail (already deleted); then insert reuses the slot
        if HTree.lookup(ino, dev, owner, name).isDefined then
          ino = HTree.delete(ino, dev, bm, owner, name)
        else
          ino = HTree.insert(ino, dev, bm, owner, name, 500_000 + k, DirEntry.TypeRegular)

      // Final invariant check: every name that resolves to Some has the
      // file_type we set, and list count matches lookup count.
      val listed = HTree.list(ino, dev, owner).filter(e => e.name != "." && e.name != "..")
      listed.length should be > 0
      for e <- listed do
        e.fileType shouldBe DirEntry.TypeRegular
      val live = (0 until seedSize).count { k =>
        HTree.lookup(ino, dev, owner, pad + f"$k%07d").isDefined
      }
      live shouldBe listed.length
    }
  }
