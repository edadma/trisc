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

    "throws when the leaf fills up (split is Phase 9d)" in {
      val f = fixture()
      var ino = f.ino
      // Pack 60-byte names → recLen 72. UsableSize = 4084. We can fit
      // (4084 - 12) / 72 = 56 entries before tryInsert fails.
      var i = 0
      var ok = true
      while ok do
        try
          ino = HTree.insert(
            ino, f.dev, f.bm, f.owner,
            "x" * 60 + f"$i%04d", 100 + i, DirEntry.TypeRegular,
          )
          i += 1
        catch case _: SfsCorruptError => ok = false
      i should be > 0
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
  }
