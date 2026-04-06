package io.github.edadma.trisc

object TFS:
  val MAGIC = 0x54465300 // "TFS\0"
  val VERSION = 1

  // Structure sizes
  val INODE_SIZE = 32
  val DIR_ENTRY_SIZE = 16
  val DIR_NAME_LEN = 14
  val NUM_DIRECT = 6

  // Special inodes
  val ROOT_INODE = 1

  // File type bits (mode bits 15-12, matches Unix)
  val S_IFMT = 0xf000
  val S_IFREG = 0x1000
  val S_IFDIR = 0x2000
  val S_IFCHR = 0x3000
  val S_IFBLK = 0x4000

  // Default permissions
  val DEFAULT_DIR_PERM = 0x1ed // rwxr-xr-x (0755)
  val DEFAULT_FILE_PERM = 0x1a4 // rw-r--r-- (0644)
  val DEFAULT_DEV_PERM = 0x1b6 // rw-rw-rw- (0666)

  // Superblock field offsets (block 1)
  val SB_MAGIC = 0
  val SB_VERSION = 4
  val SB_BLOCK_SIZE = 6
  val SB_TOTAL_BLOCKS = 8
  val SB_TOTAL_INODES = 10
  val SB_INODE_BITMAP = 12
  val SB_BLOCK_BITMAP = 14
  val SB_INODE_TABLE = 16
  val SB_FIRST_DATA = 18
  val SB_FREE_BLOCKS = 20
  val SB_FREE_INODES = 22

  // Inode field offsets (relative to inode start)
  val INO_MODE = 0 // 2 bytes
  val INO_NLINKS = 2 // 1 byte
  val INO_UID = 3 // 1 byte
  val INO_GID = 4 // 1 byte
  // 5: reserved
  val INO_SIZE = 6 // 4 bytes
  val INO_MTIME = 10 // 4 bytes
  val INO_CTIME = 14 // 4 bytes
  val INO_DIRECT0 = 18 // 6 x 2 bytes = 12 bytes
  val INO_INDIRECT = 30 // 2 bytes
  // Total: 32 bytes

  def format(blockSize: Int, totalBlocks: Int, maxInodes: Int, prefill: String, now: Int = 0, files: Map[String, Array[Byte]] = Map.empty): Array[Byte] =
    require(blockSize >= INODE_SIZE, "block size too small for inodes")
    require(blockSize >= DIR_ENTRY_SIZE, "block size too small for dir entries")
    require(maxInodes > 1, "need at least 2 inodes")
    val f = new Formatter(blockSize, totalBlocks, maxInodes, now, files)
    f.run(prefill)
    f.disk

  private def ceilDiv(a: Int, b: Int): Int = (a + b - 1) / b

  private class Formatter(blockSize: Int, totalBlocks: Int, maxInodes: Int, now: Int, files: Map[String, Array[Byte]] = Map.empty):
    val disk = new Array[Byte](totalBlocks * blockSize)

    // Layout
    val inodesPerBlock: Int = blockSize / INODE_SIZE
    val dirEntriesPerBlock: Int = blockSize / DIR_ENTRY_SIZE
    val inodeBitmapBlocks: Int = ceilDiv(maxInodes, blockSize * 8)
    val blockBitmapBlocks: Int = ceilDiv(totalBlocks, blockSize * 8)
    val inodeBitmapStart: Int = 2 // block 0 = boot, block 1 = superblock
    val blockBitmapStart: Int = inodeBitmapStart + inodeBitmapBlocks
    val inodeTableStart: Int = blockBitmapStart + blockBitmapBlocks
    val inodeTableBlocks: Int = ceilDiv(maxInodes, inodesPerBlock)
    val firstDataBlock: Int = inodeTableStart + inodeTableBlocks

    // Allocation state
    var freeBlocks: Int = totalBlocks - firstDataBlock
    var freeInodes: Int = maxInodes - 1 // inode 0 reserved
    var nextBlock: Int = firstDataBlock
    var nextInode: Int = 2 // 0 reserved, 1 = root

    // Directory tracking
    case class DirState(dataBlock: Int, var entryCount: Int, var nlinks: Int)
    val dirState = scala.collection.mutable.Map[Int, DirState]()
    val pathToInode = scala.collection.mutable.Map[String, Int]()

    def run(prefill: String): Unit =
      // Mark metadata blocks used in block bitmap
      for b <- 0 until firstDataBlock do markBlockUsed(b)
      markInodeUsed(0)

      // Create root directory
      markInodeUsed(ROOT_INODE)
      freeInodes -= 1
      val rootBlk = allocBlock()
      writeInode(ROOT_INODE, S_IFDIR | DEFAULT_DIR_PERM, 2, 0, 0, 2 * DIR_ENTRY_SIZE, Seq(rootBlk))
      writeDirEntry(rootBlk, 0, ROOT_INODE, ".")
      writeDirEntry(rootBlk, 1, ROOT_INODE, "..")
      dirState(ROOT_INODE) = DirState(rootBlk, 2, 2)
      pathToInode("/") = ROOT_INODE

      // Parse prefill
      for line <- prefill.linesIterator.map(_.trim) if line.nonEmpty do
        processLine(line)

      // Write final nlinks to directory inodes
      for (ino, state) <- dirState do
        val off = inodeTableStart * blockSize + ino * INODE_SIZE + INO_NLINKS
        disk(off) = state.nlinks.toByte

      // Write superblock (block 1)
      val sb = blockSize
      writeInt(disk, sb + SB_MAGIC, MAGIC)
      writeShort(disk, sb + SB_VERSION, VERSION)
      writeShort(disk, sb + SB_BLOCK_SIZE, blockSize)
      writeShort(disk, sb + SB_TOTAL_BLOCKS, totalBlocks)
      writeShort(disk, sb + SB_TOTAL_INODES, maxInodes)
      writeShort(disk, sb + SB_INODE_BITMAP, inodeBitmapStart)
      writeShort(disk, sb + SB_BLOCK_BITMAP, blockBitmapStart)
      writeShort(disk, sb + SB_INODE_TABLE, inodeTableStart)
      writeShort(disk, sb + SB_FIRST_DATA, firstDataBlock)
      writeShort(disk, sb + SB_FREE_BLOCKS, freeBlocks)
      writeShort(disk, sb + SB_FREE_INODES, freeInodes)

    private def processLine(line: String): Unit =
      val parts = splitLine(line)
      if parts.length < 2 then sys.error(s"TFS: malformed line: $line")
      val path = parts(0)
      val kind = parts(1)
      val parentIno = ensureParents(path)
      val name = path.split('/').last

      kind match
        case "char" =>
          require(parts.length >= 4, s"TFS: char device needs major minor: $line")
          val ino = allocInode()
          val devNum = (parts(2).toInt << 8) | (parts(3).toInt & 0xff)
          writeInode(ino, S_IFCHR | DEFAULT_DEV_PERM, 1, 0, 0, 0, Seq(devNum))
          addDirEntry(parentIno, name, ino)
        case "block" =>
          require(parts.length >= 4, s"TFS: block device needs major minor: $line")
          val ino = allocInode()
          val devNum = (parts(2).toInt << 8) | (parts(3).toInt & 0xff)
          writeInode(ino, S_IFBLK | DEFAULT_DEV_PERM, 1, 0, 0, 0, Seq(devNum))
          addDirEntry(parentIno, name, ino)
        case "file" =>
          val content = files.get(path) match
            case Some(data) => data
            case None => extractContent(parts)
          val ino = allocInode()
          if content.nonEmpty then writeFileWithContent(ino, content)
          else writeInode(ino, S_IFREG | DEFAULT_FILE_PERM, 1, 0, 0, 0)
          addDirEntry(parentIno, name, ino)
        case "dir" =>
          if !pathToInode.contains(path) then
            val ino = allocInode()
            createChildDir(ino, parentIno)
            addDirEntry(parentIno, name, ino)
            dirState(parentIno).nlinks += 1
            pathToInode(path) = ino
        case other =>
          sys.error(s"TFS: unknown type '$other' in: $line")

    private def createChildDir(ino: Int, parentIno: Int): Unit =
      val blk = allocBlock()
      writeInode(ino, S_IFDIR | DEFAULT_DIR_PERM, 2, 0, 0, 2 * DIR_ENTRY_SIZE, Seq(blk))
      writeDirEntry(blk, 0, ino, ".")
      writeDirEntry(blk, 1, parentIno, "..")
      dirState(ino) = DirState(blk, 2, 2)

    private def ensureParents(path: String): Int =
      val segments = path.stripPrefix("/").split('/')
      if segments.length <= 1 then return ROOT_INODE

      var parentIno = ROOT_INODE
      var currentPath = ""
      for seg <- segments.dropRight(1) do
        currentPath = currentPath + "/" + seg
        pathToInode.get(currentPath) match
          case Some(ino) => parentIno = ino
          case None =>
            val ino = allocInode()
            createChildDir(ino, parentIno)
            addDirEntry(parentIno, seg, ino)
            dirState(parentIno).nlinks += 1
            pathToInode(currentPath) = ino
            parentIno = ino
      parentIno

    private def writeFileWithContent(ino: Int, content: Array[Byte]): Unit =
      val blocksNeeded = ceilDiv(content.length, blockSize)
      val dataBlocks = (0 until blocksNeeded).map(_ => allocBlock())

      val indirectBlk =
        if blocksNeeded > NUM_DIRECT then
          val blk = allocBlock()
          val off = blk * blockSize
          for i <- NUM_DIRECT until blocksNeeded do
            writeShort(disk, off + (i - NUM_DIRECT) * 2, dataBlocks(i))
          blk
        else 0

      writeInode(ino, S_IFREG | DEFAULT_FILE_PERM, 1, 0, 0, content.length, dataBlocks.take(NUM_DIRECT), indirectBlk)

      for (blk, i) <- dataBlocks.zipWithIndex do
        val srcOff = i * blockSize
        val len = math.min(blockSize, content.length - srcOff)
        System.arraycopy(content, srcOff, disk, blk * blockSize, len)

    // ---- Allocation ----

    private def allocInode(): Int =
      require(freeInodes > 0, "TFS: out of inodes")
      val ino = nextInode
      nextInode += 1
      markInodeUsed(ino)
      freeInodes -= 1
      ino

    private def allocBlock(): Int =
      require(freeBlocks > 0, "TFS: out of blocks")
      val blk = nextBlock
      nextBlock += 1
      markBlockUsed(blk)
      freeBlocks -= 1
      blk

    private def markInodeUsed(ino: Int): Unit =
      val off = inodeBitmapStart * blockSize + ino / 8
      disk(off) = (disk(off) | (1 << (ino % 8))).toByte

    private def markBlockUsed(blk: Int): Unit =
      val off = blockBitmapStart * blockSize + blk / 8
      disk(off) = (disk(off) | (1 << (blk % 8))).toByte

    // ---- Inode I/O ----

    private def writeInode(
        ino: Int,
        mode: Int,
        nlinks: Int,
        uid: Int,
        gid: Int,
        size: Int,
        direct: Seq[Int] = Nil,
        indirect: Int = 0,
    ): Unit =
      val off = inodeTableStart * blockSize + ino * INODE_SIZE
      writeShort(disk, off + INO_MODE, mode)
      disk(off + INO_NLINKS) = nlinks.toByte
      disk(off + INO_UID) = uid.toByte
      disk(off + INO_GID) = gid.toByte
      writeInt(disk, off + INO_SIZE, size)
      writeInt(disk, off + INO_MTIME, now)
      writeInt(disk, off + INO_CTIME, now)
      for (blk, i) <- direct.zipWithIndex if i < NUM_DIRECT do
        writeShort(disk, off + INO_DIRECT0 + i * 2, blk)
      if indirect != 0 then writeShort(disk, off + INO_INDIRECT, indirect)

    private def updateInodeSize(ino: Int, size: Int): Unit =
      writeInt(disk, inodeTableStart * blockSize + ino * INODE_SIZE + INO_SIZE, size)

    // ---- Directory I/O ----

    private def writeDirEntry(dataBlock: Int, slot: Int, ino: Int, name: String): Unit =
      val off = dataBlock * blockSize + slot * DIR_ENTRY_SIZE
      writeShort(disk, off, ino)
      val bytes = name.getBytes("UTF-8")
      System.arraycopy(bytes, 0, disk, off + 2, math.min(bytes.length, DIR_NAME_LEN))

    private def addDirEntry(parentIno: Int, name: String, childIno: Int): Unit =
      val state = dirState(parentIno)
      require(state.entryCount < dirEntriesPerBlock, s"TFS: directory full (max $dirEntriesPerBlock entries)")
      writeDirEntry(state.dataBlock, state.entryCount, childIno, name)
      state.entryCount += 1
      updateInodeSize(parentIno, state.entryCount * DIR_ENTRY_SIZE)

    // ---- Prefill parsing ----

    private def extractContent(parts: Seq[String]): Array[Byte] =
      if parts.length > 2 then
        val quoted = parts.drop(2).mkString(" ")
        if quoted.startsWith("\"") && quoted.endsWith("\"") then
          quoted.substring(1, quoted.length - 1).getBytes("UTF-8")
        else quoted.getBytes("UTF-8")
      else Array.emptyByteArray

    private def splitLine(line: String): Seq[String] =
      val result = scala.collection.mutable.ArrayBuffer[String]()
      var i = 0
      while i < line.length do
        if line(i) == '"' then
          val end = line.indexOf('"', i + 1)
          if end < 0 then
            result += line.substring(i)
            i = line.length
          else
            result += line.substring(i, end + 1)
            i = end + 1
        else if line(i).isWhitespace then i += 1
        else
          val end = line.indexWhere(_.isWhitespace, i)
          if end < 0 then
            result += line.substring(i)
            i = line.length
          else
            result += line.substring(i, end)
            i = end
      result.toSeq

    // ---- Binary helpers ----

    private def writeShort(d: Array[Byte], off: Int, v: Int): Unit =
      d(off) = ((v >> 8) & 0xff).toByte
      d(off + 1) = (v & 0xff).toByte

    private def writeInt(d: Array[Byte], off: Int, v: Int): Unit =
      d(off) = ((v >> 24) & 0xff).toByte
      d(off + 1) = ((v >> 16) & 0xff).toByte
      d(off + 2) = ((v >> 8) & 0xff).toByte
      d(off + 3) = (v & 0xff).toByte
