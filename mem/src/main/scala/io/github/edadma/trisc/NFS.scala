package io.github.edadma.trisc

object NFS:
  val TYPE_REGULAR: Byte = 0
  val TYPE_DIR: Byte = 1
  val TYPE_CHAR: Byte = 2
  val TYPE_BLOCK: Byte = 3

  val FLAG_ACTIVE: Byte = 1

  val ENTRY_SIZE = 32
  val NAME_LEN = 16
  val MAGIC = 0x4e465300 // "NFS\0"
  val VERSION = 1
  val NO_PARENT = 0xffff

  private case class Entry(
      name: String,
      parent: Int,
      entryType: Byte,
      flags: Byte,
      fieldA: Int, // start sector or major
      fieldB: Int, // sector count or minor
      fileSize: Int,
      content: Array[Byte] = Array.emptyByteArray,
  )

  def format(sectorSize: Int, totalSectors: Int, prefill: String): Array[Byte] =
    require(sectorSize >= ENTRY_SIZE, s"sector size must be >= $ENTRY_SIZE")
    val disk = new Array[Byte](totalSectors * sectorSize)
    val entries = scala.collection.mutable.ArrayBuffer[Entry]()

    // Entry 0: root directory
    entries += Entry("/", NO_PARENT, TYPE_DIR, FLAG_ACTIVE, 0, 0, 0)

    for line <- prefill.linesIterator.map(_.trim) if line.nonEmpty do
      parseLine(line, entries)

    // Calculate layout
    val entriesPerSector = sectorSize / ENTRY_SIZE
    val dirSectors = (entries.size + entriesPerSector - 1) / entriesPerSector
    val firstDataSector = 1 + dirSectors
    var nextFreeSector = firstDataSector

    // Allocate data sectors for regular files with content
    val allocated = entries.toSeq.map { e =>
      if e.entryType == TYPE_REGULAR && e.fileSize > 0 then
        val needed = (e.fileSize + sectorSize - 1) / sectorSize
        val start = nextFreeSector
        nextFreeSector += needed
        e.copy(fieldA = start, fieldB = needed)
      else e
    }

    // Write superblock (sector 0)
    writeInt(disk, 0, MAGIC)
    writeShort(disk, 4, VERSION)
    writeShort(disk, 6, dirSectors * entriesPerSector)
    writeShort(disk, 8, allocated.size)
    writeShort(disk, 10, firstDataSector)
    writeShort(disk, 12, totalSectors)
    writeShort(disk, 14, nextFreeSector)

    // Write directory entries (starting at sector 1)
    for (entry, idx) <- allocated.zipWithIndex do
      val offset = sectorSize + idx * ENTRY_SIZE
      writeName(disk, offset, entry.name)
      writeShort(disk, offset + 16, entry.parent)
      disk(offset + 18) = entry.entryType
      disk(offset + 19) = entry.flags
      writeShort(disk, offset + 20, entry.fieldA)
      writeShort(disk, offset + 22, entry.fieldB)
      writeInt(disk, offset + 24, entry.fileSize)

    // Write file data
    for entry <- allocated do
      if entry.entryType == TYPE_REGULAR && entry.content.nonEmpty then
        val offset = entry.fieldA * sectorSize
        System.arraycopy(entry.content, 0, disk, offset, entry.content.length)

    disk

  private def parseLine(line: String, entries: scala.collection.mutable.ArrayBuffer[Entry]): Unit =
    val parts = splitLine(line)
    if parts.length < 2 then sys.error(s"NFS: malformed line: $line")
    val path = parts(0)
    val kind = parts(1)
    val parentIdx = ensureParents(path, entries)
    val name = path.split('/').last

    kind match
      case "char" =>
        require(parts.length >= 4, s"NFS: char device needs major minor: $line")
        entries += Entry(name, parentIdx, TYPE_CHAR, FLAG_ACTIVE, parts(2).toInt, parts(3).toInt, 0)
      case "block" =>
        require(parts.length >= 4, s"NFS: block device needs major minor: $line")
        entries += Entry(name, parentIdx, TYPE_BLOCK, FLAG_ACTIVE, parts(2).toInt, parts(3).toInt, 0)
      case "file" =>
        val content =
          if parts.length > 2 then
            val quoted = parts.drop(2).mkString(" ")
            if quoted.startsWith("\"") && quoted.endsWith("\"") then
              quoted.substring(1, quoted.length - 1).getBytes("UTF-8")
            else quoted.getBytes("UTF-8")
          else Array.emptyByteArray
        entries += Entry(name, parentIdx, TYPE_REGULAR, FLAG_ACTIVE, 0, 0, content.length, content)
      case "dir" =>
        entries += Entry(name, parentIdx, TYPE_DIR, FLAG_ACTIVE, 0, 0, 0)
      case other =>
        sys.error(s"NFS: unknown entry type '$other' in line: $line")

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
      else if line(i).isWhitespace then
        i += 1
      else
        val end = line.indexWhere(_.isWhitespace, i)
        if end < 0 then
          result += line.substring(i)
          i = line.length
        else
          result += line.substring(i, end)
          i = end
    result.toSeq

  private def ensureParents(path: String, entries: scala.collection.mutable.ArrayBuffer[Entry]): Int =
    val segments = path.stripPrefix("/").split('/')
    if segments.length <= 1 then return 0 // parent is root

    var parentIdx = 0
    for seg <- segments.dropRight(1) do
      entries.indexWhere(e => e.name == seg && e.parent == parentIdx && e.entryType == TYPE_DIR) match
        case -1 =>
          entries += Entry(seg, parentIdx, TYPE_DIR, FLAG_ACTIVE, 0, 0, 0)
          parentIdx = entries.size - 1
        case idx =>
          parentIdx = idx
    parentIdx

  private def writeName(disk: Array[Byte], offset: Int, name: String): Unit =
    val bytes = name.getBytes("UTF-8")
    System.arraycopy(bytes, 0, disk, offset, math.min(bytes.length, NAME_LEN))

  private def writeShort(disk: Array[Byte], offset: Int, value: Int): Unit =
    disk(offset) = ((value >> 8) & 0xff).toByte
    disk(offset + 1) = (value & 0xff).toByte

  private def writeInt(disk: Array[Byte], offset: Int, value: Int): Unit =
    disk(offset) = ((value >> 24) & 0xff).toByte
    disk(offset + 1) = ((value >> 16) & 0xff).toByte
    disk(offset + 2) = ((value >> 8) & 0xff).toByte
    disk(offset + 3) = (value & 0xff).toByte
