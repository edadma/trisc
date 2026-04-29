package io.github.edadma.trisc

/** Synthesise a tiny USTAR (POSIX 1003.1-1990) tar archive used by
  * Phase 4 chunk 5 as a fixed test fixture. The archive lives at
  * `/test.tar` in both the aarch64 and x86_64 SLIX ramdisks; the
  * `untar` slix-musl test extracts it via `mkdir` + `open(O_CREAT)`
  * + `write` and then stat-verifies the result.
  *
  * Single source of truth for the bytes — no host `tar` dependency,
  * no checked-in binary, no per-arch divergence. The shape is
  * intentionally minimal: one directory entry plus two regular
  * files, terminated by the standard two zero blocks.
  *
  * Layout (each entry is one 512-byte header + zero-or-more 512-byte
  * data blocks; data is zero-padded to the next 512-byte boundary):
  *
  *   tx/                 (typeflag '5', no data)
  *   tx/a.txt            (typeflag '0', 6 bytes "hello\n")
  *   tx/b.txt            (typeflag '0', 7 bytes "world!\n")
  *   <zero-block>
  *   <zero-block>
  */
object TestTar:
  private val BLOCK = 512

  // Public so the slix-side untar.c can depend on the same byte
  // values without anyone editing one and forgetting the other.
  val DirName     = "tx/"
  val FileAName   = "tx/a.txt"
  val FileAData   = "hello\n".getBytes("US-ASCII")     // 6 bytes
  val FileBName   = "tx/b.txt"
  val FileBData   = "world!\n".getBytes("US-ASCII")    // 7 bytes
  val FileMode    = 0x1A4                               // 0o644
  val DirMode     = 0x1ED                               // 0o755

  /** Returns the full byte stream of the test archive. */
  def bytes: Array[Byte] =
    val out = new java.io.ByteArrayOutputStream()
    out.write(headerBlock(DirName,    typeFlag = '5', mode = DirMode,  size = 0))
    out.write(headerBlock(FileAName,  typeFlag = '0', mode = FileMode, size = FileAData.length))
    out.write(padToBlock(FileAData))
    out.write(headerBlock(FileBName,  typeFlag = '0', mode = FileMode, size = FileBData.length))
    out.write(padToBlock(FileBData))
    out.write(new Array[Byte](BLOCK))    // first zero block
    out.write(new Array[Byte](BLOCK))    // second zero block (end marker)
    out.toByteArray

  private def padToBlock(data: Array[Byte]): Array[Byte] =
    val rem = data.length % BLOCK
    if rem == 0 then data
    else
      val padded = new Array[Byte](data.length + (BLOCK - rem))
      System.arraycopy(data, 0, padded, 0, data.length)
      padded

  /** Build a 512-byte USTAR header. The chksum field is filled with
    * spaces during the compute pass, then overwritten with the
    * 6-octal-digit sum + NUL + space per POSIX. Field offsets and
    * widths come straight from `<tar.h>`.
    */
  private def headerBlock(name: String, typeFlag: Char, mode: Int, size: Int): Array[Byte] =
    val h = new Array[Byte](BLOCK)
    writeStr(h,   0, name,                   100)
    writeOct(h, 100, mode.toLong,              7)    // mode (7-digit octal + NUL)
    writeOct(h, 108, 0L,                       7)    // uid
    writeOct(h, 116, 0L,                       7)    // gid
    writeOct(h, 124, size.toLong,             11)    // size (11-digit octal + NUL)
    writeOct(h, 136, 0L,                      11)    // mtime
    // chksum field: fill with spaces during compute
    var i = 148
    while i < 156 do { h(i) = ' '.toByte; i += 1 }
    h(156) = typeFlag.toByte
    // linkname stays zero
    writeStr(h, 257, "ustar",                  6)    // magic + NUL
    h(263) = '0'.toByte                              // version "00"
    h(264) = '0'.toByte
    writeStr(h, 265, "root",                  32)    // uname
    writeStr(h, 297, "root",                  32)    // gname
    // devmajor/devminor stay zero, prefix stays zero
    val sum = h.foldLeft(0)((a, b) => a + (b & 0xff))
    writeOct(h, 148, sum.toLong, 6)                  // chksum (6-digit octal)
    h(154) = 0                                       // NUL after chksum digits
    h(155) = ' '.toByte                              // space terminator (POSIX)
    h

  private def writeStr(h: Array[Byte], off: Int, s: String, max: Int): Unit =
    val src = s.getBytes("US-ASCII")
    val n = math.min(src.length, max - 1)
    System.arraycopy(src, 0, h, off, n)
    // remainder already zero

  private def writeOct(h: Array[Byte], off: Int, v: Long, digits: Int): Unit =
    val s = "%0" + digits + "o"
    val text = s.format(v).getBytes("US-ASCII")
    System.arraycopy(text, 0, h, off, digits)
    h(off + digits) = 0                              // trailing NUL
