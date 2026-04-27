# SFS — Implementation Plan

This is the long-term plan for implementing SFS as specified in `SPEC.md`.
The full spec lands in 17 phases. Each phase is small enough to be one
focused work session with its own tests. The phasing is chosen so that:

- every phase ends with the test suite green;
- later phases build on earlier ones without retrofits (e.g. the journal
  is wired in *before* dir/file ops mutate metadata, so we never have to
  go back and re-route writes through a journal);
- on-disk formats are correct from the start — no provisional layouts
  that get rewritten.

No simplifications. The full triple-indirect tree, full HTree (with
splits), full metadata journal, xattrs, and bad-block tracking all land.

---

## Phase 0 — Foundation (DONE)

- `BlockDevice` trait, `RamBlockDevice`
- `Constants` matching the spec
- ScalaTest skeleton in `sfs/src/test/scala/...`

## Phase 1 — Codec primitives

`Le` — little-endian read/write helpers (`u8`/`u16`/`u32`/`u64`,
signed and unsigned, into/out of `Array[Byte]` at offsets) plus copy
helpers for `bytes` and `zero`.

`Crc32` — CRC-32 with the IEEE polynomial (reflected, 0xEDB88320),
table-driven. This is what ext4 uses for its metadata checksums; all
SFS CRCs go through it.

Tests: byte-level round-trip; CRC32 matches known vectors (e.g. CRC32
of "123456789" = 0xCBF43926).

## Phase 2 — On-disk struct codecs

One file per structure, each a pure pack/unpack with no I/O. Every
field documented against `SPEC.md`. CRC fields are computed inside the
encoder and verified inside the decoder.

- `Superblock` — 256-byte payload, CRC over bytes 0–127, padded to 4 KiB.
- `Inode` — 256 bytes, CRC over bytes 0–207, with a typed view over
  the 128-byte union for inline extents *or* inline symlink target.
- `Extent` — 8-byte packed (`UNINITIALIZED`, `SPARSE`, 29-bit start,
  32-bit count).
- `IndirectExtentBlock` — array of 512 extents.
- `IndirectPointerBlock` — array of 1 024 u32 block addresses (used by
  indirect2 and indirect3).
- `DirEntry` — variable-length leaf entry (`inode`, `rec_len`,
  `name_len`, `file_type`, `name`, padding).
- `DirRootBlock` — dot + dotdot + 4-byte reserved + 4-byte HTree
  header (`hash_version`, `info_length`, `tree_depth`, `flags`) +
  index entries.
- `DirIndexBlock` — index entries only (interior nodes when
  `tree_depth > 0`).
- `DirLeafBlock` — leaf-entry helpers shared by root and index trees.
- `DirTail` — last 12 bytes (`magic`, `inode_num`, `crc32`).
- `JournalSuperblock` — 256-byte payload, CRC.
- `TxnDescriptor` — magic + sequence + block_count + flags + entries.
- `CommitBlock` — magic + sequence + commit_time + crc32.

Tests: round-trip every struct; verify CRC; verify magic; verify
field offsets match the spec by writing a known buffer and checking
specific byte positions.

## Phase 3 — Bitmap allocators

`Bitmap` — abstract allocator over a contiguous run of bitmap blocks
on a `BlockDevice`. In-memory cache of the bitmap region for speed;
dirty blocks flushed back through the device. Operations:

- `isSet(bit) / set(bit) / clear(bit)`
- `allocate()` — first-fit linear scan
- `allocate(hint)` — start scan near a hint (locality)
- `allocateRange(n)` — find a contiguous run of `n` free bits
- `free(bit) / freeRange(start, n)`
- `freeCount` — track free-bit count incrementally for fast statfs

`BlockAllocator` and `InodeAllocator` are thin wrappers giving the
right region bounds.

Tests: full enumerate + cross-check against a brute-force `Set[Int]`
oracle; contiguous-range allocation; free-count consistency; flushing.

## Phase 4 — Layout + format

`Layout` — given a device size and `FormatOptions(inodes,
journalBlocks, …)`, computes region sizes:

```
bitmap_blocks  = ceil(total_blocks  / (8 * BlockSize))
inode_bitmap   = ceil(total_inodes  / (8 * BlockSize))
inode_table    = ceil(total_inodes  * InodeSize / BlockSize)
journal        = opts.journalBlocks
data_start     = 2 + block_bitmap + inode_bitmap + inode_table + journal
```

with a sanity check that `data_start < total_blocks`.

`Sfs.format(dev, opts)` —
1. Zero every metadata block (clean slate).
2. Initialize block bitmap with bits 0…(`data_start` − 1) set.
3. Initialize inode bitmap with bits 0, 1, 2 set (null, bad-blocks, root).
4. Write inode 1 (bad blocks file: regular file, size 0, no extents).
5. Write inode 2 (root directory) and allocate + lay down its
   directory blocks (root + one leaf, both with valid tail).
6. Write the journal superblock (empty, sequence = 0, head = tail = 0).
7. Write the filesystem superblock at block 0 and its backup at block 1
   with `fs_state = clean`.

Tests: format a 2 MiB device, then read every metadata block and
verify it parses, CRCs check, layout fields are consistent.

## Phase 5 — Mount / unmount

`Sfs` class —

- `Sfs.mount(dev)`: read superblock; on CRC failure read backup;
  reject if `fs_state == error`; if `dirty`, run journal recovery
  (Phase 13 — until then, refuse to mount dirty); set `fs_state = dirty`
  and write superblock back; load bitmaps.
- `unmount()`: flush bitmaps + any cached blocks; set
  `fs_state = clean`; write superblock; release `dev`.
- `readInode(ino) / writeInode(ino, inode)` — locate the inode in the
  table, decode/encode with CRC.

Until journaling lands, `writeInode` and bitmap flushes are direct
device writes. They will be moved behind the journal in Phase 13
without changing their callers' API.

Tests: format → mount → readInode(2) → unmount → re-mount round-trip.

## Phase 6 — Extent traversal (read path)

`ExtentReader(inode)` — given a logical block index, return the
backing physical block (or `Sparse` / `Uninitialized` markers).
Walks inline extents first, then `indirect1`, `indirect2`, `indirect3`.

Tests with hand-crafted inodes spanning all three indirect tiers.

## Phase 7 — Extent allocation (write path)

`ExtentAllocator(inode, blockAllocator)` —

- Append blocks for a write past EOF, coalescing with the last extent
  when contiguous.
- Sparse holes — writes past EOF with no fill leave `SPARSE` extents.
- Spill across tiers automatically: inline → indirect1 → indirect2 →
  indirect3, allocating indirect blocks as needed.
- Free blocks on truncate, releasing indirect blocks when their
  populations drop to zero.

Tests: fill an inode through all four tiers; truncate back; verify
bitmap counts at each step.

## Phase 8 — File I/O

- `readFile(ino, offset, len)` — uses `ExtentReader`; fills sparse
  holes with zeros.
- `writeFile(ino, offset, bytes)` — uses `ExtentAllocator`; updates
  inode `size`, `block_count`, `mtime`, `ctime`.
- `truncate(ino, newSize)` — extend with sparse or shrink with frees.
- Boundary cases: byte-misaligned offsets, partial first/last blocks,
  zero-length, exactly EOF.

Tests including a > 64 KiB file (forces indirect1) and a contrived
> 2 GiB synthetic file (forces indirect2/3 — built sparsely so it
fits in the test device).

## Phase 9 — HTree directories

This is the largest phase. Sub-chunks:

9a. **Leaf operations on a single block**: parse, scan-skipping-tombstones,
    insert with first-fit + tombstone splitting, delete by tombstone
    in place.

9b. **Tail checksum maintenance** on every leaf/index/root write.

9c. **Single-leaf HTree** at `tree_depth = 0`: root block holds dot,
    dotdot, header, and one index entry pointing to one leaf. Lookup
    descends straight through.

9d. **Leaf split**: when an insert would overflow a leaf, split it
    by hash midpoint; reflow entries; insert a new index entry in
    the parent (root, while `tree_depth = 0`).

9e. **Tree-depth promotion**: when the root's index-entry table fills,
    promote `tree_depth` to 1 — convert root index entries into
    interior index blocks. Lookup walks two levels.

9f. **Index splits at depth 1**: when an interior block fills, split
    it; if root index-entry table fills again, error
    (`tree_depth = 2` not in spec).

Tests: insert 1 / 100 / 10 000 / 100 000 entries; verify all are
findable and listable; delete every other entry and reinsert; make
sure tombstone reuse and splits don't desync.

## Phase 10 — File ops

- `create(parent, name, mode)` → new inode, dir entry inserted.
- `unlink(parent, name)` — decrement target inode `link_count`;
  free inode + its blocks when count drops to zero and refcount is 0.
- `link(target, parent, name)` — bump target.link_count; insert dir
  entry; update target.ctime.
- `stat(ino)` — return full inode.
- POSIX permission check (`mode` bits, uid/gid).

Tests including hard-link cycles (intentionally rejected for dirs)
and link-count edge cases.

## Phase 11 — Directory ops

- `mkdir(parent, name, mode)` — alloc inode, init root + leaf dir
  blocks with dot/dotdot, bump parent.link_count for the new "..".
- `rmdir(parent, name)` — must be empty (only dot/dotdot); free dir
  inode and blocks; decrement parent.link_count.
- `readdir(ino)` — walk all leaves through the HTree, yield
  `(name, ino, file_type)`.
- `rename(oldParent, oldName, newParent, newName)` — under the
  journal once Phase 13 lands; for now best-effort.
- Cross-directory rename, overwrite-existing, type checks.

Tests: deep tree of subdirs; rename across dirs; overwrite.

## Phase 12 — Symlinks

- `symlink(target, parent, name)`: if `target.length ≤ 127`, set
  `INLINE_SYMLINK` and store the target in the union; else allocate
  data blocks and store via the extent path with the flag cleared.
  Maximum length is `PATH_MAX − 1 = 4095`.
- `readlink(ino)` → `String`.
- Loop detection in path resolution (deferred to a path-walker layer
  on top of these ops).

Tests: 1 byte, 127 bytes, 128 bytes (forces extent), 4 095 bytes.

## Phase 13 — Journal

13a. **JournalSuperblock + circular log bookkeeping**: head/tail/seq;
     wrap-around helpers; reservation accounting (don't overwrite
     unreplayed transactions).

13b. **Transaction API**:

```scala
val tx = sfs.beginTxn()
tx.writeMetadata(blockNum, buf)   // shadowed write
tx.commit()                       // emits descriptor + entries + commit
```

Entries: descriptor block, metadata blocks (with magic-collision
escape via XOR sentinel), commit block — all written in order.
Pre-commit, the device is told to flush before the commit block goes
down (so torn-write recovery is well-defined).

13c. **Wire metadata writes through the journal**: every inode write,
     bitmap write, and directory-block write is replaced by a
     `tx.writeMetadata`. Data writes stay direct (metadata-only
     journaling per spec). All callers of the old direct-write
     functions get re-routed; the old direct-write functions become
     internal helpers used by the journal alone.

13d. **Recovery**: on dirty mount, scan from journal head, replay every
     valid transaction (matching sequence, valid commit CRC). Update
     head past replayed transactions. Then bring the fs up clean.

13e. **Crash-injection tests**: a `CrashingBlockDevice` decorator that
     drops writes after a configurable barrier point; verify recovery
     reaches a consistent state.

## Phase 14 — Xattrs

The spec reserves `xattr_block` but does not specify its layout.
Phase 14 starts by *defining* it (a header + array of name/value
records, with CRC + magic). Then:

- `getxattr(ino, name)`, `setxattr(ino, name, value)`,
  `listxattr(ino)`, `removexattr(ino, name)`.
- Free the xattr block when no attributes remain.

Tests: round-trip; multiple attributes; deletion.

## Phase 15 — Bad-block tracking

Inode 1 is a regular file whose contents are an array of u32 block
addresses to skip. The block allocator consults this list at startup
(loads it into a `Set[Int]`). Adding to the bad-block list rewrites
the file.

Tests: mark a few blocks bad; allocator skips them.

## Phase 16 — fsck

- Verify every CRC: superblock, inodes, dir blocks, indirect blocks,
  journal entries.
- Re-derive bitmaps from inode reachability and compare with on-disk
  bitmaps.
- Re-derive `link_count` from directory-entry counts.
- Find allocated-but-unreferenced inodes → `lost+found`.
- Optionally repair (off by default; report only).

Tests: deliberate corruption; verify diagnostics.

## Phase 17 — Polish

- atime updating with `relatime`-style semantics.
- Permissions enforcement on every op.
- Statfs (`free_blocks`, `free_inodes`).
- Volume label / UUID utilities.
- Non-empty fs growth (resize) — possibly deferred again.

---

## Per-phase invariants

After each phase:

1. `sbt sfsJVM/test` is green.
2. No public API contract is broken (later phases extend, never break).
3. The on-disk format matches `SPEC.md` exactly — every byte at every
   offset accounted for, no provisional layouts.

---

## Style notes

- Every public method gets a Scaladoc that explains *why*, not what.
- Encoders write into a caller-supplied `Array[Byte]` slice; decoders
  read from one. The codec layer never touches `BlockDevice` directly,
  so it stays trivially testable.
- All sizes and offsets come from `Constants`. No magic numbers
  scattered through the implementation.
