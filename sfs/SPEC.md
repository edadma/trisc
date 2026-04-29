# SFS — Slix Filesystem Design Specification

## Overview

SFS is the primary filesystem for the Slix OS. It is designed to be a modern, robust, journaled filesystem with strong pedagogical value — meaning every feature is real, justified, and teachable. It replaces TFS (Tiny Filesystem), which remains available as the minimal reference implementation.

**Design goals:**
- 2TB maximum capacity
- 2TB maximum file size even under maximum fragmentation
- data=ordered journaling for crash safety (same default as ext4)
- HTree directories for O(log n) lookup
- CRC32 integrity on all critical structures
- Clean on-disk layout that is easy to reason about, implement, and teach

**Explicit non-goals:**
- Copy-on-write (btrfs/ZFS territory — too complex)
- Full data journaling (teaches the tradeoff, not required for correctness)
- Quadruple-indirect blocks (triple-indirect is sufficient)
- Variable block sizes

---

## Constants

| Constant | Value | Notes |
|---|---|---|
| `BLOCK_SIZE` | 4096 bytes | Fixed |
| `TOTAL_BLOCKS` | 536,870,912 (512M, 2²⁹) | 512M × 4KB = 2TB |
| `DEFAULT_INODE_COUNT` | 1,048,576 (1M) | Format-time parameter |
| `INODE_SIZE` | 256 bytes | Fixed |
| `EXTENT_SIZE` | 8 bytes | Fixed |
| `INLINE_EXTENTS` | 16 | Per inode |
| `EXTENTS_PER_INDIRECT` | 512 | Per indirect extent block (4096 / 8) |
| `PTRS_PER_INDIRECT2` | 1,024 | Block addresses per double/triple-indirect block (4096 / 4) |
| `NAME_MAX` | 255 | Max filename length in bytes |
| `PATH_MAX` | 4096 | Max path length; inline symlink threshold is 127 bytes |
| `DEFAULT_JOURNAL_BLOCKS` | 32,768 | 128MB default journal |
| `MAX_BLOCKS_PER_TRANSACTION` | 1,024 | Caps descriptor block count at 2 |

### Inode reservations

| Inode | Purpose |
|---|---|
| 0 | Null / invalid (never allocated) |
| 1 | Bad block tracking file |
| 2 | Root directory |

---

## Disk Layout

All region locations and sizes are stored in the superblock. The superblock is always at block 0. Everything else is described by the superblock, giving flexibility for format-time parameters (inode count, journal size).

```
Block 0:              Superblock
Block 1:              Superblock backup
Blocks 2+:            Block bitmap     (16,384 blocks = 64MB for 512M blocks)
Then:                 Inode bitmap     (32 blocks = 128KB for 1M inodes)
Then:                 Inode table      (65,536 blocks = 256MB for 1M × 256-byte inodes)
Then:                 Journal          (default 32,768 blocks = 128MB)
Then:                 Data region      (all remaining blocks)
```

At format time, all metadata blocks (bitmap, inode table, journal) are marked allocated in the block bitmap. The allocator therefore never needs to know region boundaries — it just checks the bitmap.

---

## Magic Numbers

| Structure | Magic | ASCII |
|---|---|---|
| Superblock | `0x53465300` | `SFS\0` |
| Journal superblock | `0x5346534A` | `SFSJ` |
| Transaction descriptor | `0x53465354` | `SFST` |
| Commit block | `0x53465343` | `SFSC` |
| Directory block tail | `0x53465344` | `SFSD` |

---

## On-Disk Structures

### Superblock (Block 0 and Block 1)

256 bytes of meaningful data, padded to 4096 bytes (rest zeroed).

```
Offset  Size  Field
0       4     magic               (0x53465300 "SFS\0")
4       2     version_major
6       2     version_minor
8       2     block_size          (always 4096; explicit for safety)
10      2     fs_state            (0=clean, 1=dirty, 2=error)
12      4     total_blocks
16      4     free_blocks
20      4     total_inodes        (format-time parameter)
24      4     free_inodes
28      4     block_bitmap_start  (block address)
32      4     block_bitmap_len    (in blocks)
36      4     inode_bitmap_start
40      4     inode_bitmap_len
44      4     inode_table_start
48      4     inode_table_len
52      4     journal_start
56      4     journal_len
60      4     data_start
64      4     root_inode          (always 2)
68      1     hash_algorithm      (0=FNV-1a)
69      3     reserved
72      8     format_time         (Unix seconds)
80      8     last_mount_time
88      8     last_write_time
96      16    uuid
112     16    volume_name         (null-terminated, up to 15 bytes)
128     4     crc32               (over bytes 0–127)
132     124   padding             (zeroed)
```
**Total: 256 bytes**

**fs_state lifecycle:**
- On mount → set to `dirty`, write superblock
- On clean unmount → set to `clean`, write superblock
- On next mount: if `dirty` → replay journal before use; if `error` → require manual fsck
- This is the crash recovery teaching moment in concrete form

**Pedagogical notes:**
- The superblock is the single source of truth. Corruption here is catastrophic — hence the backup at block 1.
- `fs_state` teaches why a "dirty" bit exists and how journaling relates to it.
- `uuid` teaches why volume identity matters (prevents mounting the wrong device).

---

### Inode (256 bytes)

```
Offset  Size  Field
0       2     mode                (Unix-style type + permissions)
2       2     link_count
4       4     uid
8       4     gid
12      4     flags               (see Inode Flags below)
16      8     size                (file size in bytes, 64-bit)
24      4     block_count         (allocated blocks in 512-byte units, POSIX st_blocks)
28      4     generation          (incremented on inode reuse; used by NFS)
32      4     atime_sec
36      4     atime_nsec
40      4     mtime_sec
44      4     mtime_nsec
48      4     ctime_sec           (metadata change time, not creation time)
52      4     ctime_nsec
56      4     crtime_sec          (creation time)
60      4     crtime_nsec
64      128   union:
                extents[16]       (128 bytes, when INLINE_SYMLINK not set)
                symlink_target    (128 bytes = 127 bytes + null, when INLINE_SYMLINK set)
192     4     indirect1           (block address of single-indirect extent block, or 0)
196     4     indirect2           (block address of double-indirect extent block, or 0)
200     4     indirect3           (block address of triple-indirect extent block, or 0)
204     4     xattr_block         (block address of extended attributes block, or 0)
208     4     crc32               (over bytes 0–207)
212     44    reserved            (zeroed)
```
**Total: 256 bytes**

#### Inode Flags

```
bit 0:    INLINE_SYMLINK    — union holds symlink target inline
bit 1:    HAS_XATTR         — xattr_block is valid
bit 2:    HAS_INDIRECT1     — indirect1 is valid
bit 3:    HAS_INDIRECT2     — indirect2 is valid
bit 4:    HAS_INDIRECT3     — indirect3 is valid
bits 5–31: reserved
```

#### block_count units

`block_count` is stored in **512-byte units** (POSIX `st_blocks` convention). A file occupying N 4KB blocks has `block_count = N × 8`. This teaches students why `du` and `ls -l` use different units, and why the 512-byte unit survives even on 4KB-block filesystems.

#### Timestamps

Each timestamp is split into `_sec` (Unix seconds, 4 bytes) and `_nsec` (nanoseconds, 4 bytes). `ctime` is the metadata change time (permissions, owner, link count), not creation time. `crtime` is true creation time — absent from early Unix, present in ext4.

#### Inline symlinks

When `INLINE_SYMLINK` is set, the 128-byte union holds the symlink target directly (up to 127 bytes + null terminator). This avoids allocating a data block for the overwhelming majority of real-world symlinks. Longer symlinks (up to PATH_MAX − 1 = 4095 bytes) use data blocks addressed via the extent mechanism, with `INLINE_SYMLINK` cleared.

**Pedagogical notes:**
- The name/data separation (filenames live in directory entries, not inodes) teaches why hard links work and why rename is cheap.
- `link_count` dropping to zero is what actually frees an inode — not "deleting" a file.
- `generation` teaches forward-compatibility design: fields can be reserved now and defined later.
- The inode union teaches students that on-disk structures are often reinterpreted by type.

---

### Extent (8 bytes)

```
Bit  31:      UNINITIALIZED (flag)
Bit  30:      SPARSE        (flag)
Bit  29:      reserved      (flag)
Bits 28–0:    start_block   (29-bit block address)
Bytes 4–7:    count         (32-bit block count)
```

#### Extent Flags

```
bit 31:   UNINITIALIZED   — extent is preallocated but not yet written (reads as zeros)
bit 30:   SPARSE          — extent is a hole (reads as zeros, no blocks allocated)
bit 29:   reserved
```

`SPARSE` extents enable sparse files: a 10GB file can consume almost no disk space. This explains why `du` and `ls -l` report different sizes — a classic teaching moment.

`UNINITIALIZED` teaches preallocation: space is reserved but not committed, avoiding fragmentation on growing files.

`indirect1`, `indirect2`, and `indirect3` in the inode are plain 32-bit block addresses (no flags), since they point to metadata blocks rather than data extents.

#### File size under maximum fragmentation

With every extent covering exactly 1 block (4KB):

| Tier | Extents | Data |
|---|---|---|
| 16 inline | 16 | 64KB |
| Single-indirect (512 extents) | 512 | 2MB |
| Double-indirect (1,024 × 512 extents) | 524,288 | ~2GB |
| Triple-indirect (1,024 × 1,024 × 512 extents) | 536,870,912 | >2TB (disk-bound) |

Triple-indirect makes worst-case file size **disk-bound at 2TB**, regardless of fragmentation. The 2GB double-indirect ceiling is never reached in practice before triple-indirect takes over.

---

### Indirect Extent Blocks

An indirect extent block is a plain 4KB block containing an array of 512 extents (512 × 8 bytes = 4096 bytes exactly).

- `indirect1` points to one such block, adding up to 512 extents.
- `indirect2` points to a block of 1,024 four-byte block addresses, each pointing to an indirect extent block — adding up to 1,024 × 512 = 524,288 extents.
- `indirect3` points to a block of 1,024 four-byte block addresses, each pointing to a double-indirect block — adding up to 1,024 × 1,024 × 512 = 536,870,912 extents, far exceeding the 2TB disk size and therefore making worst-case file size disk-bound.

The `HAS_INDIRECT1`, `HAS_INDIRECT2`, and `HAS_INDIRECT3` inode flags indicate which tiers are active.

---

## Journal

The journal is a fixed-size circular log stored in the journal region. SFS uses **data=ordered journaling** (the same default as ext4): data blocks are written to their final on-disk locations *before* the journal commit block is written. This guarantees that after a crash, inode extent pointers never point to unwritten or garbage data — worst case, you see the previous version of a file, never corrupted new content. Only metadata changes (inode updates, bitmap updates, directory changes) are journaled; data blocks are not copied into the journal.

**Journaling modes for reference (all teachable):**
- **metadata-only** — fastest; metadata consistent, but new data may be garbage after crash
- **data=ordered** (SFS default) — data written before commit; metadata consistent, data safe
- **data=journal** — data copied into journal first; fully safe but roughly halves write throughput

### Journal Region Layout

```
Block 0 of journal:   Journal superblock
Blocks 1+:            Circular transaction log
```

### Journal Superblock

```
Offset  Size  Field
0       4     magic           (0x5346534A "SFSJ")
4       4     version
8       4     block_count     (total journal blocks excluding this superblock)
12      4     head            (block offset of first valid transaction)
16      4     tail            (block offset of next write position)
20      4     sequence        (monotonically increasing transaction ID)
24      16    fs_uuid         (must match filesystem superblock uuid)
40      4     crc32
44      212   reserved
```
**Total: 256 bytes** (first 256 bytes of block 0; rest zeroed)

### Transaction Descriptor Block

Begins every transaction. Variable-length entries follow the fixed header.

```
Offset  Size  Field
0       4     magic           (0x53465354 "SFST")
4       4     sequence
8       4     block_count     (number of metadata blocks that follow, max 1024)
12      4     flags
16+     8×n   entries[]:
                fs_block  4   (which filesystem block this journal entry shadows)
                flags     4   (ESCAPED=1: block begins with journal magic, XOR'd to avoid false matches)
```

Each descriptor block holds up to 510 entries ((4096 − 16 header bytes) / 8 bytes per entry). For transactions of up to 1,024 blocks, at most 2 descriptor blocks are needed.

### Metadata Blocks

The actual copies of modified metadata blocks, in the same order as the descriptor entries.

### Commit Block

Ends every transaction. A transaction is only valid (and only replayed) if its commit block is present with a matching sequence number and valid CRC.

```
Offset  Size  Field
0       4     magic           (0x53465343 "SFSC")
4       4     sequence
8       8     commit_time     (Unix seconds)
16      4     crc32           (over entire transaction: descriptor + metadata blocks + this block minus crc32 field)
20      236   reserved
```
**Total: 256 bytes** (first 256 bytes of the commit block; rest zeroed)

### Recovery

On mount, if `fs_state == dirty`:
1. Locate journal head (stored in journal superblock)
2. Scan forward: for each transaction descriptor found, check for a matching commit block with valid CRC and sequence number
3. If commit block present and valid → replay: write the journaled metadata blocks to their fs_block locations
4. If commit block absent or invalid → stop; this transaction was incomplete
5. Update journal head past all replayed transactions
6. Set `fs_state = clean`

**Pedagogical notes:**
- Write-ahead logging: the commit block is written last; its presence is the atomicity guarantee.
- The sequence number teaches why monotonic counters matter for distinguishing old from new journal entries in a circular log.
- The three journaling modes (metadata-only, data=ordered, data=journal) form a complete teachable spectrum of safety vs. performance tradeoffs.

---

## Directories — HTree

SFS directories use a hash-tree (HTree) structure: filenames are hashed using FNV-1a, and the hashes are used as keys in a B-tree. This gives O(log n) lookup, scales from single-entry to millions of entries, and teaches both hashing and tree-based indexing together.

All directory blocks carry a 12-byte tail checksum (see below). Usable space per block is therefore **4084 bytes**.

### Hash Algorithm

**FNV-1a** (Fowler–Noll–Vo, 32-bit variant). Chosen for simplicity, speed, and good distribution. The `hash_algorithm` field in the superblock records which hash is in use for forward-compatibility.

### Block Types

#### Root Block (always block 0 of the directory file)

```
Offset  Size  Field
0       12    dot entry       (directory entry for ".")
12      12    dotdot entry    (directory entry for "..")
24      4     reserved
28      1     hash_version    (0 = FNV-1a)
29      1     info_length     (always 8)
30      1     tree_depth      (0 = leaves only, 1 = one index level above leaves)
31      1     flags
32      8×n   index_entries[]:
                hash    4     (minimum hash value in child subtree)
                block   4     (block number of child: index block or leaf block)
...
4084    (end of usable space)
4084    12    tail checksum
```

The dot and dotdot entries use the standard leaf directory entry format.

#### Index Blocks (internal tree nodes, present when tree_depth > 0)

```
Offset  Size  Field
0       8×n   index_entries[]:
                hash    4
                block   4     (block number of child: index or leaf)
...     (up to 510 entries in 4084 bytes)
4084    12    tail checksum
```

#### Leaf Blocks (actual directory entries)

Variable-length records packed from the start of the block.

```
Repeated until block full:
  Offset  Size  Field
  0       4     inode_number
  4       2     rec_len           (total length of this record including name and padding)
  6       1     name_len          (actual name length in bytes, max 255)
  7       1     file_type         (0=unknown, 1=regular, 2=directory, 3=symlink, 4=other)
  8       n     name              (name_len bytes, NOT null-terminated)
  8+n     p     padding           (to next 4-byte boundary)

4084            (end of usable space)
4084    12      tail checksum
```

`rec_len` may be larger than `8 + name_len + padding` — the excess space belongs to a deleted entry. This tombstoning technique avoids compacting the block on deletion and is why fsck must understand rec_len to reconstruct valid entry lists.

### Directory Block Tail Checksum (all block types)

The last 12 bytes of every directory block:

```
Offset from end   Size  Field
-12               4     magic       (0x53465344 "SFSD")
-8                4     inode_num   (owning directory inode number)
-4                4     crc32       (over entire block excluding this crc32 field)
```

Including `inode_num` in the checksum prevents block-swap attacks: a valid block from a different directory will fail this check.

**Pedagogical notes:**
- HTree teaches why linear directory scan fails at scale and how you replace it without redesigning the filesystem.
- The hash/tree combination in one structure illustrates both concepts together.
- `file_type` in directory entries is redundant with inode `mode`, but avoids reading the inode just to know if an entry is a subdirectory — a concrete cache-efficiency lesson.
- The `rec_len` tombstone teaches why deletion and compaction are separate concerns.
- Block-swap protection via `inode_num` in checksums teaches defense-in-depth.

---

## Integrity Model

| Structure | Protection |
|---|---|
| Superblock | CRC32 over bytes 0–127 |
| Superblock backup | Identical copy at block 1 |
| Inodes | CRC32 over bytes 0–207 |
| Journal transactions | CRC32 over entire transaction in commit block |
| Directory blocks | CRC32 + magic + inode_num in 12-byte tail |
| Data blocks | No per-block checksum (journaling covers write-time corruption; bit-rot detection left as a future extension) |

**Pedagogical note:** CRC32 detects silent corruption but does not correct it. This teaches the distinction between error detection and error correction, and motivates why ZFS uses per-block checksums everywhere.

---

## Known Design Tradeoffs

These are intentional limitations worth documenting for teaching purposes.

| Tradeoff | Decision | Real-world comparison |
|---|---|---|
| 1M default inodes on 2TB | ~1 inode per 2MB; inode exhaustion is a real failure mode separate from disk-full | ext4 defaults to ~1 per 16KB |
| data=ordered journaling | Data written before metadata commit; data written after last fsync before crash may be lost, but never corrupted | ext4 default mode |
| No copy-on-write | Simpler, but no atomic snapshots | btrfs, ZFS |
| No B-tree extent index | Flat indirect blocks; simpler to understand | ext4 extent tree |
| No data block checksums | Bit-rot not detected in data | ZFS, btrfs |
| Fixed block size | Always 4KB; no tuning | ext4 allows 1KB–4KB |

---

## SFS vs ext4

SFS in data=ordered mode is in the same reliability tier as ext4 in its default configuration. Differences are mostly intentional omissions for simplicity, not weaknesses.

| Property | SFS | ext4 |
|---|---|---|
| Block size | Fixed 4KB | 1KB–4KB |
| Max volume size | 2TB | 1EB |
| Max file size | 2TB | 16TB |
| Journaling default | data=ordered | data=ordered |
| Extent structure | Flat inline + indirect tiers | B-tree extent tree |
| Directory indexing | HTree (FNV-1a) | HTree (half-MD4 or TEA hash) |
| Inline data | Inline symlinks only | Inline symlinks + tiny file data in inode |
| Metadata checksums | Superblock, inodes, directory blocks | Every metadata block |
| Copy-on-write | No | No (that's btrfs) |
| Delayed allocation | Not specified (allocator strategy, no format change needed) | Yes |
| Preallocation | UNINITIALIZED extent flag | fallocate / unwritten extents |
| Sparse files | SPARSE extent flag | Hole punching via fallocate |
| Extended attributes | xattr_block pointer reserved | Full xattr support |
| Timestamps | Nanosecond, includes crtime | Nanosecond, includes crtime |
| fsck required | Journal replay only | Journal replay only |
| Designed for | Teaching + real Slix use | Production Linux |

**Honest gaps:**
- Volume and file size ceilings are lower, but irrelevant for Slix
- ext4 checksums every metadata block; SFS covers superblock, inodes, and directory blocks only
- Delayed allocation improves extent contiguity significantly — a future allocator improvement that requires no format changes
- xattr format not yet defined

**Where SFS is cleaner than ext4:**
- Extent format is simpler and easier to reason about than ext4's extent tree
- On-disk layout fully described by superblock with no hidden assumptions
- No legacy cruft accumulated over 30 years of backward compatibility

---

## Summary

| Property | Value |
|---|---|
| Block size | 4,096 bytes |
| Maximum volume size | 2TB (512M blocks) |
| Maximum file size (contiguous) | 2TB |
| Maximum file size (maximum fragmentation) | 2TB (disk-bound; triple-indirect removes extent ceiling) |
| Maximum filename length | 255 bytes |
| Maximum path length | 4,096 bytes |
| Inline symlink threshold | 127 bytes |
| Default inode count | 1,048,576 |
| Inode size | 256 bytes |
| Inline extents per inode | 16 |
| Maximum extents per file | >536M (triple-indirect; disk-bound in practice) |
| Default journal size | 128MB |
| Directory lookup | O(log n) via HTree / FNV-1a |
| Journaling mode | data=ordered (data written before commit; same default as ext4) |
| Integrity | CRC32 on superblock, inodes, journal transactions, directory blocks |
