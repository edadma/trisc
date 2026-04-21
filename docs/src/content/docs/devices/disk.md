---
title: Ramdisk & HardDisk
description: Block storage devices for the TRISC emulator.
---

TRISC provides two block storage devices with identical register layouts: a RAM-backed ramdisk and a host-file-backed hard disk.

## Register map

**Ramdisk base:** `0x800140` | **Size:** 16 bytes | **IRQ:** 3

| Offset | Size | Name | Access | Description |
|--------|------|------|--------|-------------|
| +0 | 4B | LBA | R/W | Logical block address (sector number) |
| +4 | 4B | ADDR | R/W | DMA target address in RAM |
| +8 | 4B | CAPACITY | R | Total number of sectors |
| +12 | 2B | COUNT | R/W | Number of sectors to transfer |
| +14 | 1B | STATUS | R | Status: bit 0 = ready, bit 1 = error |
| +15 | 1B | COMMAND | W | Command: 1 = read, 2 = write |

## Operation

1. Write the starting sector number to **LBA**
2. Write the RAM destination/source address to **ADDR**
3. Write the sector count to **COUNT**
4. Write the command to **COMMAND** (1 = read sectors into RAM, 2 = write sectors from RAM)
5. The device transfers data via DMA and raises an IRQ when complete

## Ramdisk

The ramdisk stores data in a RAM array within the emulator. It supports prepopulated content — the emulator can format a TFS filesystem and inject files at startup:

```scala
val ramdisk = new Ramdisk(
  Runtime.ramdiskAddress, ram,
  sectors = 256, sectorSize = 4096,
  intc, irq = 3,
  prefill = """
    /dev/tty0 char 0 0
    /etc/passwd file "root:x:0:0:root:/root:/nsh"
  """,
  files = Map("nsh" -> nshBinary, "cat" -> catBinary),
)
```

## HardDisk

The hard disk has the same register interface but is backed by a host file via `RandomAccessFile`. This provides persistent storage across emulator runs.

```scala
val disk = new HardDisk(baseAddr, ram, "/path/to/disk.img", intc, irq = 3)
```

## Sector sizes

Both devices support configurable sector sizes (default 4096 bytes to match TFS page size). The CAPACITY register reports the total number of sectors.
