#!/usr/bin/env python3
"""Convert a flat binary to TRB v1 format for SLIX loader."""
import struct, sys

if len(sys.argv) != 4:
    print(f"Usage: {sys.argv[0]} <flat.bin> <entry_addr> <output.trb>", file=sys.stderr)
    sys.exit(1)

data = open(sys.argv[1], 'rb').read()
entry = int(sys.argv[2], 0)
org = entry  # load address = entry point (first instruction)

trb = b'TRB\x01'
trb += struct.pack('<I', entry)      # entry point
trb += struct.pack('<I', 1)          # 1 record
trb += struct.pack('<I', org)        # org (load address)
trb += struct.pack('<I', 0)          # kind = PROGBITS
trb += struct.pack('<I', len(data))  # size
trb += data

open(sys.argv[3], 'wb').write(trb)
print(f"  {sys.argv[3]}: {len(data)} bytes code at 0x{org:X}, entry 0x{entry:X}", file=sys.stderr)
