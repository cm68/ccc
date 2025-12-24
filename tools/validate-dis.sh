#!/bin/sh
# Validate that disassembly round-trips correctly
# Usage: ./validate-dis.sh file.o
#
# Compares the text section of the original object with the reassembled one

if [ -z "$1" ]; then
    echo "Usage: $0 file.o" >&2
    exit 1
fi

orig="$1"
base=$(basename "$orig" .o)
dir=$(dirname "$orig")

# Create temp files
tmpdir="/tmp/validate-$$"
mkdir -p "$tmpdir"
trap "rm -rf '$tmpdir'" EXIT

# Step 1: Disassemble original
./wsnm -d "$orig" > "$tmpdir/$base.dis" 2>&1
if [ ! -s "$tmpdir/$base.dis" ]; then
    echo "FAIL: Could not disassemble $orig" >&2
    exit 1
fi

# Step 2: Convert to assemblable source
./dis2asm.sh "$tmpdir/$base.dis" > "$tmpdir/$base.s"
if [ ! -s "$tmpdir/$base.s" ]; then
    echo "FAIL: Could not convert disassembly to source" >&2
    exit 1
fi

# Step 3: Assemble with -8 flag (no jp->jr relaxation for 8080 objects)
./asz -8 "$tmpdir/$base.s" -o "$tmpdir/$base.new.o" 2>&1
# asz writes to .o based on input filename
if [ ! -f "$tmpdir/$base.o" ]; then
    echo "FAIL: Assembly failed" >&2
    exit 1
fi

# Step 4: Extract text sections and compare
# Get text size from header (bytes 4-5 little-endian, offset from byte 4)
get_text() {
    # Read header to get text size and offset
    # Magic=1, Config=1, Symlen=1, Symtab=2, Text=2, Data=2, BSS=2, Heap=2, TextOff=2, DataOff=2
    # Text size at offset 5-6 (bytes 6-7, 0-indexed 5-6)
    od -A n -t u2 -j 5 -N 2 "$1" | tr -d ' '
}

# Extract text bytes using wsnm
orig_text=$(./wsnm -d "$orig" 2>/dev/null | awk '
    /^Disassembly:/ { in_dis=1; next }
    /^Data segment:/ { in_dis=0 }
    in_dis && /^  [0-9a-f][0-9a-f][0-9a-f][0-9a-f]  / {
        # Extract hex bytes from columns 9-20
        hex = substr($0, 9, 12)
        gsub(/[ \t]+/, "", hex)
        printf "%s", hex
    }
')

new_text=$(./wsnm -d "$tmpdir/$base.o" 2>/dev/null | awk '
    /^Disassembly:/ { in_dis=1; next }
    /^Data segment:/ { in_dis=0 }
    in_dis && /^  [0-9a-f][0-9a-f][0-9a-f][0-9a-f]  / {
        # Extract hex bytes from columns 9-20
        hex = substr($0, 9, 12)
        gsub(/[ \t]+/, "", hex)
        printf "%s", hex
    }
')

if [ "$orig_text" = "$new_text" ]; then
    echo "PASS: $orig - text sections match"
    exit 0
else
    echo "FAIL: $orig - text sections differ" >&2
    echo "Original: $orig_text" >&2
    echo "New:      $new_text" >&2
    exit 1
fi
