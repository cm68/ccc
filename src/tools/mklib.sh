#!/bin/sh
# Process library directories
# For each .o file:
#   1. Rename .o to .O (original)
#   2. Create .s from .dis using dis2asm
#   3. Create a Makefile

if [ -z "$1" ]; then
    echo "Usage: $0 <libdir>" >&2
    exit 1
fi

LIBDIR="$1"
WSDIR="$(dirname "$0")"

cd "$LIBDIR" || exit 1

# Collect object names
objs=""
for o in *.o; do
    [ -f "$o" ] || continue
    base="${o%.o}"
    objs="$objs $base.o"

    # Rename .o to .O if not already done
    if [ ! -f "$base.O" ]; then
        mv "$o" "$base.O"
    fi

    # Create .s from .dis
    if [ -f "$base.dis" ]; then
        "$WSDIR/dis2asm.sh" "$base.dis" > "$base.s"
    fi
done

# Create Makefile
cat > Makefile << 'EOF'
ASZ = ../../asz
AFLAGS = -8

SRCS = $(wildcard *.s)
OBJS = $(SRCS:.s=.o)

all: $(OBJS)

%.o: %.s
	$(ASZ) $(AFLAGS) $<

clean:
	rm -f $(OBJS)

.PHONY: all clean
EOF

echo "Created Makefile in $LIBDIR"
echo "Objects: $objs"
