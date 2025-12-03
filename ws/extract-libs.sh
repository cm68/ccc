#!/bin/sh
# Extract Whitesmith's archives to lib/ subdirectories and disassemble
# Usage: ./extract-libs.sh

set -e

cd "$(dirname "$0")"

# Create lib directory
mkdir -p lib

# Process each archive
for ar in tests/lib*.a; do
    name=$(basename "$ar" .a)
    echo "=== Extracting $name ==="

    # Create subdirectory
    mkdir -p "lib/$name"

    # Extract objects
    (cd "lib/$name" && ../../wslib -xv "../../tests/$name.a")

    # Disassemble each object
    echo "--- Disassembling $name ---"
    for obj in "lib/$name"/*.o; do
        if [ -f "$obj" ]; then
            base=$(basename "$obj" .o)
            ./wsnm -d "$obj" > "lib/$name/$base.dis" 2>&1 || true
        fi
    done

    echo ""
done

echo "Done. Archives extracted to lib/*"
echo "Disassembly files: lib/*/*.dis"
