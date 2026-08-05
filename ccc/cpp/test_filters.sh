#!/bin/bash
#
# test_filters.sh - Test cpp filter chain on all compiler sources
#
# Runs cpp on every source file and validates the .i output
#

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
CPP="$SCRIPT_DIR/cpp"
VALIDATOR="$SCRIPT_DIR/validate_i.py"
OUTDIR="/tmp/cpp_test_$$"

# Include paths
INC="-I$SCRIPT_DIR/../../libsrc/include"
LIBINC="-I$SCRIPT_DIR/../lib"

mkdir -p "$OUTDIR"

PASS=0
FAIL=0
FAILED_FILES=""

run_test() {
    local src="$1"
    local name="$(basename "$src" .c)"
    local dir="$(basename "$(dirname "$src")")"
    local outbase="$OUTDIR/${dir}_${name}"
    local defines="$2"

    printf "%-20s " "$dir/$name:"

    if "$CPP" $defines $INC $LIBINC -p -o "$outbase" "$src" 2>"$outbase.err"; then
        if python3 "$VALIDATOR" "$outbase.i" 2>"$outbase.val"; then
            echo "OK"
            PASS=$((PASS + 1))
        else
            echo "FAIL (validate)"
            cat "$outbase.val"
            FAIL=$((FAIL + 1))
            FAILED_FILES="$FAILED_FILES $dir/$name"
        fi
    else
        echo "FAIL (cpp)"
        cat "$outbase.err"
        FAIL=$((FAIL + 1))
        FAILED_FILES="$FAILED_FILES $dir/$name"
    fi
}

echo "Testing cpp filter chain..."
echo "Output dir: $OUTDIR"
echo

# pass1 sources
echo "=== pass1 ==="
for f in "$SCRIPT_DIR/../pass1"/*.c; do
    [ -f "$f" ] || continue
    run_test "$f" "-DCCC -I$SCRIPT_DIR/../pass1"
done

echo
echo "=== pass2 ==="
for f in "$SCRIPT_DIR/../pass2"/*.c; do
    [ -f "$f" ] || continue
    run_test "$f" "-DCCC -I$SCRIPT_DIR/../pass2"
done

echo
echo "=== cpp ==="
for f in "$SCRIPT_DIR"/*.c; do
    [ -f "$f" ] || continue
    # Skip test files
    case "$(basename "$f")" in
        test*.c|mkkw.c|filttest.c|xdump.c) continue ;;
    esac
    run_test "$f" "-I$SCRIPT_DIR"
done

echo
echo "========================================"
echo "Results: $PASS passed, $FAIL failed"
if [ $FAIL -gt 0 ]; then
    echo "Failed files:$FAILED_FILES"
    echo
    echo "Output files in: $OUTDIR"
    exit 1
else
    rm -rf "$OUTDIR"
    exit 0
fi
