#!/bin/sh
# runtest.sh - Run loop lowering tests for cpp
#
# Usage: ./runtest.sh [testname]
#        ./runtest.sh              - run all tests
#        ./runtest.sh while        - run only while.c test
#        ./runtest.sh -g           - regenerate all expected outputs

CPP=../cpp
XDUMP=../xdump
TESTS="while for do nested break continue switch_in_loop dowhile_nested mixed_loops multiline_for spill_while spill_for spill_nested spill_do"

cd "$(dirname "$0")"

# Check tools exist
if [ ! -x "$CPP" ]; then
    echo "Error: cpp not found at $CPP"
    echo "Run 'make' in parent directory first"
    exit 1
fi

if [ ! -x "$XDUMP" ]; then
    echo "Error: xdump not found at $XDUMP"
    echo "Run 'make' in parent directory first"
    exit 1
fi

# Regenerate mode
if [ "$1" = "-g" ]; then
    echo "Regenerating expected outputs..."
    for t in $TESTS; do
        if [ -f "$t.c" ]; then
            echo "  $t.c"
            $CPP "$t.c" 2>/dev/null
            $XDUMP "$t.x" > "$t.expected" 2>/dev/null
            rm -f "$t.x" "$t.i"
        fi
    done
    echo "Done."
    exit 0
fi

# Single test mode
if [ -n "$1" ]; then
    TESTS="$1"
fi

# Run tests
pass=0
fail=0
total=0

for t in $TESTS; do
    if [ ! -f "$t.c" ]; then
        echo "SKIP: $t.c not found"
        continue
    fi

    if [ ! -f "$t.expected" ]; then
        echo "SKIP: $t.expected not found (run with -g to generate)"
        continue
    fi

    total=$((total + 1))

    # Run cpp
    $CPP "$t.c" 2>/dev/null
    if [ $? -ne 0 ]; then
        echo "FAIL: $t - cpp failed"
        fail=$((fail + 1))
        continue
    fi

    # Run xdump and compare
    $XDUMP "$t.x" > "$t.out" 2>/dev/null

    if diff -q "$t.expected" "$t.out" >/dev/null 2>&1; then
        echo "PASS: $t"
        pass=$((pass + 1))
    else
        echo "FAIL: $t - output differs"
        echo "  Expected:"
        head -5 "$t.expected" | sed 's/^/    /'
        echo "  Got:"
        head -5 "$t.out" | sed 's/^/    /'
        fail=$((fail + 1))
    fi

    # Cleanup
    rm -f "$t.x" "$t.i" "$t.out"
done

echo ""
echo "Results: $pass passed, $fail failed, $total total"

if [ $fail -gt 0 ]; then
    exit 1
fi
exit 0
