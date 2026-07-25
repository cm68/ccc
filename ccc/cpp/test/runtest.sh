#!/bin/sh
# runtest.sh - Run cpp filter tests
#
# Usage: ./runtest.sh [testname]
#        ./runtest.sh              - run all tests
#        ./runtest.sh while        - run only while.c test
#        ./runtest.sh -g           - regenerate all expected outputs
#        ./runtest.sh -f           - run only filter stress tests

CPP=../cpp
XDUMP=../xdump

# Loop lowering tests (compare xdump output)
TESTS="while for do nested break continue switch_in_loop dowhile_nested mixed_loops multiline_for spill_while spill_for spill_nested spill_do funcptr"

# Filter stress tests (just check cpp succeeds + validate patterns)
FILTER_TESTS="test_filtknr test_filtdecl test_filtbrace test_filtctrl"

cd "$(dirname "$0")"

# cpp -p forks xdump via PATH
PATH="$(cd .. && pwd):$PATH"
export PATH

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

# Filter-only mode
FILTER_ONLY=0
if [ "$1" = "-f" ]; then
    FILTER_ONLY=1
    TESTS=""
    shift
fi

# Single test mode
if [ -n "$1" ]; then
    TESTS="$1"
    FILTER_TESTS=""
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

    # Check output-language conformance (OUTPUT.md)
    if ! python3 ../validate_x.py "$t.x" >"$t.xerr" 2>&1; then
        echo "FAIL: $t - .x not to spec"
        cat "$t.xerr"
        fail=$((fail + 1))
        rm -f "$t.x" "$t.i" "$t.xerr"
        continue
    fi
    rm -f "$t.xerr"

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

# Run filter stress tests
for t in $FILTER_TESTS; do
    if [ ! -f "$t.c" ]; then
        continue
    fi

    total=$((total + 1))

    # Run cpp (-p generates the .i the pattern checks below read)
    $CPP -DCCC -p "$t.c" -o "$t" 2>"$t.err"
    if [ $? -ne 0 ]; then
        echo "FAIL: $t - cpp failed"
        cat "$t.err"
        fail=$((fail + 1))
        rm -f "$t.err"
        continue
    fi

    # Check output-language conformance (OUTPUT.md)
    if ! python3 ../validate_x.py "$t.x" >>"$t.err" 2>&1; then
        echo "FAIL: $t - .x not to spec"
        cat "$t.err"
        fail=$((fail + 1))
        rm -f "$t.x" "$t.i" "$t.err"
        continue
    fi

    # Check output file exists and has content
    if [ ! -s "$t.i" ]; then
        echo "FAIL: $t - empty output"
        fail=$((fail + 1))
        rm -f "$t.x" "$t.i" "$t.err"
        continue
    fi

    # Validate specific patterns for each filter
    ok=1
    case "$t" in
        test_filtknr)
            # K&R should convert to ANSI params
            grep -q 'int simple (' "$t.i" && grep -q 'int a ,' "$t.i" || ok=0
            ;;
        test_filtdecl)
            # Initializers should be separated
            grep -q 'int x ;' "$t.i" && grep -q 'x = 5 ;' "$t.i" || ok=0
            ;;
        test_filtbrace)
            # Single statements should get braces
            grep -q 'if ( x > 0 ) {' "$t.i" || ok=0
            ;;
        test_filtctrl)
            # Loops should become labels/gotos, long conditions preserved
            grep -q '__W.*T:' "$t.i" && grep -q 'DECR' "$t.i" || ok=0
            ;;
    esac

    if [ $ok -eq 1 ]; then
        echo "PASS: $t"
        pass=$((pass + 1))
    else
        echo "FAIL: $t - validation failed"
        fail=$((fail + 1))
    fi

    # Cleanup
    rm -f "$t.x" "$t.i" "$t.err"
done

echo ""
echo "Results: $pass passed, $fail failed, $total total"

if [ $fail -gt 0 ]; then
    exit 1
fi
exit 0
