#!/bin/bash
#
# Run valgrind memory leak detection on compiler tests
#

# Get the directory where this script lives
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# Parent directory is the project root
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Paths to compiler binaries
CC1="$PROJECT_ROOT/root/bin/cc1"

PASS=0
FAIL=0
LEAK=0
TOTAL=0

# Change to tests directory
cd "$SCRIPT_DIR" || exit 1

# Parse command line arguments
if [ $# -ne 0 ] ; then
	# Strip tests/ prefix if present
	TESTS=()
	for test in "$@"; do
		TESTS+=("${test#tests/}")
	done
else
	# Use all .c files in current directory
	TESTS=(*.c)
fi

echo "Running valgrind on ${#TESTS[@]} tests..."
echo "========================================"
echo

# Run valgrind on each test
for t in ${TESTS[@]}; do
	TOTAL=$((TOTAL + 1))
	printf "%-45s" "$t: "

	# Run valgrind with leak check
	# - leak-check=full: detailed leak information
	# - show-leak-kinds=definite: only report definite leaks (not reachable blocks)
	# - errors-for-leak-kinds=definite: exit with error code if definite leaks found
	# - error-exitcode=1: exit with code 1 on errors
	if valgrind --leak-check=full \
		--show-leak-kinds=definite \
		--errors-for-leak-kinds=definite \
		--error-exitcode=1 \
		--log-file=/tmp/valgrind_$$.log \
		$CC1 -E "$t" > /dev/null 2>&1; then
		echo "PASS"
		PASS=$((PASS + 1))
	else
		# Check if it was a leak or crash
		if grep -q "definitely lost:" /tmp/valgrind_$$.log; then
			LOST=$(grep "definitely lost:" /tmp/valgrind_$$.log | awk '{print $4, $5, $6, $7}')
			echo "LEAK: $LOST"
			LEAK=$((LEAK + 1))
		else
			echo "FAIL (crash/error)"
			FAIL=$((FAIL + 1))
		fi
	fi

	# Clean up log file
	rm -f /tmp/valgrind_$$.log
done

echo
echo "========================================"
echo "Results:"
echo "  PASS (no leaks): $PASS"
echo "  LEAK:            $LEAK"
echo "  FAIL:            $FAIL"
echo "  TOTAL:           $TOTAL"
echo

# Exit with error if any tests failed or leaked
if [ $FAIL -ne 0 ] || [ $LEAK -ne 0 ]; then
	exit 1
fi

exit 0
