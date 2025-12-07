#!/bin/bash

# Get the directory where this script lives
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# Parent directory is the project root
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

VERBOSE=""
not_k=true
expect_fail=false
while getopts hkfv: flag; do
	case $flag in
	h)
		echo -v verbosity
		echo -k continue after failure
		echo -f expect failure
		exit
		;;
	k)
		not_k=false
		;;
	f)
		expect_fail=true
		;;
	v)
		VERBOSE="-v $OPTARG"
		;;
	*)
		echo "unknown option $OPTARG"
		exit
		;;
	esac
done
shift $((OPTIND-1))

# Change to tests directory
cd "$SCRIPT_DIR" || exit 1

if [ $# -ne 0 ] ; then
	# Strip tests/ prefix if present
	TESTS=()
	for test in "$@"; do
		TESTS+=("${test#tests/}")
	done
else
	TESTS=(*.c)
fi

for t in "${TESTS[@]}" ; do
	echo testing against $t
	echo "======= source ========"
	cat "$t"
	echo "======== run ========"
	echo ../cc1 $VERBOSE -E $t

	# Run cc1 and capture stderr
	ERRORS=$(../cc1 -DTEST=$t -I.. $VERBOSE -E "$t" 2>&1 >/dev/null | grep "^file.*error code")
	EXIT_CODE=$?

	# Check if there were errors
	if [ -n "$ERRORS" ]; then
		if $expect_fail ; then
			echo "EXPECTED FAILURE - Test correctly failed:"
			echo "$ERRORS"
			# Success: test failed as expected
		else
			echo "ERRORS DETECTED:"
			echo "$ERRORS"
			echo "file ../cc1" > .gdbargs
			echo "set args -DTEST=$t -I.. $VERBOSE -E $t" >> .gdbargs
			if $not_k ; then exit 1 ; fi
		fi
	else
		if $expect_fail ; then
			echo "*** UNEXPECTED PASS *** Test was expected to fail but passed!"
			if $not_k ; then exit 1 ; fi
		else
			echo "exited code 0"
		fi
	fi

done
