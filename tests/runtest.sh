#!/bin/bash

# Get the directory where this script lives
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# Parent directory is the project root
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Paths to compiler binaries
CC1="$PROJECT_ROOT/root/bin/cc1"
CC2="$PROJECT_ROOT/root/bin/cc2"
ASZ="$PROJECT_ROOT/root/bin/asz"

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
	base="${t%.c}"
	ast="${base}.ast"
	s="${base}.s"
	pp="${base}.pp"

	echo testing against $t
	echo "======= source ========"
	cat "$t"
	echo "======== cc1 ========"
	echo $CC1 -DTEST=$t -I.. $VERBOSE -o $ast $t

	# Run cc1 and capture stderr
	ERRORS=$($CC1 -DTEST=$t -I.. $VERBOSE -o "$ast" "$t" 2>&1 >/dev/null | grep "^file.*error code")

	# Check if there were errors
	if [ -n "$ERRORS" ]; then
		if $expect_fail ; then
			echo "EXPECTED FAILURE - Test correctly failed:"
			echo "$ERRORS"
			# Success: test failed as expected
		else
			echo "ERRORS DETECTED:"
			echo "$ERRORS"
			echo "file $CC1" > .gdbargs
			echo "set args -DTEST=$t -I.. $VERBOSE -o $ast $t" >> .gdbargs
			if $not_k ; then exit 1 ; fi
		fi
	else
		if $expect_fail ; then
			echo "*** UNEXPECTED PASS *** Test was expected to fail but passed!"
			if $not_k ; then exit 1 ; fi
		else
			echo "cc1 ok"
		fi

		# Run cc2 to produce .s
		echo "======== cc2 ========"
		echo $CC2 -o $s $ast
		if $CC2 -o "$s" "$ast" 2>&1; then
			echo "cc2 ok"
		else
			echo "cc2 FAILED"
			if $not_k ; then exit 1 ; fi
		fi

		# Run astpp to produce .pp
		echo "======== astpp ========"
		echo ../astpp $ast
		if ../astpp "$ast" > "$pp" 2>&1; then
			echo "astpp ok"
		else
			echo "astpp FAILED"
			if $not_k ; then exit 1 ; fi
		fi

		# Run asz to produce .o
		echo "======== asz ========"
		echo $ASZ -o ${base}.o $s
		if $ASZ -o "${base}.o" "$s" 2>&1; then
			echo "asz ok"
		else
			echo "asz FAILED"
			if $not_k ; then exit 1 ; fi
		fi
	fi

done
