#!/bin/bash

# Get the directory where this script lives
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# Parent directory is the project root
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Path to compiler driver
CCC="$PROJECT_ROOT/root/bin/ccc"

VERBOSE=""
not_k=true
expect_fail=false
do_link=false
do_run=false
compile_only=false
while getopts hkflrsv:V: flag; do
	case $flag in
	h)
		echo -v verbosity for cc1
		echo -V verbosity for cc2
		echo -k continue after failure
		echo -f expect failure
		echo -s compile only, no assembly
		echo -l link the .o file
		echo -r run under simulation \(implies -l\)
		exit
		;;
	l)
		do_link=true
		;;
	r)
		do_link=true
		do_run=true
		;;
	s)
		compile_only=true
		;;
	k)
		not_k=false
		;;
	f)
		expect_fail=true
		;;
	v)
		VERBOSE="$VERBOSE -v $OPTARG"
		;;
	V)
		VERBOSE="$VERBOSE -V $OPTARG"
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

	echo testing against $t
	echo "======= source ========"
	cat "$t"
	echo "======== ccc ========"
	CCC_OPTS="-DTEST=$t -I.. $VERBOSE -P -k -c"
	if $compile_only ; then CCC_OPTS="$CCC_OPTS -s" ; fi
	echo $CCC $CCC_OPTS $t

	# Run ccc with -k -c to compile and keep intermediates
	if $CCC $CCC_OPTS "$t" 2>&1; then
		if $expect_fail ; then
			echo "*** UNEXPECTED PASS *** Test was expected to fail but passed!"
			if $not_k ; then exit 1 ; fi
		else
			echo "ccc ok"
			if $do_link ; then
				echo "======== link ========"
				echo $CCC -o "$base" "$base.o"
				if $CCC -o "$base" "$base.o" 2>&1; then
					echo "link ok"
					if $do_run ; then
						echo "======== run ========"
						"$PROJECT_ROOT/root/sim" "./$base"
						echo "exit code: $?"
					fi
				else
					echo "link FAILED"
					if $not_k ; then exit 1 ; fi
				fi
			fi
		fi
	else
		if $expect_fail ; then
			echo "EXPECTED FAILURE - Test correctly failed"
		else
			echo "ccc FAILED"
			if $not_k ; then exit 1 ; fi
		fi
	fi

done
