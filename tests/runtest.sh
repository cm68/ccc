#!/bin/bash

# Get the directory where this script lives
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# Parent directory is the project root
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

VERBOSE=""

while getopts :v:h flag; do
	case $flag in
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
	if ! ../cc1 -DTEST=$t -I.. $VERBOSE -E "$t" ; then
		echo "file ../cc1" > .gdbargs
		echo "set args -DTEST=$t -I.. $VERBOSE -E $t" >> .gdbargs
		echo "exited code $?"
		exit
	fi
	echo "========= object ========="
	cat "${t%.c}.i"
	echo ""
done
