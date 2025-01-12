#!/bin/bash

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

if [ $# -ne 0 ] ; then
	TESTS=($*)
else
	TESTS=(tests/*.c)
fi

for t in ${TESTS[*]} ; do
	echo testing against $t
	echo "======= source ========"
	cat $t
        echo "======== run ========"
	echo ./cc1 $VERBOSE -E $t
	if ! ./cc1 -DTEST=$t -I. $VERBOSE -E $t ; then
		echo "file ./cc1" > .gdbargs
		echo "set args -DTEST=$t -I. $VERBOSE -E $t" >> .gdbargs
		echo "exited code $?"
		exit
	fi
	echo "========= object ========="
	cat $(echo ${t%.c}.i)
	echo ""
done
