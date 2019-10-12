#!/bin/bash

VERBOSE=0

while getopts :v:h flag; do
	case $flag in
	v)
		VERBOSE=$OPTARG
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
	echo ./cc1 -v $VERBOSE -E $t
	./cc1 -DTEST=$t -I. -v $VERBOSE -E $t
	echo ""
	echo "========= object ========="
	cat $(echo ${t%.c}.i)
	echo ""
done
