#!/bin/bash
cd $(dirname $0)

if [ $# -gt 0 ] ; then
	TESTS=($*)
else
	TESTS=(*.c)
fi

for t in ${TESTS[*]} ; do
	echo testing against $t
	echo "======= source ========"
	cat $t
        echo "======== run ========"
	../lextest -v 0 -E $t
	echo ""
	echo "========= object ========="
	cat $(echo ${t%.c}.i)
	echo ""
done
