#!/bin/sh
#
# abdiff.sh - A/B harness for the filter-unification campaign.
#
# The gate for every phase of the cpp normalizer rewrite: the .x
# stream (and the .s that pass1/pass2 generate from it) for every
# source in the tree must not change unless the change is understood
# and blessed.
#
#   ./abdiff.sh save    - run stage1 everywhere, snapshot outputs
#   ./abdiff.sh check   - run stage1 everywhere, diff against snapshot
#
# The snapshot lives in <treetop>/xbase, untracked.

DIRS="cpp pass1 pass2 peep"
LIBDIRS="src/libc src/libu src/libcpm"
TOP=$(cd "$(dirname "$0")/../.." && pwd)
BASE=$TOP/xbase

run_stage1() {
	for d in $DIRS; do
		make -C "$TOP/ccc/$d" stage1 > /dev/null 2>&1 || {
			echo "stage1 FAILED in $d"; exit 1; }
	done
}

# The library sources go through cpp too - getargs.c held the K&R
# struct-param shape none of the compiler sources use.  Preprocess
# each into $1/<area>/, skipping files cpp cannot take standalone.
run_libs() {
	for d in $LIBDIRS; do
		a=$(basename "$d")
		mkdir -p "$1/$a"
		for f in "$TOP/$d"/*.c; do
			b=$(basename "$f" .c)
			(cd "$TOP/$d" && "$TOP/src/cpp/cpp" -DCCC \
				-i"$TOP/src/include" -I. \
				-o "$1/$a/$b" "$b.c") > /dev/null 2>&1 || \
				rm -f "$1/$a/$b".*
		done
	done
}

case "$1" in
save)
	run_stage1
	rm -rf "$BASE"
	for d in $DIRS; do
		mkdir -p "$BASE/$d"
		cp "$TOP/ccc/$d/stage1"/*.x "$TOP/ccc/$d/stage1"/*.n \
		   "$TOP/ccc/$d/stage1"/*.s "$BASE/$d/" 2>/dev/null
	done
	run_libs "$BASE"
	echo "baseline saved: $(ls "$BASE"/*/*.x | wc -l) .x files"
	;;
check)
	[ -d "$BASE" ] || { echo "no baseline - run save first"; exit 1; }
	run_stage1
	rm -rf "$BASE.now"
	run_libs "$BASE.now"
	bad=0; n=0
	for d in $DIRS; do
		for f in "$BASE/$d"/*; do
			b=$(basename "$f")
			n=$((n + 1))
			if ! cmp -s "$f" "$TOP/ccc/$d/stage1/$b"; then
				echo "DIFF: $d/$b"
				bad=$((bad + 1))
			fi
		done
	done
	for d in $LIBDIRS; do
		a=$(basename "$d")
		[ -d "$BASE/$a" ] || continue
		for f in "$BASE/$a"/*; do
			b=$(basename "$f")
			n=$((n + 1))
			if ! cmp -s "$f" "$BASE.now/$a/$b"; then
				echo "DIFF: $a/$b"
				bad=$((bad + 1))
			fi
		done
	done
	if [ "$bad" -eq 0 ]; then
		echo "OK: $n files byte-identical"
	else
		echo "$bad of $n files differ"
		exit 1
	fi
	;;
*)
	echo "usage: abdiff.sh save|check"; exit 1
	;;
esac
