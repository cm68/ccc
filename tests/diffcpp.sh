#!/bin/sh
#
# Differential test: cpp compiled by zc3 against cpp compiled by ccc.
#
# Both are Z80 binaries and both are run under the simulator on the
# same inputs - cpp's own sources - and their lexeme output is byte
# compared.  What this asks is not "does ccc compile cpp" but "does
# the cpp that ccc built compute what the cpp that zc3 built computes",
# which is a stronger question and the one a self-hosting compiler has
# to answer.
#
# zc3 is the reference only in the sense that it is the other
# implementation.  Where they differ, either could be the wrong one,
# and this session has already found three places where zc3 was.
#
# Usage: sh tests/diffcpp.sh

set -e
root=$(cd "$(dirname "$0")/.." && pwd)
cpp=$root/ccc/cpp
work=$(mktemp -d /tmp/diffcpp.XXXXXX)

SRCS="cpp lex io macro kw emit util kwtab lexdata filtenum filtknr
      filtdecl filtbrace filtctrl filtutil typetab"

# -i matters: the simulator maps / to the host root, so without it
# cpp reaches the host's /usr/include instead of the tree's.
INC="-i$root/libsrc/include -I$cpp -I$root/ccc/lib -I$root/libsrc/include"

echo "building cpp with ccc"
mkdir -p "$cpp/comccc"
for b in $SRCS; do
	(cd "$cpp/comccc" && PATH="$root/root/bin:$PATH" \
	    "$root/root/bin/ccc" -DCCC $INC -c "$cpp/$b.c" >"$b.log" 2>&1) || {
		echo "  $b: FAILED TO COMPILE"; cat "$cpp/comccc/$b.log"; exit 1; }
done

# Link both with the same recipe; each image takes the runtime built
# by its own compiler from the matching root/lib area.
link() {
	out=$1; dir=$2; libs=$3
	objs=""
	for b in $SRCS; do objs="$objs $dir/$b.o"; done
	"$root/root/bin/wsld" -s -o "$out" -Ttext=0x100 \
	    "$root/root/lib/crt0.o" $objs \
	    -L"$root/root/lib/$libs" -lccc -lc -lu -lc
}
link "$work/cpp-zc3.mx" "$cpp/comzc3" zc3
link "$work/cpp-ccc.mx" "$cpp/comccc" ccc

echo "running both over $(echo $SRCS | wc -w) sources"
echo
printf '%-12s %8s %8s  %s\n' file zc3 ccc result
same=0; diff=0; err=0

# A wrong answer takes as long as a right one; an infinite loop does
# not.  Without this the first hang blocked the whole sweep, and a run
# that had made no progress in two hours looked the same from outside
# as one that was working.  cpp's own sources take a second or two.
TMO=${DIFFCPP_TIMEOUT:-120}

for b in $SRCS; do
	hung=
	for who in zc3 ccc; do
		rc=0
		timeout "$TMO" "$root/root/sim" -d / "$work/cpp-$who.mx" \
		    -DCCC $INC -o "$work/$b.$who" "$cpp/$b.c" \
		    >/dev/null 2>&1 || rc=$?
		if [ "$rc" -eq 124 ]; then
			hung="$hung $who"
		fi
	done
	if [ -n "$hung" ]; then
		printf '%-12s %8s %8s  HUNG:%s\n' "$b" - - "$hung"
		err=$((err + 1))
		continue
	fi

	if [ ! -s "$work/$b.zc3.x" ] || [ ! -s "$work/$b.ccc.x" ]; then
		printf '%-12s %8s %8s  %s\n' "$b" - - "NO OUTPUT"
		err=$((err + 1))
		continue
	fi

	zs=$(wc -c < "$work/$b.zc3.x")
	cs=$(wc -c < "$work/$b.ccc.x")
	if cmp -s "$work/$b.zc3.x" "$work/$b.ccc.x"; then
		printf '%-12s %8d %8d  same\n' "$b" "$zs" "$cs"
		same=$((same + 1))
	else
		off=$(cmp "$work/$b.zc3.x" "$work/$b.ccc.x" 2>&1 | sed 's/.*byte //;s/,.*//')
		# Both sizes, always.  Reporting only where they first differ
		# read as a near miss when ccc was emitting a file header and
		# stopping - the offset moved about as bugs were fixed and
		# looked like progress inside the stream.
		printf '%-12s %8d %8d  differ at %s\n' "$b" "$zs" "$cs" "$off"
		diff=$((diff + 1))
	fi
done

echo
echo "----------------------------------------"
echo "$same same, $diff differ, $err no output"
echo "work kept: $work"
[ "$diff" -eq 0 ] && [ "$err" -eq 0 ]
