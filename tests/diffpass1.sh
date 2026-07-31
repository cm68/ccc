#!/bin/sh
#
# Differential test: c0 compiled by zc3 against c0 compiled by ccc.
#
# The same question diffcpp.sh asks, one pass further along.  Both are
# Z80 binaries, both run under the simulator on the same lexeme
# streams, and both outputs are byte compared.  c0 writes two files -
# the AST in .1 and the assembly it emits itself in .2 - and a
# difference in either is a difference.
#
# The inputs are the tree's own sources, preprocessed once by the host
# cpp so that both runs see identical bytes.  What is being compared is
# what c0 computes, not what cpp did.
#
# Usage: sh tests/diffpass1.sh [name ...]

set -e
root=$(cd "$(dirname "$0")/.." && pwd)
p1=$root/ccc/pass1
work=$(mktemp -d /tmp/diffp1.XXXXXX)

# pass1's own sources, then cpp's - the second set matters because it
# is written in a different style and takes shapes pass1 does not.
P1SRCS="pass1 error lexread expr parse decl type declare outast regalloc util"
CPPSRCS="cpp lex io macro kw emit util kwtab lexdata filtenum filtknr
         filtdecl filtbrace filtctrl filtutil typetab"

TMO=${DIFFP1_TIMEOUT:-180}

zc3mx=$p1/mxzc3/c0.mx
cccmx=$p1/mxccc/c0.mx
for f in "$zc3mx" "$cccmx"; do
	[ -f "$f" ] || { echo "missing $f"; echo "build both first:"; \
	    echo "  make -C ccc/pass1 mx-zc3"; \
	    echo "  and the ccc build, see the top of this script"; exit 1; }
done

echo "c0 built by zc3: $(wc -c < "$zc3mx") bytes"
echo "c0 built by ccc: $(wc -c < "$cccmx") bytes"
echo

printf '%-12s %8s %8s  %s\n' file zc3 ccc result
same=0; diff=0; err=0

run() {
	dir=$1; base=$2; src=$3
	# One .x, shared by both runs: the comparison is of c0, so cpp
	# must not be a variable in it.  -k leaves the intermediates in
	# the working directory under the source's own basename, so the
	# run happens there and the .x is renamed after.
	(cd "$work" && "$root/root/bin/ccc" -k -c -DCCC \
	    -i"$root/libsrc/include" -I"$dir" -I"$root/ccc/lib" \
	    -I"$root/libsrc/include" "$src" >/dev/null 2>&1) || return 1
	[ -s "$work/$b.x" ] || return 1
	mv "$work/$b.x" "$work/$base.x"
	rm -f "$work/$b.1" "$work/$b.2" "$work/$b.s" "$work/$b.o"

	for who in zc3 ccc; do
		mx=$zc3mx; [ "$who" = ccc ] && mx=$cccmx
		rc=0
		timeout "$TMO" "$root/root/sim" -d / "$mx" \
		    "$work/$base.x" "$work/$base.$who.1" "$work/$base.$who.2" \
		    </dev/null >/dev/null 2>&1 || rc=$?
		if [ "$rc" -eq 124 ]; then
			printf '%-12s %8s %8s  HUNG: %s\n' "$base" - - "$who"
			return 2
		fi
	done
	return 0
}

for set in p1 cpp; do
	case $set in
	p1)  dir=$p1; list=$P1SRCS;;
	cpp) dir=$root/ccc/cpp; list=$CPPSRCS;;
	esac
	for b in $list; do
		name="$set/$b"
		if ! run "$dir" "$set-$b" "$dir/$b.c"; then
			printf '%-12s %8s %8s  NO OUTPUT\n' "$name" - -
			err=$((err + 1)); continue
		fi
		f=$work/$set-$b
		bad=
		for ext in 1 2; do
			[ -s "$f.zc3.$ext" ] || bad="$bad .$ext-empty"
			cmp -s "$f.zc3.$ext" "$f.ccc.$ext" || bad="$bad .$ext"
		done
		zs=$(wc -c < "$f.zc3.1")
		cs=$(wc -c < "$f.ccc.1")
		if [ -z "$bad" ]; then
			printf '%-12s %8d %8d  same\n' "$name" "$zs" "$cs"
			same=$((same + 1))
		else
			printf '%-12s %8d %8d  differ:%s\n' "$name" "$zs" "$cs" "$bad"
			diff=$((diff + 1))
		fi
	done
done

echo
echo "----------------------------------------"
echo "$same same, $diff differ, $err no output"
echo "work kept: $work"
[ "$diff" -eq 0 ] && [ "$err" -eq 0 ]
