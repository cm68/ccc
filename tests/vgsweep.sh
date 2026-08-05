#!/bin/sh
#
# Valgrind sweep for uninitialised values across the tree's own sources.
#
# Leaks are not what this is for.  These programs read a file, write a
# file and exit, so what they lose to the allocator costs nothing - but
# a field that is read before it is written changes what the compiler
# emits, and does it differently depending on what was compiled before.
# One of those (Expr.nored, never set by alloc()) silently blocked a
# reduction and left a marker in one file while the same function
# compiled clean on its own.
#
# Runs c0 and c1 over the stage1 intermediates, which have to exist:
#	make -C ccc stage1 && make -C tools stage1
#
# Usage: sh tests/vgsweep.sh [-l]      -l also reports definite leaks

set -e
root=$(cd "$(dirname "$0")/.." && pwd)
leaks=no
[ "$1" = "-l" ] && leaks=yes

VG="valgrind --error-exitcode=9 --track-origins=yes"
if [ "$leaks" = yes ]; then
	VG="$VG --leak-check=full --show-leak-kinds=definite --errors-for-leak-kinds=definite"
else
	VG="$VG --leak-check=no"
fi

log=$(mktemp)
bad=0
n=0

for d in ccc/cpp ccc/pass1 ccc/pass2 tools; do
	[ -d "$root/$d/stage1" ] || continue
	for x in "$root/$d"/stage1/*.x; do
		[ -f "$x" ] || continue
		b=${x%.x}
		n=$((n + 1))

		# c0: .x -> .1 .2
		if ! $VG --log-file="$log" "$root/ccc/pass1/c0" \
		    "$x" /tmp/vg$$.1 /tmp/vg$$.2 >/dev/null 2>&1; then
			if grep -q 'uninitialised\|Invalid read\|Invalid write' "$log"; then
				echo "c0 $(basename "$b"):"
				grep -A3 'uninitialised value\|Invalid read\|Invalid write' "$log" |
				    grep '   at \|   by ' | head -3
				bad=$((bad + 1))
			fi
		fi

		# c1: .1 .2 -> .s
		if [ -f "$b.1" ] && [ -f "$b.2" ]; then
			if ! $VG --log-file="$log" "$root/ccc/pass2/c1" \
			    "$b.1" "$b.2" /tmp/vg$$.s >/dev/null 2>&1; then
				if grep -q 'uninitialised\|Invalid read\|Invalid write' "$log"; then
					echo "c1 $(basename "$b"):"
					grep -A3 'uninitialised value\|Invalid read\|Invalid write' "$log" |
					    grep '   at \|   by ' | head -3
					bad=$((bad + 1))
				fi
			fi
		fi
	done
done

rm -f "$log" /tmp/vg$$.1 /tmp/vg$$.2 /tmp/vg$$.s
echo "----------------------------------------"
echo "$n files, $bad with uninitialised or invalid access"
[ "$bad" -eq 0 ]
