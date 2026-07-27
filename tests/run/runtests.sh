#!/bin/sh
#
# Run the runtime correctness tests through one toolchain.
#
#   runtests.sh <native|zc3|ccc> [test.c ...]
#
# Each test returns the number of the first check that failed, or zero
# if all passed, so the exit status alone says what went wrong and no
# output has to be parsed or kept in step across the three paths.
#
# native builds with the host compiler and runs directly; zc3 and ccc
# build Z80 binaries and run them under the simulator.  The native run
# is the reference: the tests use short throughout so that it computes
# the same answers a 16-bit target does.

set -e

here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
work="$here/out"

SIM=${SIM:-$root/root/sim}
CCC=$root/root/bin/ccc
TMOUT=${TMOUT:-10}

mode=$1
shift || true

case "$mode" in
native|zc3|ccc) ;;
*) echo "usage: $0 <native|zc3|ccc> [test.c ...]" >&2; exit 2 ;;
esac

tests="$*"
if [ -z "$tests" ]; then
	tests=$(cd "$here" && echo rt_*.c)
fi

mkdir -p "$work"

pass=0
fail=0
skip=0

for t in $tests; do
	base=$(basename "$t" .c)
	src="$here/$base.c"
	bin="$work/$base.$mode"
	log="$work/$base.$mode.log"

	printf '%-10s %-8s ' "$base" "$mode"

	# build
	built=yes
	case "$mode" in
	native)
		# gnu89 for the same reason the rest of the tree uses it:
		# these are K&R declarations, which a modern default rejects
		gcc -w -std=gnu89 -o "$bin" "$src" >"$log" 2>&1 || built=no
		;;
	zc3)
		# -O is what the shipped binaries are built with.  zc3 has no
		# idea where this tree keeps its runtime, so compile only and
		# link by hand, the way the com targets do.
		(cd "$work" &&
		 PATH="$root/root/bin:$PATH" zc3 -O -c -I"$here" "$src" &&
		 "$root/root/bin/wsld" -o "$bin" \
			"$root/root/lib/crt0.o" "$base.o" \
			-L"$root/root/lib" -lc -lu) >"$log" 2>&1 || built=no
		;;
	ccc)
		(cd "$work" && "$CCC" -o "$bin" -I"$here" "$src") >"$log" 2>&1 ||
			built=no
		;;
	esac

	if [ "$built" = no ]; then
		echo "BUILD FAILED  (see $log)"
		fail=$((fail + 1))
		continue
	fi

	# run
	rc=0
	if [ "$mode" = native ]; then
		"$bin" >>"$log" 2>&1 || rc=$?
	else
		if [ ! -x "$SIM" ]; then
			echo "no simulator at $SIM"
			skip=$((skip + 1))
			continue
		fi
		# the simulator resolves its argument through its own root,
		# so hand it a bare name from the directory it starts in.
		# Under a timeout, because a miscompiled loop should be a
		# failure rather than a wedged suite.
		(cd "$work" && timeout "$TMOUT" "$SIM" "$base.$mode") \
			>>"$log" 2>&1 || rc=$?
		[ "$rc" -eq 124 ] && { echo "TIMED OUT"; fail=$((fail + 1));
			continue; }
	fi

	if [ "$rc" -eq 0 ]; then
		echo ok
		pass=$((pass + 1))
	else
		echo "FAILED at check $rc"
		fail=$((fail + 1))
	fi
done

echo "----------------------------------------"
printf '%s: %d ok' "$mode" "$pass"
[ "$fail" -gt 0 ] && printf ', %d failed' "$fail"
[ "$skip" -gt 0 ] && printf ', %d skipped' "$skip"
echo

[ "$fail" -eq 0 ]
