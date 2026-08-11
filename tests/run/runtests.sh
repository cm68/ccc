#!/bin/sh
#
# Run the runtime correctness tests through one toolchain.
#
#   runtests.sh <native|ccc> [test.c ...]
#
# Each test returns the number of the first check that failed, or zero
# if all passed, so the exit status alone says what went wrong and no
# output has to be parsed or kept in step across the two paths.
#
# native builds with the host compiler and runs directly; ccc builds a
# Z80 binary and runs it under the simulator.  The native run is the
# reference: the tests use short throughout so that it computes the
# same answers a 16-bit target does.  A third path built with Hi-Tech
# C went with the bootstrap.

set -e

here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
work="$here/out"

SIM=${SIM:-$root/tests/sim}
CCC=$root/desthost/bin/ccc
# Extra flags for the ccc mode.  -O by default, because -O is what
# ships: every library in the tree and every pass built for the target
# is compiled with the peephole on, and a suite that runs without it
# proves the wrong binary correct.  Running optimised code and getting
# the same answers is the only test of the peephole that means
# anything, and it should not need remembering.
#
# CCCFLAGS= turns it off, for telling a codegen bug from a peephole
# one when something here fails.
CCCFLAGS=${CCCFLAGS:--O}
TMOUT=${TMOUT:-10}

mode=$1
shift || true

case "$mode" in
native|ccc) ;;
*) echo "usage: $0 <native|ccc> [test.c ...]" >&2; exit 2 ;;
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
		# these are K&R declarations, which a modern default rejects.
		# -m32 so that long is 32 bits here as it is on the Z80 - on a
		# 64-bit host it is 64, and a long test would be computing
		# different answers in the two places and the reference would
		# be worth nothing.  short and char are unaffected.
		gcc -w -m32 -std=gnu89 -DRT_NATIVE -o "$bin" "$src" >"$log" 2>&1 || built=no
		;;
	ccc)
		(cd "$work" && "$CCC" $CCCFLAGS -DRT_CCC -o "$bin" -I"$here" "$src") >"$log" 2>&1 ||
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
		# stdin from /dev/null: the simulator opens it at startup
		# and fails with a status of its own if the caller has none
		# to give, which reads exactly like a failing check
		(cd "$work" && timeout "$TMOUT" "$SIM" "$base.$mode") \
			>>"$log" 2>&1 </dev/null || rc=$?
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
