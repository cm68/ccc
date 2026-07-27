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
		# these are K&R declarations, which a modern default rejects.
		# -m32 so that long is 32 bits here as it is on the Z80 - on a
		# 64-bit host it is 64, and a long test would be computing
		# different answers in the two places and the reference would
		# be worth nothing.  short and char are unaffected.
		gcc -w -m32 -std=gnu89 -o "$bin" "$src" >"$log" 2>&1 || built=no
		;;
	zc3)
		# The same recipe as ccc/pass1's mx-zc3 target: compile only,
		# then link by hand against this tree's runtime.  What makes
		# the result Micronix rather than CP/M is the link - crt0 and
		# libu - not -CPM, which is about the compiler's own
		# assumptions and is what mx-zc3 passes too.  -Ttext=0x100
		# matters: the simulator loads there, and without it the
		# binary is laid out at zero and runs off into the header.
		(cd "$work" &&
		 PATH="$root/root/bin:$PATH" \
		 zc3 -O -c -CPM -I"$here" "$src" &&
		 "$root/root/bin/wsld" -o "$bin" -Ttext=0x100 \
			"$root/root/lib/crt0.o" "$base.o" \
			-L"$root/root/lib" -lc -lu -lc) >"$log" 2>&1 || built=no
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
