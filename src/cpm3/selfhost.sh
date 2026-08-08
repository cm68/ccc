#!/bin/sh
#
# Compile the compiler with itself, on CP/M.
#
# Every source of cpp, pass1, pass2, peep and ccclib goes through the
# three passes twice: once with the host compiler, and once with the
# CP/M .com images running on the cpm3 machine.  The outputs are
# compared.
#
# Nothing is copied.  The tree's directories are handed to the machine
# as drives and the compiler reads them where they sit:
#
#	A:  the pass being compiled, and where its output goes
#	B:  src/include	 the target's headers
#	C:  src/ccclib	 libutil.h, format.h
#	D:  src/cpp	 lexeme.h, the token numbers everyone reads
#
# which is what a CP/M machine has instead of directories, and is
# enough - the passes are not all distinct in 8.3 (expr.c, util.c,
# io.c, rules.c and debug.h each exist in two or three of them), so
# flattening them into one place would have collided.
#
# Both sides are run on the same bare names: cpp records the name it
# was given, so a path on one side and a bare name on the other would
# differ for that reason alone.  The host gets the same four
# directories as -I paths.
#
# c1's assembly is compared with the host's "; stmt" commentary
# stripped - the host build defines DEBUG and the CP/M one does not.
#
# usage: selfhost.sh [-k] [pass ...]        (pass: cpp pass1 pass2 peep ccclib)
#	-k	keep the work directories
#

# A pass that has not finished in this many seconds is reported as a
# timeout rather than left to hold the run up.  The machine is a
# cycle-stepped emulator: a second of it is a good fraction of a
# second of Z80, so a source that takes minutes is saying something.
TMO=${TMO:-180}

here=$(cd "$(dirname "$0")" && pwd)
top=$(cd "$here/../.." && pwd)
sim=$here/cpm3
keep=0

if [ "$1" = "-k" ]; then keep=1; shift; fi

for f in "$sim" "$top/cpm/bin/cpp.com" "$top/cpm/bin/c0.com" \
	 "$top/cpm/bin/c1.com"; do
	[ -f "$f" ] || { echo "missing $f - make && make cpm first" >&2; exit 2; }
done

passes="$*"
[ -z "$passes" ] && passes="ccclib cpp pass1 pass2 peep"

root=$(mktemp -d "${TMPDIR:-/tmp}/cpm3host.XXXXXX")
if [ $keep = 0 ]; then
	trap 'rm -rf "$root"' EXIT
else
	echo "work: $root"
fi

ok=0; bad=0; slow=0
printf '%-8s %-12s %7s %6s  %s\n' pass source bytes secs result
echo "-------------------------------------------------------"

for p in $passes; do
	# A: is a scratch directory holding the pass's sources, because
	# the outputs land on it too and the tree is not the place for
	# them.  The headers are read where they live.
	work=$root/$p
	mkdir -p "$work"
	cp "$top/src/$p"/*.c "$top/src/$p"/*.h "$work"/ 2>/dev/null

	drives="-d $work -d B=$top/src/include -d C=$top/src/ccclib \
		-d D=$top/src/cpp"

	# Both sides are given the same include path, spelled the same
	# way, because cpp records the name it opened and a comparison
	# of the lexeme streams would otherwise be measuring the
	# spelling.  A colon is a legal character in a Unix filename,
	# so the host reaches "B:stdarg.h" through a link of that name
	# while the machine reaches it through drive B.
	for h in "$top"/src/include/*.h; do
		ln -sf "$h" "$work/B:$(basename "$h")"
	done
	for h in "$top"/src/ccclib/*.h; do
		ln -sf "$h" "$work/C:$(basename "$h")"
	done
	ln -sf "$top/src/cpp/lexeme.h" "$work/D:lexeme.h"
	for h in "$work"/*.h; do
		case "$h" in *:*) continue ;; esac
		ln -sf "$h" "$work/A:$(basename "$h")"
	done

	# -IA: and not -I.: there is no "." on CP/M, and a path that is
	# not a drive gets a '/' put after it, which the runtime then
	# tries to read as part of the filename.  The pass's own
	# directory is drive A, so that is what it is called.
	inc="-iB: -IA: -IC: -ID:"

	for src in "$top/src/$p"/*.c; do
		b=$(basename "$src" .c)
		# mkkw and test are build-time helpers; dbgtags is a
		# fragment the pass #includes, not a source of its own.
		case "$b" in mkkw|test|dbgtags) continue ;; esac

		sz=$(wc -c < "$src")
		start=$(date +%s)

		( cd "$work" && rm -f h.x h.n h.1 h.2 h.s g.x g.n g.1 g.2 g.s )

		if ! ( cd "$work" &&
		       "$top"/unix/bin/cpp -DCCC $inc -o h "$b.c" &&
		       "$top"/unix/bin/c0 h.x h.1 h.2 &&
		       "$top"/unix/bin/c1 h.1 h.2 h.s ) >/dev/null 2>&1; then
			printf '%-8s %-12s %7s %6s  %s\n' \
				"$p" "$b" "$sz" - "skip (host)"
			continue
		fi

		why=""
		timeout $TMO $sim $drives "$top/cpm/bin/cpp.com" -DCCC $inc \
			-o g "$b.c" >/dev/null 2>&1 || why="cpp"
		[ -z "$why" ] && { timeout $TMO $sim $drives \
			"$top/cpm/bin/c0.com" g.x g.1 g.2 >/dev/null 2>&1 || why="c0"; }
		[ -z "$why" ] && { timeout $TMO $sim $drives \
			"$top/cpm/bin/c1.com" g.1 g.2 g.s >/dev/null 2>&1 || why="c1"; }

		secs=$(( $(date +%s) - start ))

		if [ -n "$why" ]; then
			printf '%-8s %-12s %7s %6s  %s\n' \
				"$p" "$b" "$sz" "$secs" "FAIL ($why)"
			bad=$((bad + 1))
			continue
		fi

		cmp -s "$work/g.x" "$work/h.x" || why="$why .x"
		cmp -s "$work/g.1" "$work/h.1" || why="$why .1"
		cmp -s "$work/g.2" "$work/h.2" || why="$why .2"
		grep -v '^;' "$work/h.s" > "$work/h.stripped" 2>/dev/null
		cmp -s "$work/g.s" "$work/h.stripped" || why="$why .s"

		if [ -n "$why" ]; then
			printf '%-8s %-12s %7s %6s  %s\n' \
				"$p" "$b" "$sz" "$secs" "DIFFERS$why"
			bad=$((bad + 1))
		else
			printf '%-8s %-12s %7s %6s  %s\n' \
				"$p" "$b" "$sz" "$secs" ok
			ok=$((ok + 1))
			[ "$secs" -gt 30 ] && slow=$((slow + 1))
		fi
	done
done

echo "-------------------------------------------------------"
echo "$ok agree with the host, $bad do not"
[ $bad = 0 ]

# vim: tabstop=8 shiftwidth=8 noexpandtab:
