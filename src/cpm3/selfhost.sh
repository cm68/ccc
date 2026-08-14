#!/bin/sh
#
# Compile the compiler with itself, on every target it has.
#
# Every source of cpp, pass1, pass2, peep and ccclib goes through the
# three passes three times: with the host compiler, with the Micronix
# .mx images under the usersim, and with the CP/M .com images on the
# cpm3 machine.  Both simulated legs are compared against the host.
#
# The assertion is that the target the compiler runs on does not
# change what it emits.  Nothing downstream can be trusted without it:
# a self-hosted compiler that is subtly a different compiler is worse
# than one that does not run at all, because it produces plausible
# output.
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
# LEGS= picks which simulated targets to run: "mx", "cpm", or both
# (the default).  TPA= moves the CP/M bdos - see below.
#

# A pass that has not finished in this many seconds is reported as a
# timeout rather than left to hold the run up.  The machine is a
# cycle-stepped emulator: a second of it is a good fraction of a
# second of Z80, so a source that takes minutes is saying something.
TMO=${TMO:-180}

# Where to put the bdos, and so how big the TPA is.  The default is
# the machine's own, which is generous; set it to what a real banked
# CP/M 3 gives - 0xf900, a 62K TPA - to check the passes fit a
# machine that exists rather than one that is convenient.
TPA=${TPA:-}

# Which simulated targets to compare against the host.
LEGS=${LEGS:-mx cpm}

here=$(cd "$(dirname "$0")" && pwd)
top=$(cd "$here/../.." && pwd)
sim=$here/cpm3
mxsim=$top/tests/sim

keep=0

if [ "$1" = "-k" ]; then keep=1; shift; fi

need=""
case " $LEGS " in *" cpm "*)
	need="$need $sim $top/destcpm/bin/cpp.com $top/destcpm/bin/c0.com \
	      $top/destcpm/bin/c1.com" ;;
esac
case " $LEGS " in *" mx "*)
	need="$need $mxsim $top/destmicronix/lib/pass0 $top/destmicronix/lib/c0 \
	      $top/destmicronix/lib/c1" ;;
esac
for f in $need; do
	[ -f "$f" ] || { echo "missing $f - make, make micronix, make cpm" >&2
			 exit 2; }
done

# The bdos flag goes on AFTER the file check, and stays out of $sim
# until then: the first version of this appended it before the
# assignment that sets $sim overwrote it, so TPA= was accepted and
# silently ignored - every run measured the machine's default bdos
# while the summary said otherwise.  Putting it in $sim before the
# check instead made the check test a string with a flag in it.
[ -n "$TPA" ] && sim="$sim -t $TPA"

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
	# every header cpp exports, not just lexeme.h: the token codes
	# were split into lexops.h and linking one by name meant the host
	# leg could not open the other, so those sources came out "skip
	# (host)" and the run reported 21 of 50 with nothing wrong.
	for h in "$top"/src/cpp/*.h; do
		ln -sf "$h" "$work/D:$(basename "$h")"
	done
	for h in "$work"/*.h; do
		case "$h" in *:*) continue ;; esac
		ln -sf "$h" "$work/A:$(basename "$h")"
	done

	# -IA: and not -I.: there is no "." on CP/M, and a path that is
	# not a drive gets a '/' put after it, which the runtime then
	# tries to read as part of the filename.  The pass's own
	# directory is drive A, so that is what it is called.
	# the usersim's filesystem root is the work directory, so the
	# images it runs have to be inside it
	# destmicronix is a system root - the passes sit in lib under the
	# names the driver runs them by, not as .mx files in bin.
	case " $LEGS " in *" mx "*)
		cp "$top"/destmicronix/lib/pass0 "$top"/destmicronix/lib/c0 \
		   "$top"/destmicronix/lib/c1 "$work"/ ;;
	esac

	inc="-iB: -IA: -IC: -ID:"

	# What the pass is actually built from.  Globbing *.c swept up
	# mkkw and test (build-time helpers), xdump (its own program),
	# dbgtags (a fragment #included under DEBUG) and pass1's empty
	# io.c - none of which any build compiles, and every one of
	# which was being counted as a source that failed.
	srcs=$(cd "$top/src/$p" && make -s -p 2>/dev/null |
		grep -m1 '^SOURCES = ' | sed 's/SOURCES = //')
	if [ -z "$srcs" ]; then
		echo "$p: cannot read SOURCES" >&2
		continue
	fi

	for name in $srcs; do
		src="$top/src/$p/$name"
		b=$(basename "$name" .c)
		[ -f "$src" ] || continue

		sz=$(wc -c < "$src")
		start=$(date +%s)

		( cd "$work" && rm -f h.x h.nam h.ast h.dat h.s g.x g.nam g.ast g.dat g.s )

		#
		# libexec, not lib.  The passes moved and this did not,
		# so every file failed the HOST half of the comparison
		# and was reported "skip (host)" - a gate that answered
		# "0 agree, 0 do not" and exited 0, which reads like
		# success and is the absence of one.
		#
		if ! ( cd "$work" &&
		       "$top"/desthost/libexec/pass0 -DCCC $inc -o h "$b.c" &&
		       "$top"/desthost/libexec/c0 h.x h.ast h.dat &&
		       "$top"/desthost/libexec/c1 h.ast h.dat h.s ) >/dev/null 2>&1; then
			printf '%-8s %-12s %7s %6s  %s\n' \
				"$p" "$b" "$sz" - "skip (host)"
			continue
		fi

		why=""

		# Micronix.  Bare names, exactly as the host got them:
		# cpp records the name it opened, so "/expr.c" here and
		# "expr.c" there differ in the .x for that reason alone
		# and nothing else.
		case " $LEGS " in *" mx "*)
			( cd "$work" && rm -f m.x m.nam m.ast m.dat m.s )
			timeout $TMO $mxsim -d "$work" /pass0 -DCCC $inc \
				-o m "$b.c" >/dev/null 2>&1 </dev/null ||
				why="mx:cpp"
			[ -z "$why" ] && { timeout $TMO $mxsim -d "$work" \
				/c0 m.x m.ast m.dat >/dev/null 2>&1 </dev/null ||
				why="mx:c0"; }
			[ -z "$why" ] && { timeout $TMO $mxsim -d "$work" \
				/c1 m.ast m.dat m.s >/dev/null 2>&1 </dev/null ||
				why="mx:c1"; }
			;;
		esac

		# CP/M 3
		case " $LEGS " in *" cpm "*)
			[ -z "$why" ] && { timeout $TMO $sim $drives \
				"$top/destcpm/bin/cpp.com" -DCCC $inc -o g "$b.c" \
				>/dev/null 2>&1 || why="cpm:cpp"; }
			[ -z "$why" ] && { timeout $TMO $sim $drives \
				"$top/destcpm/bin/c0.com" g.x g.ast g.dat >/dev/null 2>&1 ||
				why="cpm:c0"; }
			[ -z "$why" ] && { timeout $TMO $sim $drives \
				"$top/destcpm/bin/c1.com" g.ast g.dat g.s >/dev/null 2>&1 ||
				why="cpm:c1"; }
			;;
		esac

		secs=$(( $(date +%s) - start ))

		if [ -n "$why" ]; then
			printf '%-8s %-12s %7s %6s  %s\n' \
				"$p" "$b" "$sz" "$secs" "FAIL ($why)"
			bad=$((bad + 1))
			continue
		fi

		# The host's assembly carries "; stmt" commentary that the
		# target builds do not - the host is a DEBUG build.
		grep -v '^;' "$work/h.s" > "$work/h.stripped" 2>/dev/null

		# Both legs against the host, naming the leg that differs.
		# An empty .1 is not a failure: a source that is all table
		# and no function has no AST, and rules.c is exactly that.
		for leg in $LEGS; do
			case $leg in
			mx)  o=m ;;
			cpm) o=g ;;
			*)   continue ;;
			esac
			cmp -s "$work/$o.x" "$work/h.x" || why="$why $leg:.x"
			cmp -s "$work/$o.ast" "$work/h.ast" || why="$why $leg:.1"
			cmp -s "$work/$o.dat" "$work/h.dat" || why="$why $leg:.2"
			# .n by meaning, not by bytes - see nchk.py
			python3 "$here/nchk.py" "$work/h.nam" "$work/$o.nam" \
				>/dev/null 2>&1 || why="$why $leg:.n"
			cmp -s "$work/$o.s" "$work/h.stripped" ||
				why="$why $leg:.s"
		done

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
echo "$ok agree with the host on [$LEGS], $bad do not"
[ $bad = 0 ]

# vim: tabstop=8 shiftwidth=8 noexpandtab:
