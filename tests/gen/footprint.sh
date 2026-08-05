#!/bin/sh
#
# The memory footprint of the self-host, measured, with a floor.
#
# The three simulated passes compile every one of the compiler's own
# sources - the set that defines the tipping point - and each run's
# brk/gap is read out of the simulator's -S report.  Correctness
# rides along: c0 and c1 must produce byte-identical output to the
# host passes (cpp is compared with matching relative paths, because
# the spelling of the source path lands in the line markers).
#
# Failure means one of:
#   - a pass ran OUT OF MEMORY on a source it used to fit
#   - a gap fell below the floor (default 256 bytes): still fitting,
#     but a bug fix away from not
#   - simulated output diverged from the host's
#
# The full gap table goes to stdout so the drift is visible long
# before the floor is hit.
#
set -e

here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
FLOOR=${FLOOR:-256}
work=${FPWORK:-$here/fpwork}

# stage: sources laid out the way the sim's chroot sees them
rm -rf "$work"
mkdir -p "$work/cpp" "$work/pass1" "$work/pass2" "$work/inc" "$work/lib"
for d in cpp pass1 pass2; do
	cp "$root"/ccc/$d/*.c "$root"/ccc/$d/*.h "$work/$d/" 2>/dev/null || true
done
cp "$root"/libsrc/include/*.h "$work/inc/"
cp "$root"/ccc/lib/*.h "$work/lib/" 2>/dev/null || true

make -C "$root/ccc/cpp" mx-ccc >/dev/null
make -C "$root/ccc/pass1" mx-ccc >/dev/null
make -C "$root/ccc/pass2" mx-ccc >/dev/null
cp "$root/ccc/cpp/comccc/cpp.mx" "$root/ccc/pass1/mxccc/c0.mx" \
   "$root/ccc/pass2/mxccc/c1.mx" "$work/"

SIM="$root/root/sim -S -d $work"
fail=0
printf '%-18s %-13s %-13s %-13s\n' source "cpp gap" "c0 gap" "c1 gap"

gapof() {
	grep -o 'gap [0-9-]*' "$1" | tail -1 | cut -d' ' -f2
}

for d in cpp pass1 pass2; do
  for f in "$work"/$d/*.c; do
	b=$(basename "$f" .c)
	[ "$b" = test ] && continue
	rm -f "$work"/s.* "$work"/h.* "$work"/o.*

	# host chain, with the same relative spelling the sim uses
	(cd "$work" &&
	 "$root"/ccc/cpp/cpp -DCCC -iinc -I$d -Ilib -o h $d/$b.c &&
	 "$root"/ccc/pass1/c0 h.x h.1 h.2 &&
	 "$root"/ccc/pass2/c1 h.1 h.2 h.s) >/dev/null 2>&1 || true

	# simulated chain, spelled exactly as the host chain was: the
	# source path lands in the line markers, so a leading slash
	# would be a byte of divergence in every marker
	(cd "$work" && timeout 300 $SIM cpp.mx -DCCC -iinc -I$d -Ilib \
		-o s $d/$b.c </dev/null) >"$work/o.cpp" 2>&1 || true
	gc=$(gapof "$work/o.cpp"); : "${gc:=?}"
	if grep -q "out of memory" "$work/o.cpp"; then
		echo "$d/$b: cpp OUT OF MEMORY (the tipping point)"; fail=1
		printf '%-18s %-13s %-13s %-13s\n' "$d/$b" OOM - -
		continue
	fi
	cmp -s "$work/s.x" "$work/h.x" || { echo "$d/$b: cpp DIVERGES"; fail=1; }

	(cd "$work" && timeout 300 $SIM c0.mx s.x s.1 s.2 \
		</dev/null) >"$work/o.c0" 2>&1 || true
	g0=$(gapof "$work/o.c0"); : "${g0:=?}"
	if grep -q "out of memory" "$work/o.c0"; then
		echo "$d/$b: c0 OUT OF MEMORY (the tipping point)"; fail=1
		printf '%-18s %-13s %-13s %-13s\n' "$d/$b" "$gc" OOM -
		continue
	fi
	cmp -s "$work/s.1" "$work/h.1" && cmp -s "$work/s.2" "$work/h.2" ||
		{ echo "$d/$b: c0 DIVERGES"; fail=1; }

	g1=-
	if [ -s "$work/s.2" ] && [ -s "$work/s.1" ]; then
		(cd "$work" && timeout 600 $SIM c1.mx s.1 s.2 s.s \
			</dev/null) >"$work/o.c1" 2>&1 || true
		g1=$(gapof "$work/o.c1"); : "${g1:=?}"
		if grep -q "out of memory" "$work/o.c1"; then
			echo "$d/$b: c1 OUT OF MEMORY"; fail=1; g1=OOM
		else
			grep -v '^;' "$work/s.s" >"$work/s.cmp" 2>/dev/null || true
			grep -v '^;' "$work/h.s" >"$work/h.cmp" 2>/dev/null || true
			cmp -s "$work/s.cmp" "$work/h.cmp" ||
				{ echo "$d/$b: c1 DIVERGES"; fail=1; }
		fi
	fi

	printf '%-18s %-13s %-13s %-13s\n' "$d/$b" "$gc" "$g0" "$g1"
	for g in "$gc" "$g0" "$g1"; do
		case "$g" in -|OOM|"?") continue;; esac
		if [ "$g" -lt "$FLOOR" ]; then
			echo "$d/$b: gap $g under the $FLOOR-byte floor"
			fail=1
		fi
	done
  done
done

rm -rf "$work"
if [ "$fail" = 0 ]; then
	echo "footprint: every pass fits every source, no gap under $FLOOR"
else
	echo "footprint: FAILED - the self-host is at or past the tipping point"
fi
exit $fail
