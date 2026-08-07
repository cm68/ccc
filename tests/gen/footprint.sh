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
# Sources are measured in parallel.  Each one is a separate errand -
# a source in, a gap and a verdict out - and the simulator spends
# forty seconds on a source, most of it in c1.  The only thing that
# ever made this serial was the scratch names: every source wrote
# s.x and h.1 into one directory.  Named per source instead, the
# whole table is one wave rather than fifty-five.  FPJOBS overrides
# the width.
#
set -e

here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
FLOOR=${FLOOR:-256}
work=${FPWORK:-$here/fpwork}
JOBS=${FPJOBS:-$( n=$(nproc 2>/dev/null || echo 4); echo $((n * 2)) )}

# stage: sources laid out the way the sim's chroot sees them
rm -rf "$work"
base=$(dirname "$work")
out="$work-out"
rm -rf "$out"; mkdir -p "$out"
mkdir -p "$work/cpp" "$work/pass1" "$work/pass2" "$work/inc" "$work/lib"
for d in cpp pass1 pass2; do
	cp "$root"/ccc/$d/*.c "$root"/ccc/$d/*.h "$work/$d/" 2>/dev/null || true
done
cp "$root"/libsrc/include/*.h "$work/inc/"
cp -r "$root"/libsrc/include/sys "$work/inc/" 2>/dev/null || true
cp "$root"/ccc/lib/*.h "$work/lib/" 2>/dev/null || true

make -C "$root/ccc/cpp" mx-ccc >/dev/null
make -C "$root/ccc/pass1" mx-ccc >/dev/null
make -C "$root/ccc/pass2" mx-ccc >/dev/null
cp "$root/ccc/cpp/comccc/cpp.mx" "$root/ccc/pass1/mxccc/c0.mx" \
   "$root/ccc/pass2/mxccc/c1.mx" "$work/"

export root work base out FLOOR SIMBIN
SIMBIN="$root/root/sim"


# One source: the host chain and the simulated one, compared.  Writes
# its table row and any complaint to out/<tag>, and touches
# out/<tag>.bad if the self-host is at or past the tipping point.
measure() {
	d=${1%/*}; b=${1#*/}
	tag=$2
	# Each source gets its own directory, and the name is three
	# digits wide on purpose: "fpw007" is exactly as long as
	# "fpwork", so the simulator's argv is the same length it was
	# when this ran one source at a time.  argv sits on the initial
	# stack and the gap is measured from there - a longer scratch
	# spelling would quietly cost a dozen bytes of the number being
	# reported.
	jd=$(printf '%s/fpw%03d' "$base" "$tag")
	s=s; h=h; o="$out/$tag.o"
	res="$out/$tag"
	: >"$res"
	gapof() { grep -o 'gap [0-9-]*' "$1" 2>/dev/null | tail -1 | cut -d' ' -f2; }
	bad() { echo "$1" >>"$res"; : >"$out/$tag.bad"; }

	rm -rf "$jd"; cp -al "$work" "$jd" 2>/dev/null || cp -a "$work" "$jd"
	SIM="$SIMBIN -S -d $jd"

	(cd "$jd" &&
	 "$root"/ccc/cpp/cpp -DCCC -iinc -I$d -Ilib -o $h $d/$b.c &&
	 "$root"/ccc/pass1/c0 $h.x $h.1 $h.2 &&
	 "$root"/ccc/pass2/c1 $h.1 $h.2 $h.s) >/dev/null 2>&1 || true

	(cd "$jd" && timeout 300 $SIM cpp.mx -DCCC -iinc -I$d -Ilib \
		-o $s $d/$b.c </dev/null) >"$o.cpp" 2>&1 || true
	gc=$(gapof "$o.cpp"); : "${gc:=?}"
	if grep -q "out of memory" "$o.cpp"; then
		bad "$d/$b: cpp OUT OF MEMORY (the tipping point)"
		printf '%-18s %-13s %-13s %-13s\n' "$d/$b" OOM - - >>"$res"
		return 0
	fi
	cmp -s "$jd/$s.x" "$jd/$h.x" || bad "$d/$b: cpp DIVERGES"

	(cd "$jd" && timeout 300 $SIM c0.mx $s.x $s.1 $s.2 \
		</dev/null) >"$o.c0" 2>&1 || true
	g0=$(gapof "$o.c0"); : "${g0:=?}"
	if grep -q "out of memory" "$o.c0"; then
		bad "$d/$b: c0 OUT OF MEMORY (the tipping point)"
		printf '%-18s %-13s %-13s %-13s\n' "$d/$b" "$gc" OOM - >>"$res"
		return 0
	fi
	cmp -s "$jd/$s.1" "$jd/$h.1" && cmp -s "$jd/$s.2" "$jd/$h.2" ||
		bad "$d/$b: c0 DIVERGES"

	g1=-
	if [ -s "$jd/$s.2" ] && [ -s "$jd/$s.1" ]; then
		(cd "$jd" && timeout 600 $SIM c1.mx $s.1 $s.2 $s.s \
			</dev/null) >"$o.c1" 2>&1 || true
		g1=$(gapof "$o.c1"); : "${g1:=?}"
		if grep -q "out of memory" "$o.c1"; then
			bad "$d/$b: c1 OUT OF MEMORY"; g1=OOM
		else
			grep -v '^;' "$jd/$s.s" >"$jd/$s.cmp" 2>/dev/null || true
			grep -v '^;' "$jd/$h.s" >"$jd/$h.cmp" 2>/dev/null || true
			cmp -s "$jd/$s.cmp" "$jd/$h.cmp" ||
				bad "$d/$b: c1 DIVERGES"
		fi
	fi

	printf '%-18s %-13s %-13s %-13s\n' "$d/$b" "$gc" "$g0" "$g1" >>"$res"
	for g in "$gc" "$g0" "$g1"; do
		case "$g" in -|OOM|"?") continue;; esac
		if [ "$g" -lt "$FLOOR" ]; then
			bad "$d/$b: gap $g under the $FLOOR-byte floor"
		fi
	done
	rm -rf "$jd"; rm -f "$o".*
	return 0
}
list=""
for d in cpp pass1 pass2; do
	for f in "$work"/$d/*.c; do
		b=$(basename "$f" .c)
		[ "$b" = test ] && continue
		list="$list $d/$b"
	done
done

printf '%-18s %-13s %-13s %-13s\n' source "cpp gap" "c0 gap" "c1 gap"

# a batch at a time, JOBS wide.  Every source is independent now that
# the scratch names carry its tag, so the only thing to wait for is
# the slowest one in each batch.
n=0; i=0
for one in $list; do
	i=$((i + 1))
	measure "$one" "$i" &
	n=$((n + 1))
	if [ "$n" -ge "$JOBS" ]; then wait; n=0; fi
done
wait

fail=0; i=0
for one in $list; do
	i=$((i + 1)); tag=$i
	[ -f "$out/$tag" ] && cat "$out/$tag"
	[ -f "$out/$tag.bad" ] && fail=1
done

rm -rf "$work" "$out" "$base"/fpw[0-9][0-9][0-9]
if [ "$fail" = 0 ]; then
	echo "footprint: every pass fits every source, no gap under $FLOOR"
else
	echo "footprint: FAILED - the self-host is at or past the tipping point"
fi
exit $fail
