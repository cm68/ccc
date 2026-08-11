#!/bin/sh
#
# host c0/c1 against the .mx pair, over the generated corpus.
#
# One source at a time was the shape this started in, and it cost a
# quarter of an hour: the simulator is where the time goes and every
# file waits for the one before it.  Nothing here is shared but the
# names - h.ast, s.s and the rest, all written into $RUN by whichever
# file was current - so each source gets its own directory and they
# all run at once.  SIMJOBS overrides the width.
#
set -e

root=$1
run=$2
JOBS=${SIMJOBS:-$( n=$(nproc 2>/dev/null || echo 4); echo $((n * 2)) )}
out="$run/.simout"
rm -rf "$out"; mkdir -p "$out"

one() {
	b=$1
	jd="$run/.sim$b"
	rm -rf "$jd"; mkdir -p "$jd"
	cp "$run/c0.mx" "$run/c1.mx" "$jd/"
	(cd "$jd" &&
	 "$root"/src/cpp/cpp -DRT_CCC -i"$root"/src/include -I"$run" \
		-o $b "$run/$b.c" >/dev/null 2>&1
	 "$root"/src/pass1/c0 $b.x h.ast h.dat >/dev/null 2>&1
	 "$root"/src/pass2/c1 h.ast h.dat h.s >/dev/null 2>&1
	 timeout 300 "$root"/tests/sim -d "$jd" /c0.mx /$b.x /s.1 /s.2 \
		</dev/null >/dev/null 2>&1
	 timeout 600 "$root"/tests/sim -d "$jd" /c1.mx /s.1 /s.2 /s.s \
		</dev/null >/dev/null 2>&1
	 grep -v '^;' h.s > h.cmp 2>/dev/null || true
	 grep -v '^;' s.s > s.cmp 2>/dev/null || true
	 if cmp -s h.cmp s.cmp; then echo "$b: identical" > "$out/$b"
	 else echo "$b: DIVERGES" > "$out/$b"; : > "$out/$b.bad"; fi) || \
		{ echo "$b: DIVERGES" > "$out/$b"; : > "$out/$b.bad"; }
	rm -rf "$jd"
}

list=$(cd "$run" && ls gp_*.c 2>/dev/null | sed 's/\.c$//')
n=0
for b in $list; do
	one "$b" &
	n=$((n + 1))
	if [ "$n" -ge "$JOBS" ]; then wait; n=0; fi
done
wait

fail=0
for b in $list; do
	[ -f "$out/$b" ] && cat "$out/$b"
	[ -f "$out/$b.bad" ] && fail=1
done
rm -rf "$out"
(cd "$run" && rm -f h.* s.* gp_*.x gp_*.nam c0.mx c1.mx)
exit $fail
