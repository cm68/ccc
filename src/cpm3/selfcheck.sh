#!/bin/sh
#
# Run the compiler's three passes under the CP/M 3 machine and check
# that they produce what the host compiler produces.
#
# cpp and c0 have to match byte for byte.  c1's output is compared
# with the host's DEBUG commentary stripped: the host build defines
# DEBUG and writes a "; stmt" line for every construct, and the CP/M
# build does not, so the assembly is the same and the annotations are
# not.
#
# usage: selfcheck.sh [source.c ...]
#

set -e

here=$(cd "$(dirname "$0")" && pwd)
top=$(cd "$here/../.." && pwd)
sim=$here/cpm3
work=$(mktemp -d "${TMPDIR:-/tmp}/cpm3check.XXXXXX")
trap 'rm -rf "$work"' EXIT

for f in "$sim" "$top/cpm/bin/cpp.com" "$top/cpm/bin/c0.com" \
	 "$top/cpm/bin/c1.com"; do
	if [ ! -f "$f" ]; then
		echo "missing $f - make && make cpm first" >&2
		exit 2
	fi
done
cp "$top"/cpm/bin/cpp.com "$top"/cpm/bin/c0.com "$top"/cpm/bin/c1.com "$work"/

srcs="$*"
if [ -z "$srcs" ]; then
	srcs=$here/tests/*.c
fi

fail=0
for src in $srcs; do
	b=$(basename "$src" .c)
	printf '%-14s' "$b"

	# CP/M is 8.3 and has no directories: everything in one place
	# under a short name.
	cp "$src" "$work/t.c"

	( cd "$work" && rm -f t.x t.n t.1 t.2 t.s h.* )

	# The host, as the reference.  Run from the same directory on
	# the same relative name: cpp records the source file it was
	# given in the lexeme stream, so a full path here and a bare
	# name under CP/M would differ for that reason alone.
	( cd "$work" && "$top"/unix/bin/cpp -DCCC -o h t.c ) >/dev/null 2>&1
	( cd "$work" && "$top"/unix/bin/c0 h.x h.1 h.2 ) >/dev/null 2>&1
	( cd "$work" && "$top"/unix/bin/c1 h.1 h.2 h.s ) >/dev/null 2>&1

	# and the same three under CP/M
	( cd "$work" && "$sim" -d . cpp.com -DCCC -o t t.c ) >/dev/null 2>&1
	( cd "$work" && "$sim" -d . c0.com t.x t.1 t.2 ) >/dev/null 2>&1
	( cd "$work" && "$sim" -d . c1.com t.1 t.2 t.s ) >/dev/null 2>&1

	bad=""
	cmp -s "$work/t.x" "$work/h.x" || bad="$bad .x"
	cmp -s "$work/t.1" "$work/h.1" || bad="$bad .1"
	cmp -s "$work/t.2" "$work/h.2" || bad="$bad .2"
	grep -v '^;' "$work/h.s" > "$work/h.stripped" 2>/dev/null || true
	cmp -s "$work/t.s" "$work/h.stripped" || bad="$bad .s"

	# and it has to be assembleable, not merely equal
	if ! "$top"/unix/bin/asz "$work/t.s" -o "$work/t.o" >/dev/null 2>&1; then
		bad="$bad asm"
	fi

	if [ -n "$bad" ]; then
		echo "FAIL$bad"
		fail=1
	else
		echo "ok"
	fi
done

if [ $fail != 0 ]; then
	echo "selfcheck: the CP/M passes disagree with the host"
	exit 1
fi
echo "selfcheck: the compiler runs under CP/M 3 and agrees with the host"

# vim: tabstop=8 shiftwidth=8 noexpandtab:
