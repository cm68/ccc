#!/bin/sh
#
# Assemble every instruction in appendix-c.txt and check that the
# object code matches the Z280 manual's Appendix C (Instructions in
# Alphabetic Order).  The data file is one instruction per line:
#
#       mnemonic operands<TAB>expected hex bytes
#
# spelled in the assembler's own syntax and de-duplicated: LDW/INCW/
# DECW fold into ld/inc/dec (the operand width says which), and the
# families the assembler does not implement - EPU, EXTS, LDUD/LDUP,
# the 32-bit MULTW/DIVW, TSET - are absent.  Constants are 0x hex, and
# the relative jumps use $+n so a displacement is fixed regardless of
# where the instruction lands.

set -e
dir=$(dirname "$0")
asz="$dir/../asz280"
data="$dir/appendix-c.txt"
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

printf '\t.text\n' > "$tmp/appendix.s"
: > "$tmp/want.hex"

# A literal tab, because dash (this script's /bin/sh) does not do $'\t'.
TAB=$(printf '\t')
while IFS="$TAB" read -r ins bytes; do
	case "$ins" in ''|\#*) continue ;; esac
	printf '\t%s\n' "$ins" >> "$tmp/appendix.s"
	printf '%s' "$bytes" | tr -d ' \t' >> "$tmp/want.hex"
done < "$data"

"$asz" -o "$tmp/appendix.o" "$tmp/appendix.s"

# A Whitesmith's object is a 16-byte header, then the text segment.
nbytes=$(( $(wc -c < "$tmp/want.hex") / 2 ))
got=$(od -A n -v -t x1 -j 16 -N "$nbytes" "$tmp/appendix.o" \
	| tr -s ' \n' ' ' | sed 's/^ //; s/ $//')
want=$(sed 's/../& /g' "$tmp/want.hex" | sed 's/ $//')

if [ "$got" = "$want" ]; then
	echo "appendix-c: ok ($nbytes bytes)"
	exit 0
fi

echo "appendix-c: FAIL" >&2
echo "  first mismatch (byte offset: want got):" >&2
i=0
for w in $want; do
	i=$((i + 1))
	g=$(echo "$got" | cut -d' ' -f"$i")
	[ "$w" = "$g" ] || { echo "  $i: $w $g" >&2; break; }
done
exit 1
