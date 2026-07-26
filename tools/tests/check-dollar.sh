#!/bin/sh
#
# $ must be the address of the instruction being assembled, not how
# far the emitter has got through it - so "jr $+2" targets the next
# instruction and assembles to a zero displacement.
#
# A $-relative jp must also not be relaxed to jr, since shrinking the
# instruction would move where $+n points.

set -e
dir=$(dirname "$0")
asz="$dir/../asz"
obj=$(mktemp)
trap 'rm -f "$obj"' EXIT

"$asz" -o "$obj" "$dir/dollar.s"

# text segment: 16 bytes, straight after the 16-byte object header
want="00 18 00 18 fe 18 fc 00 21 08 00 00 c3 0f 00 00"
got=$(od -A n -v -t x1 -j 16 -N 16 "$obj" | tr -s ' \n' ' ')
got=${got# }
got=${got% }

if [ "$got" = "$want" ]; then
	echo "dollar: ok"
	exit 0
fi

echo "dollar: FAIL" >&2
echo "  want: $want" >&2
echo "  got:  $got" >&2
exit 1
