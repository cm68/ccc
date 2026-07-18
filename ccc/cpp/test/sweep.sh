#!/bin/sh
# sweep.sh - input-phase invariance test.
#
# The lexer refills its input buffer every TBSIZE (512) bytes, so any
# construct can straddle a refill boundary.  Bugs in that path are
# position-dependent (the E_O_F-straddle heap corruption was one).
# This test shifts a construct-rich payload through every phase of
# the 512-byte block with a leading pad comment; with -N the output
# must be byte-identical for all 512 phases.
#
# Usage: ./sweep.sh [cpp-binary]

CPP=${1:-../cpp}
cd "$(dirname "$0")"

if [ ! -x "$CPP" ]; then
    echo "sweep: cpp not found at $CPP"
    exit 1
fi

work=sweep.tmp
rm -rf $work
mkdir -p $work

# Construct-rich payload: macros (object + function-like), strings
# with escapes, char constants, both comment styles, continuations,
# conditionals, loops.  Emitted twice (second copy renamed) so the
# constructs span several 512-byte blocks as the phase shifts.
cat > $work/payload.c <<'EOF'
#define ZED 0
#define ONE 1
#define ADDUP(a,b) ((a) + (b))
char *s1 = "plain";
char *s2 = "es\ncape\t\"quoted\" and \\ back";
char c1 = 'x';
char c2 = '\n';
char c3 = '\377';
int t1 = ZED;
int t2 = ADDUP(ONE, 2);
/* block comment * with stars ** inside */
int t3 = ONE; // line comment
int lo\
ng_split = 3;
#if ONE
int kept = ADDUP(ZED, ONE);
#else
int dropped;
#endif
int f(int n)
{
	int i;
	int sum;
	sum = 0;
	for (i = 0; i < n; i++) {
		if (i == ZED)
			continue;
		sum = ADDUP(sum, i);
	}
	while (sum > 100) {
		sum -= ONE;
		break;
	}
	return sum;
}
EOF
{
	cat $work/payload.c
	grep -v '^#define\|^int lo\|^ng_split' $work/payload.c | \
		sed 's/s1/s3/;s/s2/s4/;s/c1/c4/;s/c2/c5/;s/c3/c6/;s/t1/t4/;s/t2/t5/;s/t3/t6/;s/kept/kept2/;s/dropped/dropped2/;s/int f(/int g(/'
} > $work/body.c

fail=0
ref=""
i=0
while [ $i -lt 512 ]; do
	pad=$(awk -v n=$i 'BEGIN { s=""; for (j = 0; j < n; j++) s = s "x"; print s }')
	{ printf '/*%s*/\n' "$pad"; cat $work/body.c; } > $work/in.c
	if ! $CPP -DCCC -N -o $work/out $work/in.c > $work/err.txt 2>&1; then
		echo "sweep: FAIL phase $i - cpp error"
		cat $work/err.txt
		fail=1
		break
	fi
	if [ -z "$ref" ]; then
		cp $work/out.x $work/ref.x
		ref=yes
	elif ! cmp -s $work/out.x $work/ref.x; then
		echo "sweep: FAIL phase $i - output differs from phase 0"
		fail=1
		break
	fi
	i=$((i + 1))
done

rm -f $work/in.c $work/out.x $work/err.txt
if [ $fail -eq 0 ]; then
	echo "PASS: sweep (512 phases byte-identical)"
	rm -rf $work
fi
exit $fail
