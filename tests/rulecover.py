#!/usr/bin/env python3
#
# Which of pass2's rewrite rules ever match.
#
# A rule that matches nothing is worse than one that is absent: it
# reads as coverage that does not exist.  One sat in the table for a
# long time emitting bit n,(iy+d) - correct, and unreachable, because
# an AND reduced its left operand before any rule could see it - and
# every test passed the whole time, because what ran instead was
# right, only longer.
#
# c1 built with -DDEBUG counts what it matches and appends the counts
# to $CCC_RULEHITS at exit.  This compiles a corpus - the compiler, the
# tools, libc and the runtime tests - and reports what never fired.
#
# Usage:  python3 tests/rulecover.py [-v]
#         -v also lists the hottest rules and the failures.
#
import subprocess, glob, os, sys, collections, re

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
HITS = '/tmp/ccc-rulehits.%d' % os.getpid()
VERBOSE = '-v' in sys.argv

CORPUS = sorted(set(
    glob.glob(ROOT + '/src/cpp/*.c') + glob.glob(ROOT + '/src/pass1/*.c') +
    glob.glob(ROOT + '/src/pass2/*.c') + glob.glob(ROOT + '/tools/*.c') +
    glob.glob(ROOT + '/tests/run/rt_*.c') +
    glob.glob(ROOT + '/tests/run/gp_*.c') + glob.glob(ROOT + '/src/libc/*.c') +
    glob.glob(ROOT + '/src/libu/*.c') + glob.glob(ROOT + '/src/libcpm/*.c')))

env = dict(os.environ, CCC_RULEHITS=HITS)
fails = []
for f in CORPUS:
    d = os.path.dirname(f)
    s = f[:-2] + '.s'
    #
    # ccc -s writes <base>.s beside the source, and some of these
    # directories hold hand-written assembly under a name a .c file
    # also uses - libcpm/getargs.s sits next to libcpm/getargs.c and is
    # a source, not a product.  Keep whatever was there and put it back.
    #
    saved = open(s, 'rb').read() if os.path.exists(s) else None
    r = subprocess.run(
        #
        # -I src/cpp is for lexeme.h, which pass1, pass2 and astpp all
        # read - src/pass1/GNUmakefile carries the same -I../cpp and says
        # so.  Without it every one of those files failed to compile and
        # the run said so in a count nobody read: 44 of 279, and thirty
        # of them were the compiler itself.  The corpus was measuring
        # rule coverage over libc and the runtime tests while leaving out
        # the largest and least regular C in the tree.
        #
        [ROOT + '/desthost/bin/ccc', '-DCCC', '-s', '-I' + d, '-I' + ROOT + '/src/ccclib',
         '-I' + ROOT + '/tests/run', '-I' + ROOT + '/src/include',
         '-I' + ROOT + '/src/cpp',
         os.path.basename(f)], cwd=d, capture_output=True, env=env, timeout=300)
    if r.returncode:
        fails.append(f.replace(ROOT + '/', ''))
    if saved is not None:
        open(s, 'wb').write(saved)
    elif os.path.exists(s):
        os.remove(s)

if not os.path.exists(HITS):
    sys.exit('no counts written - is c1 built with -DDEBUG?')

tot, pat = collections.Counter(), {}
for line in open(HITS):
    i, n, p = line.rstrip('\n').split('\t')
    tot[int(i)] += int(n)
    pat[int(i)] = p
os.remove(HITS)

# index -> line number in rules.c, for a reference that can be followed
src = open(ROOT + '/src/pass2/rules.c').read().split('\n')
start = next(i for i, l in enumerate(src) if 'struct rule rules[]' in l)
lines, idx, i = {}, 0, start
while i < len(src):
    if re.match(r'\s*R\(', src[i]):
        lines[idx] = i + 1
        idx += 1
    i += 1

n = len(pat)
dead = sorted(k for k in pat if not tot[k])
print('corpus %d files, %d did not compile' % (len(CORPUS), len(fails)))
if VERBOSE and fails:
    for f in fails:
        print('    ' + f)
print('rules %d   fired %d (%.0f%%)   never fired %d' %
      (n, n - len(dead), 100.0 * (n - len(dead)) / n, len(dead)))
print()
if VERBOSE:
    print('=== hottest ===')
    for k, c in tot.most_common(20):
        print('  %8d  rules.c:%-5d %s' % (c, lines.get(k, 0), pat[k]))
    print()
print('=== never fired ===')
for k in dead:
    print('  rules.c:%-5d %s' % (lines.get(k, 0), pat[k]))
