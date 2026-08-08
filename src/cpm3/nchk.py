#!/usr/bin/env python3
"""
Compare two .n name sidecars by what they mean, not by their bytes.

The .n is a count, a table of offsets, and a blob of NUL-terminated
spellings.  Two runs can pack the blob in a different order and still
agree completely - the offsets say where each name is - so comparing
the files byte for byte cries wolf.  What has to match is the count
and the id-to-name mapping.

It went unchecked entirely until a cpp whose .n was one record short
left the last eighteen names pointing past the end of the file.  c1
read them as empty and emitted jumps with no label, three passes and
a whole layer away from the cause.
"""
import sys

def mapping(path):
    d = open(path, "rb").read()
    if len(d) < 2:
        return None, "shorter than its own count"
    n = d[0] | d[1] << 8
    out = {}
    for i in range(n):
        j = 2 + 2 * i
        if j + 1 >= len(d):
            return None, "offset table runs past the end"
        o = d[j] | d[j + 1] << 8
        if o >= len(d):
            out[i + 1] = None          # past EOF: the bug this catches
            continue
        e = d.find(b"\0", o)
        out[i + 1] = d[o:e if e >= 0 else len(d)].decode("latin1")
    return out, None

a, ea = mapping(sys.argv[1])
b, eb = mapping(sys.argv[2])
if ea or eb:
    print("n: %s" % (ea or eb)); sys.exit(1)
if len(a) != len(b):
    print("n: %d names against %d" % (len(a), len(b))); sys.exit(1)
dangling = [k for k, v in b.items() if v is None]
if dangling:
    print("n: %d ids point past the end of the file: %s"
          % (len(dangling), dangling[:8])); sys.exit(1)
bad = [k for k in a if a[k] != b.get(k)]
if bad:
    print("n: %d ids name something else: %s" % (len(bad), bad[:8])); sys.exit(1)
sys.exit(0)
