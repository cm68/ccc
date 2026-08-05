#!/usr/bin/env python3
"""Native-vs-Z80 pipeline equivalence and memory footprint matrix.

For every cpp and pass1 source, run:
  host cpp  -> .x     vs   sim cpp.mx  -> .x     (byte compare)
  host c0   -> .1/.2  vs   sim c0.mx   -> .1/.2  (byte compare + rc)
capturing the simulator's -S heap/stack report for both Z80 programs.

Requires the native cpp/xdump/c0 and the mx-flavor Z80 builds
(comzc3/*.o and mxzc3/c0.mx); `make footprint` builds them first.

Exits nonzero if any .x differs, any .1/.2 differs, or any run
OOMs/hangs, so it can gate CI.  The footprint table always prints.
"""

import os
import re
import subprocess
import sys
import tempfile
import filecmp
import shutil

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
R = ROOT + "/root"
C = ROOT + "/ccc/cpp"
P1 = ROOT + "/ccc/pass1"
LB = ROOT + "/ccc/lib"
L = ROOT + "/libsrc/include"

CPP_SRCS = ("cpp lex io macro kw emit util kwtab lexdata filtenum filtknr "
            "filtdecl filtbrace filtctrl filtutil typetab").split()
P1_SRCS = ("pass1 error lexread expr parse decl type declare outast "
           "regalloc util").split()


def stackline(errfile):
    m = None
    for l in open(errfile, errors="replace").read().splitlines():
        if l.startswith("stack:"):
            m = l
    if not m:
        return None
    g = re.findall(r'initial (\S+) low (\S+) used (\d+) brk (\S+) gap (-?\d+)', m)
    if not g:
        return None
    return dict(zip(["initial", "low", "used", "brk", "gap"], g[0]))


def run_group(work, cppmx, tag, srcdir, srcs):
    incs = ["-DCCC", "-i" + L, "-I" + srcdir, "-I" + LB, "-I" + L]
    rows = []
    for b in srcs:
        row = {"b": b, "tag": tag}
        hx = f"{work}/h_{tag}_{b}"
        gx = f"{work}/g_{tag}_{b}"
        subprocess.run([C + "/cpp"] + incs + ["-o", hx, f"{srcdir}/{b}.c"],
                       cwd="/", capture_output=True, timeout=60)
        with open(f"{work}/cpp_{tag}_{b}.err", "w") as ef:
            subprocess.run([R + "/sim", "-S", "-d", "/", cppmx] + incs
                           + ["-o", gx, f"{srcdir}/{b}.c"],
                           stdin=subprocess.DEVNULL, stdout=subprocess.DEVNULL,
                           stderr=ef, timeout=120)
        row["xsize"] = os.path.getsize(hx + ".x") if os.path.exists(hx + ".x") else -1
        row["xsame"] = (os.path.exists(gx + ".x")
                        and filecmp.cmp(hx + ".x", gx + ".x", shallow=False))
        row["cppstk"] = stackline(f"{work}/cpp_{tag}_{b}.err")
        hrc = subprocess.run([P1 + "/c0", hx + ".x", hx + ".1", hx + ".2"],
                             capture_output=True, timeout=60).returncode
        with open(f"{work}/c0_{tag}_{b}.err", "w") as ef:
            try:
                grc = subprocess.run([R + "/sim", "-S", "-d", "/",
                                      P1 + "/mxzc3/c0.mx",
                                      hx + ".x", gx + ".1", gx + ".2"],
                                     stdin=subprocess.DEVNULL,
                                     stdout=subprocess.DEVNULL,
                                     stderr=ef, timeout=180).returncode
            except subprocess.TimeoutExpired:
                grc = 124
        err = open(f"{work}/c0_{tag}_{b}.err", errors="replace").read()
        s1 = (os.path.exists(gx + ".1") and os.path.exists(hx + ".1")
              and filecmp.cmp(hx + ".1", gx + ".1", shallow=False))
        s2 = (os.path.exists(gx + ".2") and os.path.exists(hx + ".2")
              and filecmp.cmp(hx + ".2", gx + ".2", shallow=False))
        if "out of memory" in err:
            row["st"] = "OOM"
        elif grc == 124:
            row["st"] = "HANG"
        elif hrc == grc and s1 and s2:
            row["st"] = "SAME"
        else:
            row["st"] = (f"DIFF(h={hrc},g={grc},"
                         f"1={'ok' if s1 else 'X'},2={'ok' if s2 else 'X'})")
        row["c0stk"] = stackline(f"{work}/c0_{tag}_{b}.err")
        rows.append(row)
    return rows


def main():
    work = tempfile.mkdtemp(prefix="footprint.")
    cppmx = work + "/cpp.mx"
    r = subprocess.run([R + "/bin/wsld", "-s", "-o", cppmx, "-Ttext=0x100",
                        R + "/lib/crt0.o"]
                       + [f"{C}/comzc3/{b}.o" for b in CPP_SRCS]
                       + ["-L" + R + "/lib/zc3", "-lccc", "-lc", "-lu", "-lc"],
                       capture_output=True, text=True)
    if r.returncode != 0:
        print("cpp.mx link failed:", r.stderr, file=sys.stderr)
        return 2

    rows = (run_group(work, cppmx, "cpp", C, CPP_SRCS)
            + run_group(work, cppmx, "p1", P1, P1_SRCS))

    print(f"{'file':14} {'x-size':>6} {'x':>2} |"
          f" {'cpp-brk':>7} {'cpp-gap':>7} |"
          f" {'c0-brk':>6} {'c0-stk':>6} {'c0-gap':>7} status")
    fails = 0
    for r_ in rows:
        c, k = r_["cppstk"], r_["c0stk"]
        if not (r_["xsame"] and r_["st"] == "SAME"):
            fails += 1
        print(f"{r_['tag'] + '/' + r_['b']:14} {r_['xsize']:>6}"
              f" {'ok' if r_['xsame'] else 'XX':>2} |"
              f" {(c or {}).get('brk', '?'):>7} {(c or {}).get('gap', '?'):>7} |"
              f" {(k or {}).get('brk', '?'):>6} {(k or {}).get('used', '?'):>6}"
              f" {(k or {}).get('gap', '?'):>7} {r_['st']}")
    n = len(rows)
    print(f"\n{n - fails}/{n} SAME", "" if fails == 0 else f"- {fails} FAILED")
    shutil.rmtree(work, ignore_errors=True)
    return 1 if fails else 0


if __name__ == "__main__":
    sys.exit(main())
