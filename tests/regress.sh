#!/bin/bash
#
# regress.sh - cpp regression harness
#
# Runs cpp over a comprehensive corpus and compares each file's output
# (.x byte stream + exit code + stderr) against a baseline tree.
#
# Usage:
#   ./regress.sh             # compare current output to baseline
#   ./regress.sh --bless     # regenerate baseline from current cpp
#   ./regress.sh --keep      # leave temp outputs in place even on pass
#   ./regress.sh --filter X  # only run files matching shell pattern X
#   ./regress.sh --cpp PATH  # use a specific cpp binary
#   ./regress.sh --list      # just list the corpus, don't run
#
# Baseline layout: tests/baseline/<reldir>/<base>.{x,rc,err}
#   .x   - cpp output (binary lexeme stream).  Missing => cpp failed.
#   .rc  - exit code (always present).
#   .err - stderr   (only present if non-empty after noise filtering).
#
# A regression is any change in any of these three files.

set -u

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO="$(cd "$SCRIPT_DIR/.." && pwd)"
DEFAULT_CPP="$REPO/src/cpp/cpp"
BASELINE="$SCRIPT_DIR/baseline"
WORK="$(mktemp -d "${TMPDIR:-/tmp}/ccc-regress.XXXXXX")"

mode=compare
keep=0
filter=""
cpp_bin="$DEFAULT_CPP"
just_list=0

cleanup() {
    if [ "$keep" -eq 0 ] && [ "$mode" != "bless" ]; then
        rm -rf "$WORK"
    else
        echo "Work dir kept: $WORK"
    fi
}
trap cleanup EXIT

# cpp segfaults on a few inputs (part of the baseline contract).  Suppress
# core dumps so they don't litter the source tree we cd into.
ulimit -c 0 2>/dev/null || true

while [ $# -gt 0 ]; do
    case "$1" in
        --bless)  mode=bless ;;
        --keep)   keep=1 ;;
        --filter) shift; filter="$1" ;;
        --cpp)    shift; cpp_bin="$1" ;;
        --list)   just_list=1 ;;
        -h|--help)
            sed -n '3,/^$/p' "$0" | sed 's/^# *//'
            exit 0
            ;;
        *)
            echo "Unknown arg: $1" >&2
            exit 2
            ;;
    esac
    shift
done

if [ "$just_list" -eq 0 ] && [ ! -x "$cpp_bin" ]; then
    echo "ERROR: cpp not executable: $cpp_bin" >&2
    echo "Build it first (make -C src/cpp cpp) or pass --cpp PATH." >&2
    exit 2
fi
# Resolve to absolute path - the harness cd's into source dirs, so a
# relative --cpp arg would break.
[ -n "$cpp_bin" ] && cpp_bin="$(readlink -f "$cpp_bin")"

#
# Corpus: each line is "DIR | GLOB | EXTRA-FLAGS"
#
# DIR is relative to repo root.  GLOB matches files in DIR.
# cpp is run from DIR so that '-I.' / '-I../lib' resolve like the Makefiles do.
# EXTRA-FLAGS are the include/define flags used by that subtree's Makefile.
#
# Files that intentionally don't preprocess cleanly (system headers we don't
# have, etc.) are still baselined - their failure mode is part of the contract.
#
CORPUS='
src/cpp/test       | *.c | -DCCC -I.. -I../../include
src/cpp            | cpp.c lex.c io.c macro.c kw.c emit.c util.c norm.c filtutil.c knr.c cfold.c tdsrc.c lexdata.c xdump.c | -DCCC -I. -I../ccclib -I../include
src/pass1          | *.c | -DCCC -I. -I../ccclib -I../include
src/pass2          | *.c | -DCCC -I. -I../ccclib -I../include
src/peep           | *.c | -DCCC -I. -I../ccclib -I../include
src/ccclib         | *.c | -DCCC -I../include
src/tools          | *.c | -DCCC -I. -I../ccclib -I../include
src/libc           | *.c | -DCCC -I../include
src/libcpm         | *.c | -DCCC -I../include
src/libu           | *.c | -DCCC -I../include
tests              | *.c | -DCCC -I../src/include
'

#
# expand_globs DIR "glob1 glob2 ..."
#   prints one filename per line (relative to DIR), skipping nonexistent
#
expand_globs() {
    local dir="$1" globs="$2" g f
    cd "$REPO/$dir" || return
    for g in $globs; do
        for f in $g; do
            [ -f "$f" ] && echo "$f"
        done
    done
    cd "$REPO" || return
}

#
# Run cpp on one file. Captures .x + rc + stderr into $WORK/<rel>.
# Args: dir, file (relative to dir), flags
#
run_one() {
    local dir="$1" file="$2" flags="$3"
    local rel="$dir/$file"
    local base="${file%.c}"
    local outdir="$WORK/$dir"
    local out_x="$outdir/$base.x"
    local out_rc="$outdir/$base.rc"
    local out_err="$outdir/$base.err"
    local rc

    mkdir -p "$outdir"

    # cd into source dir so cpp resolves -I and #includes the same way the
    # Makefile does. Outer subshell catches bash's "Segmentation fault" line
    # so it doesn't pollute the harness output. The inner subshell does the
    # actual work and writes cpp's stderr to $out_err.
    {
        ( cd "$REPO/$dir" && "$cpp_bin" $flags -N -o "$outdir/$base" "$file" 2>"$out_err" )
        rc=$?
    } 2>/dev/null

    # Always record rc and err so baseline captures full behavior.
    echo "$rc" >"$out_rc"
    # .i file is debug-only - delete it (it's just xdump of .x and bloats /tmp)
    rm -f "$outdir/$base.i"
    # Normalize stderr: strip environmental noise (ld.so LD_PRELOAD warnings),
    # DEBUG-build statistics (a non-DEBUG cpp prints none, and the baseline
    # must not care which build ran), and absolute paths so the baseline is
    # portable across users/machines.
    if [ -s "$out_err" ]; then
        sed -i \
            -e '/^ERROR: ld\.so: /d' \
            -e '/^POOLSTATS/d' \
            -e "s|$REPO/||g" \
            -e "s|$WORK/||g" \
            "$out_err"
        # If filtering left it empty, remove it.
        [ -s "$out_err" ] || rm -f "$out_err"
    else
        rm -f "$out_err"
    fi
}

#
# Compare one file's output against baseline. Returns 0 on match, 1 on diff.
#
compare_one() {
    local dir="$1" file="$2"
    local base="${file%.c}"
    local rel="$dir/$base"
    local cur_x="$WORK/$rel.x"     b_x="$BASELINE/$rel.x"
    local cur_rc="$WORK/$rel.rc"   b_rc="$BASELINE/$rel.rc"
    local cur_err="$WORK/$rel.err" b_err="$BASELINE/$rel.err"
    local diffs=""

    # Exit code
    local cur_v=0 b_v=0
    [ -f "$cur_rc" ] && cur_v="$(cat "$cur_rc")"
    [ -f "$b_rc"   ] && b_v="$(cat "$b_rc")"
    [ "$cur_v" != "$b_v" ] && diffs="$diffs rc($b_v->$cur_v)"

    # .x bytes
    if [ -f "$cur_x" ] && [ -f "$b_x" ]; then
        if ! cmp -s "$cur_x" "$b_x"; then
            diffs="$diffs xdiff"
        fi
    elif [ -f "$cur_x" ] && [ ! -f "$b_x" ]; then
        diffs="$diffs x-appeared"
    elif [ ! -f "$cur_x" ] && [ -f "$b_x" ]; then
        diffs="$diffs x-vanished"
    fi

    # stderr
    if [ -f "$cur_err" ] && [ -f "$b_err" ]; then
        if ! cmp -s "$cur_err" "$b_err"; then
            diffs="$diffs err-diff"
        fi
    elif [ -f "$cur_err" ] && [ ! -f "$b_err" ]; then
        diffs="$diffs err-new"
    elif [ ! -f "$cur_err" ] && [ -f "$b_err" ]; then
        diffs="$diffs err-gone"
    fi

    if [ -n "$diffs" ]; then
        printf "  FAIL %s -%s\n" "$rel" "$diffs"
        return 1
    fi
    return 0
}

#
# Bless one file: copy current output into baseline.
#
bless_one() {
    local dir="$1" file="$2"
    local base="${file%.c}"
    local rel="$dir/$base"
    local b_dir="$BASELINE/$dir"

    mkdir -p "$b_dir"
    # Always write rc (so baseline is unambiguous).
    cp -f "$WORK/$rel.rc" "$b_dir/$base.rc"
    # .x and .err only if cpp produced them.
    if [ -f "$WORK/$rel.x" ]; then
        cp -f "$WORK/$rel.x" "$b_dir/$base.x"
    else
        rm -f "$b_dir/$base.x"
    fi
    if [ -f "$WORK/$rel.err" ]; then
        cp -f "$WORK/$rel.err" "$b_dir/$base.err"
    else
        rm -f "$b_dir/$base.err"
    fi
}

#
# Main loop
#
total=0
fail=0
ok=0

cd "$REPO" || exit 2

# trim leading/trailing whitespace
trim() { local s="$1"; s="${s#"${s%%[![:space:]]*}"}"; s="${s%"${s##*[![:space:]]}"}"; printf '%s' "$s"; }

echo "$CORPUS" | while IFS='|' read -r d g f; do
    d="$(trim "$d")"
    g="$(trim "$g")"
    f="$(trim "$f")"
    [ -z "$d" ] && continue
    [ ! -d "$REPO/$d" ] && { echo "  skip: $d (no such dir)"; continue; }
    echo "=== $d ==="
    expand_globs "$d" "$g"
done > "$WORK/.manifest"

# Re-process manifest with the flags-per-dir mapping
declare -A FLAGS
while IFS='|' read -r d g f; do
    d="$(trim "$d")"
    f="$(trim "$f")"
    [ -n "$d" ] && FLAGS["$d"]="$f"
done <<<"$CORPUS"

# Track current dir as we walk the manifest
cur_dir=""
while IFS= read -r line; do
    case "$line" in
        "=== "*)
            cur_dir="${line#=== }"
            cur_dir="${cur_dir% ===}"
            continue
            ;;
        "  skip:"*) echo "$line"; continue ;;
    esac
    [ -z "$line" ] && continue
    file="$line"

    rel="$cur_dir/$file"
    # Apply filter
    if [ -n "$filter" ]; then
        case "$rel" in
            *$filter*) ;;
            *) continue ;;
        esac
    fi

    total=$((total + 1))
    if [ "$just_list" -eq 1 ]; then
        echo "$rel  [${FLAGS[$cur_dir]}]"
        continue
    fi

    run_one "$cur_dir" "$file" "${FLAGS[$cur_dir]}"

    if [ "$mode" = "bless" ]; then
        bless_one "$cur_dir" "$file"
        ok=$((ok + 1))
    else
        if compare_one "$cur_dir" "$file"; then
            ok=$((ok + 1))
        else
            fail=$((fail + 1))
        fi
    fi
done < "$WORK/.manifest"

echo
echo "----------------------------------------"
if [ "$just_list" -eq 1 ]; then
    echo "Corpus: $total files"
    exit 0
fi
if [ "$mode" = "bless" ]; then
    echo "Blessed $ok files into $BASELINE/"
    exit 0
fi
echo "Total: $total   OK: $ok   FAIL: $fail"
if [ "$fail" -gt 0 ]; then
    echo
    echo "To inspect a diff:"
    echo "  cmp tests/baseline/<rel>.x $WORK/<rel>.x"
    echo "  diff <(./src/cpp/xdump tests/baseline/<rel>.x) <(./src/cpp/xdump $WORK/<rel>.x)"
    keep=1   # auto-keep work dir on failure
    exit 1
fi
exit 0
