# pass2 Architecture

The pass2 code generator (`c1`) translates the binary AST from pass1 into Z80
assembly.

```
c1 <base>.1 <base>.2 <base>.s
```

- **`<base>.1`** — input, the AST (see [../AST_FORMAT.md](../AST_FORMAT.md))
- **`<base>.2`** — input, the assembly pass1 already wrote for globals, string
  data, and static initializers; copied through
- **`<base>.s`** — output, the assembly for the functions
- **`<base>.n`** — read alongside `.1`: cpp's id-to-name sidecar. Identifiers
  travel through the front of the compiler as 2-byte ids, and **c1 is where
  ids become symbols again** — expansion happens as names are read.

## Streaming Model

The AST is **not** ingested into a complete tree. Processing is streaming: each
expression is parsed, labeled, rewritten with code emitted along the way, and
freed before the next. This keeps the memory footprint small enough for the
64KB target.

```
AST stream ──┬── functions (AST_FUNC) ──→ prolog, then for each statement:
             │                               readexpr()  → build tree
             │                               setdest()   → flags/value/none
             │                               label()     → Sethi-Ullman counts
             │                               assign()    → target registers
             │                               rewrite()   → match rules, emit
             │                               freeexpr()  → release memory
             │
             └── in-body asm (ASM) ──→ pass through verbatim
```

Globals, strings, and static initializers never reach pass2's AST reader — they
arrive already assembled in `.2`, and `copyinit()` copies that file through.

**Parse-time work** (`parseast.c`, `expr.c`):
- Symbol resolution: an AST `SYM` becomes `SYMREF` (symbol + link-time offset);
  `LOCALVAR`/`REGVAR` arrive already resolved by pass1
- Type/size computation: `width` from the AST type suffix, sized via `TSIZE()`
- Argument lists: `CALL` args are chained through `ARGNODE` nodes
- Name expansion: `@<id>` markers are resolved from the `.n` sidecar as read

**No intermediate representation** — the expression tree exists only between
`readexpr()` and `freeexpr()`.

## Expression Tree

The core data structure is `Expr` (`expr.h`). It is deliberately small — six
bytes of scalars, two pointers, and a union:

```c
typedef struct Expr {
    unsigned char op;      /* operator: lexeme.h token or opcodes.h synthetic */
    unsigned char width;   /* type suffix: b/B/s/S/l/L/v */
    unsigned char dest;    /* DEST_NONE / FLAGS / VALUE / STACK */
    unsigned char regs;    /* Sethi-Ullman label: registers needed */
    unsigned char tgt;     /* target register (R_HL, R_DE, 0 = any) */
    unsigned char nored;   /* don't reduce: preserve for the parent's rule */
    struct Expr *left, *right;
    union {
        long val;                                  /* NUMBER */
        char *name;                                /* SYM */
        struct { unsigned char argc; } call;       /* CALL */
        struct { unsigned char reg; short off; } var;   /* LOCALVAR/REGVAR/INDEX */
        struct { unsigned short amt; } incdec;     /* PREINC/POSTINC/... */
        struct { unsigned char off, wid; } bf;     /* BFEXTRACT/BFASSIGN */
        struct { char *name; short off; } symref;  /* SYMREF */
    } u;
} Expr;
```

### Destination Context

`dest` says what the result is *for*, and rules can match on it:

| Value | Name | Meaning |
|------:|------|---------|
| 0 | `DEST_NONE` | Expression statement — discard the result |
| 1 | `DEST_FLAGS` | Condition — leave the answer in the CPU flags |
| 2 | `DEST_VALUE` | Value needed in a register |
| 3 | `DEST_STACK` | Push it: a call argument |

### Operator Codes

Expression opcodes are the AST's (see [../AST_FORMAT.md](../AST_FORMAT.md)),
plus pass2's own **synthetic opcodes** from `opcodes.h`, which only exist
during rewriting:

| Value | Name | Meaning |
|------:|------|---------|
| 226 | `INDEX` | register+offset addressing: `(ix+d)`, `(iy+d)` |
| 227 | `INHL` | the value is in HL |
| 228 | `INDE` | the value is in DE |
| 229 | `INA` | the value is in A |
| 230 | `INBC` | the value is in BC |
| 231 | `SYMREF` | symbol + offset, resolved at link time |
| 232 | `CODE` | emitted assembly; carries the result's location |
| 233 | `INE` | the value is in E (low byte of DE) |
| 234 | `INL` | the value is in L (low byte of HL) |

Note pass1 normalizes `GT`/`GE` away, so only `LT`, `LE`, `EQ`, and `NEQ`
arrive — but the rewriter re-creates `GT`/`GE` internally when it flips a
comparison (`GT(a,n)` → `GE(a,n+1)`).

## Rewriting

Code generation is **table-driven tree rewriting**, not a tree walk with a
`switch` per operator. A rule matches a pattern in the tree, emits an assembly
template, and replaces the matched subtree with a `CODE` node naming where the
result now lives. This repeats to a fixed point, depth first.

Fully described in [REWRITE.md](REWRITE.md); [CONDITIONS.md](CONDITIONS.md)
covers the flag-context half, [STACK.md](STACK.md) the frame layout, and
[HELPERS.md](HELPERS.md) the runtime routines the templates call.

## Register Model

| Code | Register | Role |
|-----:|----------|------|
| 1 | `R_B` | register variable (byte) |
| 2 | `R_C` | register variable (byte) |
| 3 | `R_BC` | register variable (word) |
| 4 | `R_IX` | struct pointer register variable |
| 5 | `R_DE` | secondary temp |
| 6 | `R_HL` | primary accumulator for word results |
| 7 | `R_A` | byte accumulator |
| 8 | `R_IY` | frame pointer |
| 9–12 | `R_E R_D R_L R_H` | individual halves of DE and HL |

Only HL and DE are free as temporaries: BC and IX belong to register variables
when a function has them, and IY is the frame pointer.

Comparison results are named as flags rather than registers:

| Code | Name | Meaning |
|-----:|------|---------|
| 16 | `F_Z` | zero |
| 17 | `F_NZ` | not zero |
| 18 | `F_C` | carry |
| 19 | `F_NC` | not carry |
| 20 | `F_M` | sign set: negative |
| 21 | `F_P` | sign clear: non-negative |
| 22 | `F_CC` | *not a flag* — "the flag this comparison answers in" |

`F_CC` is what a rule writes when it serves a whole family of comparisons and
the answer depends on which one; `ccflag()` works out the real flag from the
operator and its signedness. That unification removed 89 near-identical rows
from the rule table.

## Calling Convention

- Arguments pushed right-to-left
- Word results in HL, byte results in A
- Long results via the `lL`/`lR` memory temporaries
- The frame is set up by a `fent*` helper and torn down by a `fex*` helper
  (see [HELPERS.md](HELPERS.md) and [STACK.md](STACK.md)); a function's exit
  is a `jp` to its own `X<name>` label

## Statement Processing

`parseStmt()` in `parseast.c` dispatches on the AST statement opcodes:

| Opcode | Statement | Handling |
|-------:|-----------|----------|
| 222 `AST_BLOCK` | block | Read the statement count, recurse that many times |
| 147 `IF` | if | Read `nlabels`, lower the condition to flags, branch, then/else |
| 146 `RETURN` | return | Evaluate the value, `jp` to the function's exit label |
| 112 `LABEL` | label | Emit the label |
| 145 `GOTO` | goto | `jp label` |
| 150 `SWITCH` | switch | Evaluate the control value, jump over the bodies, emit the comparison chain after them |
| 151 `CASE` | case | Record the value and label, emit the body |
| 155 `DEFAULT` | default | Record the default label, emit the body |
| 157 `ASM` | inline asm | Copy the length-prefixed text through |
| 1 `SEMI` | empty | Nothing |
| *(other)* | expression | Not an opcode — the byte begins an expression |

## Switch Implementation

The stream is sequential — `SWITCH`, then each `CASE` with its value and its
body — so **the values are not all known before the first body has to be
emitted**. So: the control value is worked out, the bodies are jumped over, and
the comparison chain goes *after* them, where every case label is known.
Nothing falls into that block — the last body jumps past it, and the bodies
were jumped over rather than run, so the control value is still live when the
comparisons are reached.

Case values are sixteen-bit ints, so a value over 255 widens the switch to a
pair table (`swtabw`) with a sixteen-bit compare. Within a byte the chain is a
`cp` against A; a control expression need not be a byte — a state machine over
an `int` is the usual shape — so a word control in a byte switch tests its high
byte once before comparing the low one. `MAXSWNEST` (8) bounds switch nesting.

`break` never appears: cpp lowered it to `goto __S<n>B` and appended the label.

## Output

`out()`, `outc()`, `outd()`, and `outf()` in `astio.c` write the assembly.
DEBUG builds interleave `;`-comments naming each statement and expression as it
is read, which is the practical way to inspect what pass2 made of an AST — see
the note in [../ASTPP.md](../ASTPP.md).

## Long (32-bit) Support

Long values use the memory temporaries `lL` and `lR`, with runtime helpers for
the arithmetic. See [HELPERS.md](HELPERS.md) and
[../../libsrc/libc/QLONG.md](../../libsrc/libc/QLONG.md).

## File Organization

| File | Lines | Purpose |
|------|------:|---------|
| `pass2.h` | 100 | Shared definitions: type suffixes, register and flag codes |
| `expr.h` | 88 | `Expr`, the tree builders, and the tree operations |
| `opcodes.h` | 21 | pass2's synthetic opcodes (226+) |
| `rules.h` | 155 | `struct rule`, the pattern specials, and the packing macros |
| `pass2.c` | 156 | Main: argument handling, file setup |
| `astio.c` | 371 | AST reading, the `.n` sidecar, output primitives |
| `parseast.c` | 866 | Statement dispatch, function prolog/epilog, switch |
| `expr.c` | 689 | Tree builders, Sethi-Ullman labeling, target assignment |
| `rewrite.c` | 1570 | The rule matcher and the template interpolator |
| `lower.c` | 2570 | What matched rules call: compounds, longs, calls, spills |
| `rules.c` | 3138 | The rule table, the shared templates, and `preserve[]` |

`lower.c` was split out of `rewrite.c`, and for the usual reason: the biggest
pass2 source has to fit through the preprocessor *on the target*, and cpp's
per-unit tables are paid per translation unit.

`mkrulepat.py` regenerates `rulepat[]` — the DEBUG spellings of the rules —
from `rules[]` itself, and the Makefile runs it before every build of
`rules.c`. The two used to be maintained by hand in parallel and drifted the
first time rules were added without names, so every trace line after the
insertion point named the wrong rule.

Generated: `debug.h` and `debugtags.c`, from `makedebug.sh`.
