# CPP - C Preprocessor

A standalone C preprocessor that produces a compact binary lexeme stream (`.x`) and human-readable preprocessed output (`.i`). Designed for the ccc compiler project with emphasis on minimal memory footprint.

## Overview

The preprocessor performs full C preprocessing including:
- Macro definition and expansion (#define, #undef)
- Conditional compilation (#if, #ifdef, #ifndef, #elif, #else, #endif)
- File inclusion (#include)
- Comment stripping (C and C++ style)
- Tokenization with keyword recognition

## Source Files

| File | Lines | Purpose |
|------|-------|---------|
| cpp.c | 345 | Main entry point, command-line processing, filter-chain wiring |
| lex.c | 1608 | Lexer/tokenizer with embedded CPP directive handling |
| macro.c | 582 | Macro definition, lookup, and expansion |
| io.c | 565 | Unified character stream (files, includes, macros), output buffer stack |
| emit.c | 283 | Binary token output to the `.x` stream |
| kw.c | 264 | Compressed keyword lookup tables |
| filtknr.c | 420 | Filter: K&R → ANSI function-definition conversion |
| filtdecl.c | 381 | Filter: local declaration/initializer separation, typedef tracking |
| filtbrace.c | 545 | Filter: brace insertion around control-structure bodies |
| filtctrl.c | 560 | Filter: control-flow lowering (loops → if/goto/label) |
| filtutil.c | 253 | Shared filter helpers (pending buffers, stacks, label emit) |
| util.c | 276 | Error reporting, expression parsing, utilities |
| cpp.h | 298 | Common definitions and data structures |
| lexeme.h | 175 | Token code definitions, shared with pass1 |

> The token filter is a **pull-based pipeline** of four stages
> (`filtknr → filtdecl → filtbrace → filtctrl`), not a single `knr.c` module.
> See [FILTERS.md](FILTERS.md) for the pipeline detail and [OUTPUT.md](OUTPUT.md)
> for the guarantees it places on the output stream.

## Command Line

```bash
cpp [options] <source.c>
```

**Options:**
- `-o <base>` - Output base name (produces `<base>.x`; `<base>.i` with `-p`)
- `-I<dir>` - Add user include directory
- `-i<dir>` - System include directory (default: `/usr/include` or `libsrc/include`)
- `-D<name>[=val]` - Define macro (defaults to value `1` if no `=val`)
- `-p` - Also emit a human-readable `<base>.i` (forks `xdump`)
- `-E` - Preprocess and dump readable output to stdout (forks `xdump`)
- `-N` - Suppress line markers (`LINENO`/`NEWLINE`) in the `.x` stream

**Output Files:**
- `<base>.x` - Binary lexeme stream for pass1 compiler (always written)
- `<base>.i` - Human-readable preprocessed source (only with `-p`; rendered by `xdump`)

The `.x` stream is the real output; the `.i` form is produced by decoding `.x`
with `xdump`, not written directly by the preprocessor.

## Architecture

### Processing Pipeline

```
Source File → Character Stream → Lexer → filtknr → filtdecl → filtbrace → filtctrl → Emitter → .x
                    ↑                        └──────────── token filter pipeline ───────────┘
              Include Files
              Macro Expansions
```

The filter pipeline is **pull-based**: the main loop calls `filtctrl()`, which
pulls from `filtbrace()`, which pulls from `filtdecl()`, then `filtknr()`, and
finally the lexer wrapper `lex_get()`. Each stage performs one transformation:

- `filtknr` - K&R to ANSI function-definition conversion (file scope only)
- `filtdecl` - local declaration initializer splitting (`int x = 5;` →
  `int x; x = 5;`); typedef tracking for type-name recognition
- `filtbrace` - brace insertion around single-statement control bodies
  (if/else/while/for/do)
- `filtctrl` - loop lowering (while/for/do → if/goto/label) and
  break/continue → goto resolution

See [FILTERS.md](FILTERS.md) for stage-by-stage detail and [OUTPUT.md](OUTPUT.md)
for the resulting output-language guarantees.

### Program Flow (cpp.c)

```c
main() {
    1. Parse command-line arguments
    2. Create output file: <basename>.x
    3. Add include paths (current directory first, then -I paths)
    4. filterInit()            // Wire up the filtknr→filtdecl→filtbrace→filtctrl chain
    5. Call process(sourcefile)
    6. If -p/-E: fork xdump to render/dump the .i form
}

process(sourcefile) {
    1. pushfile(sourcefile)    // Push source onto textbuf stack
    2. ioinit()                // Prime lexer with first two characters
    3. emitFileStart()         // Emit initial LINENO marker
    4. gettoken(); gettoken(); // Fill cur and next tokens
    5. Loop: filtctrl(&t); emitStructTok(&t) until t.type == E_O_F
    6. Brace-balance checks, then emit final E_O_F token
}
```

---

# Lexeme Stream Format (.x file)

The lexeme stream is a flat sequence of tokens in program order, terminated by a
single `E_O_F` (0x00) byte. It is **raw little-endian binary** — token codes are
the values defined in [`lexeme.h`](lexeme.h) and shared verbatim with pass1, and
multi-byte fields are emitted as raw bytes (not ASCII hex).

> **Authoritative spec:** the complete token-code tables, payload encodings, and
> line-marker rules live in [OUTPUT.md](OUTPUT.md). This section is a summary;
> if the two ever disagree, OUTPUT.md wins.

## Token Encoding Summary

| Token Type | Code | Format |
|------------|-----:|--------|
| Simple | (self) | 1 byte: the token code from `lexeme.h` |
| Symbol (`SYM`) | 20 | code + 1-byte len + name bytes |
| Number (`NUMBER`) | 21 | code + 4-byte LE integer |
| String (`STRING`) | 22 | code + 2-byte LE len + bytes |
| Float (`FNUMBER`) | 23 | code + 4-byte LE IEEE-754 bits |
| Label (`LABEL`) | 112 | code + 1-byte len + name bytes |
| Line marker (`LINENO`) | 116 | code + 2-byte LE line + 1-byte namelen + name |
| Newline (`NEWLINE`) | 117 | 1 byte: "advance current line by 1" |
| Asm string (`ASMSTR`) | 118 | code + 2-byte LE len + bytes |
| Keyword | 128–160 | 1 byte: keyword code from `lexeme.h` |
| EOF (`E_O_F`) | 0 | single `0x00` byte, ends the stream |

Simple tokens cover delimiters (`SEMI`=1, `BEGIN`=2, …), operators
(`PLUS`=40, `ASSIGN`=80, `ARROW`=50, …), and keywords (`INT`=128, `IF`=147, …).
`SIZEOF_KW` is normalized to `SIZEOF` (91) on emission, and `CONST`/`VOLATILE`
are silently dropped. All values are in `lexeme.h`.

## Worked Example

Source code:
```c
int x = 42;
```

Emitted `.x` bytes (line markers omitted for clarity):
```
80            INT        (128)
14 01 78      SYM "x"    (20, len 1, 'x')
50            ASSIGN     (80)
15 2a 00 00 00  NUMBER 42 (21, value 0x0000002a little-endian)
01            SEMI       (1)
00            E_O_F      (0)
```

Note `NUMBER 42` is five raw bytes (`15 2a 00 00 00`) — the tag plus a 4-byte
little-endian value. It is **not** the tag followed by ASCII hex digits. The
real bytes for a lowered `while` loop can be inspected with
`od -An -tx1 <file>.x`; a decoded rendering is in `test/while.expected`.

---

# Preprocessor Directives

## #define

### Object-like Macros

```c
#define NAME value
#define NAME          // Defines as empty (or "1" from -D flag)
```

### Function-like Macros

**Critical:** The `(` must immediately follow the name with NO whitespace:

```c
#define FOO(a,b) a+b     // Function-like: FOO(1,2) -> 1+2
#define BAR (x)          // Object-like: BAR -> (x)
```

### Special Operators

**Stringify (#):** Converts parameter to string literal
```c
#define STR(x) #x
STR(hello)    // -> "hello"
```

**Token Paste (##):** Concatenates adjacent tokens
```c
#define CONCAT(a,b) a##b
CONCAT(foo,bar)   // -> foobar
```

### Line Continuation

```c
#define LONG_MACRO(x) \
    if (x) \
        do_something()
```

## #include

```c
#include <stdio.h>    // System: search sysIncPath first, then -I paths
#include "myheader.h" // User: search -I paths only (current dir first)
```

**Search order for `<file.h>`:**
1. System include path (`-i` flag or default)
2. User include paths (`-I` flags, in order)

**Search order for `"file.h"`:**
1. Current directory (empty path added first)
2. User include paths (`-I` flags, in order)

## Conditional Compilation

```c
#if EXPR
#ifdef NAME        // True if NAME is defined
#ifndef NAME       // True if NAME is NOT defined
#elif EXPR         // Else-if (can have multiple)
#else              // Else (only one allowed)
#endif             // End conditional block
```

**Expression operators supported:**
- Arithmetic: `+`, `-`, `*`, `/`, `%`
- Comparison: `<`, `>`, `<=`, `>=`, `==`, `!=`
- Logical: `&&`, `||`, `!`
- Bitwise: `&`, `|`, `^`, `~`, `<<`, `>>`
- Parentheses: `(`, `)`

**defined() pseudo-function:**
```c
#if defined(DEBUG) && !defined(NDEBUG)
#if defined(__GNUC__)
```

**Undefined identifiers evaluate to 0:**
```c
#if UNDEFINED_MACRO    // Evaluates to #if 0
```

## #undef

```c
#undef NAME    // Removes macro definition (silently succeeds if not found)
```

---

# Data Structures

## struct token (cpp.h:114)

The current and next token during lexing:

```c
struct token {
    token_t type;           // Token type (single byte)
    union {
        long numeric;       // NUMBER: integer value
        float fval;         // FNUMBER: float value
        char *name;         // SYM: identifier string (malloc'd)
        cstring str;        // STRING: counted string (len + data)
    } v;
};
```

**Global instances:**
- `cur` - Current token being processed
- `next` - Lookahead token

## struct macro (cpp.h:141)

Macro definition storage:

```c
struct macro {
    unsigned char parmcount;   // 0 = object-like, >0 = function-like
    char *name;                // Macro name
    char **parms;              // Parameter names array (function-like only)
    char *mactext;             // Replacement text
    struct macro *next;        // Linked list
};
```

**Global list head:** `macros`

## struct textbuf (cpp.h:127)

Unified buffer for files and macro expansions:

```c
struct textbuf {
    int fd;                   // File descriptor (-1 = macro buffer)
    char *name;               // Filename or macro name
    char *storage;            // Buffer data
    short offset;             // Current read position
    short valid;              // Total valid bytes in buffer
    short lineno;             // Current line number
    short saved_column;       // Parent's column (for restoration)
    struct textbuf *prev;     // Stack pointer
};
```

**Stack top:** `tbtop`

## struct cond (cpp.h:152)

Conditional compilation state:

```c
struct cond {
    int flags;
#define C_TRUE      0x01      // Current block is active
#define C_ELSESEEN  0x02      // #else already seen
#define C_TRUESEEN  0x04      // At least one branch was true
    struct cond *next;        // Stack for nesting
};
```

**Stack top:** `cond`

---

# I/O System (io.c)

## Character Stream Interface

The preprocessor maintains a two-character lookahead:

- `curchar` - Current character being processed
- `nextchar` - Next character (lookahead)
- `column` - Column position of curchar
- `lineno` - Current line number

## Textbuf Stack

Files and macro expansions share a unified stack:

```
┌─────────────────┐
│ Macro expansion │ ← tbtop (current)
├─────────────────┤
│ Include file    │
├─────────────────┤
│ Main source     │
└─────────────────┘
```

### pushfile(name)

Opens main source file and pushes onto stack:
1. Open file
2. Allocate textbuf with TBSIZE (1024) buffer
3. Push onto stack
4. Set filename/lineno globals

### insertfile(name, sys)

Opens include file with path search:
1. For `<file>`: Try sysIncPath first
2. Search include paths in order
3. Push onto stack
4. Initialize curchar/nextchar from new file
5. Fatal error if not found

### insertmacro(name, macbuf)

Inserts macro expansion into stream:

**Optimization:** If expansion fits in already-read portion of current buffer:
1. Copy text before current offset
2. Back up offset
3. Update curchar/nextchar

**Otherwise:**
1. Allocate new textbuf with fd=-1
2. Duplicate macro text as storage
3. Push onto stack

### advance()

Core I/O function - advances character stream:

```
1. Move nextchar → curchar
2. If buffer has more data: read nextchar
3. If buffer exhausted and file open: refill buffer
4. If file exhausted or macro empty: pop textbuf
5. Update line/column tracking
6. If curchar == 0 from macro end: goto step 1
```

**State restoration on pop:**
- Restore parent's column position
- Restore parent's line number
- Restore parent's filename
- Read nextchar from parent buffer

## Output Buffer Stack

For loop lowering and other transformations that require out-of-order emission, io.c provides an output buffer stack:

- `outbufPush()` - Start buffering output to a new level
- `outbufWrite()` - Write data to current buffer (spills to temp file if needed)
- `outbufPop()` - Replay buffered content to parent and free

The output stack uses the same `struct textbuf` as input, with `fd=-1` for memory-only buffers that spill to temp files when exceeding `TBSIZE` (512 bytes).

---

# Lexer (lex.c)

## Token Recognition

The lexer (`gettoken()`) recognizes:

1. **Comments** - Stripped before tokenization
   - C-style: `/* ... */`
   - C++ style: `// ...`

2. **Preprocessor directives** - `#` at column 0
   - Dispatched to `doCpp()`
   - Non-column-0 `#` is a token

3. **Identifiers/Keywords**
   - `issym()` extracts identifier
   - `kwlook()` checks keyword tables
   - Keywords become their token type
   - Non-keywords become SYM tokens

4. **Numbers**
   - Decimal: `123`
   - Hexadecimal: `0x1a`, `0X1A`
   - Octal: `0755`
   - Binary: `0b1010`, `0B1010`
   - Character: `'a'`, `'\n'`
   - Float: `3.14`, `1.0e10`

5. **Strings** - `"..."`
   - Escape sequences processed
   - Stored as counted strings

6. **Operators**
   - Single: `+ - * / % & | ^ < > ! ~ ? : = .`
   - Doubled: `++ -- || && == >> <<`
   - With `=`: `+= -= *= /= %= &= |= ^= >= <= != >>= <<=`
   - Arrow: `->`

## Keyword Tables (kw.c)

Compressed tables using a custom encoding:

```
Grammar:
  0xff <token>       - End: return token if string at null
  0xfe <token>       - End or continue: return token or advance
  <char>             - Literal: must match exactly
  <char|0x80> <skip> - Branch: match or skip bytes
```

Example from `cppkw[]`:
```c
'd'|HI, 7, 'e', 'f', 'i', 'n', 'e', 0xff, DEFINE,
//  ↑     └──────────────────────────┘
//  │              "define"
//  └── Skip 7 bytes if 'd' doesn't match
```

## Escape Sequences

Supported in character and string literals:

| Escape | Value | Description |
|--------|-------|-------------|
| `\n` | 0x0a | Newline |
| `\r` | 0x0d | Carriage return |
| `\t` | 0x09 | Tab |
| `\b` | 0x08 | Backspace |
| `\f` | 0x0c | Form feed |
| `\v` | 0x0b | Vertical tab |
| `\e` | 0x1b | Escape (extension) |
| `\\` | 0x5c | Backslash |
| `\'` | 0x27 | Single quote |
| `\"` | 0x22 | Double quote |
| `\0` - `\7` | octal | Octal (up to 3 digits) |
| `\xNN` | hex | Hexadecimal |
| `\BNN` | binary | Binary (extension) |
| `\DNN` | decimal | Decimal (extension) |

## ONELINE Mode

Used for `#if`/`#elif` expression evaluation:

- Enabled via `tflags |= ONELINE`
- Newline translates to `;` token
- Expression parser stops at `;`
- Prevents expressions spanning multiple lines

---

# Macro Expansion (macro.c)

## Expansion Process

1. **Lookup:** `maclookup(name)` searches linked list
2. **Arguments:** For function-like macros, parse `(arg1, arg2, ...)`
   - Track parenthesis nesting
   - Copy string/char literals verbatim
3. **Substitution:** Walk macro text, replace parameters
   - `#param` → `"arg"` (stringify)
   - `##` → removed (tokens concatenated)
   - `param` → `arg` (direct substitution)
4. **Insertion:** `insertmacro()` pushes expansion
5. **Recursion:** Nested macros expanded when inserted text is tokenized

## Argument Parsing

```c
#define FOO(a, b) ...
FOO(x + y, bar(1, 2))
//  └─┬─┘  └───┬───┘
//    a        b
```

- Parentheses tracked for nested calls
- Commas inside parens don't split arguments
- String/char literals copied without processing

## Example Expansion

```c
#define MAX(a, b) ((a) > (b) ? (a) : (b))

MAX(x, y+1)
```

Expansion steps:
1. Parse args: `a = "x"`, `b = "y+1"`
2. Substitute in `((a) > (b) ? (a) : (b))`
3. Result: `((x) > (y+1) ? (x) : (y+1))`
4. Insert into character stream
5. Lexer tokenizes the expansion

---

# Token Filter Pipeline

The token filter sits between the lexer and the emitter, transforming the token
stream before output. It is a **pull-based pipeline of four stages** —
`filtknr → filtdecl → filtbrace → filtctrl` — each a small state machine that
performs one transformation and pulls tokens from the stage above it. The
sections below describe those transformations by effect; for the per-stage
state machines, buffers, and helper API see [FILTERS.md](FILTERS.md).

## K&R to ANSI Conversion

Transforms old-style K&R function definitions to ANSI style:

```c
// K&R style input:
int foo(a, b, c)
int a;
char *b;
long c;
{ ... }

// ANSI style output:
int foo(int a, char *b, long c)
{ ... }
```

Parameters without explicit type declarations default to `int`.

## Brace Insertion (if/else only)

Inserts braces around single-statement if/else bodies:

```c
// Input:
if (x > 0) x = 1;
if (x) foo(); else bar();

// Output:
if (x > 0) { x = 1; }
if (x) { foo(); } else { bar(); }
```

Note: While/for/do loops are handled by loop lowering (see below), not brace insertion.

## Loop Lowering

Transforms while/for/do loops into equivalent if/goto/label sequences. This eliminates
loop constructs entirely, so pass1 only needs to handle `if` and `goto` for control flow.

**WHILE loop:**
```c
// Input:
while (cond) { body }

// Output:
{
    __W1T:
    if (!(cond)) goto __W1B;
    { body }
    goto __W1T;
    __W1B: ;
}
```

**FOR loop:**
```c
// Input:
for (init; cond; incr) { body }

// Output:
{
    init;
    __F1T:
    if (!(cond)) goto __F1B;
    { body }
    __F1C:
    incr;
    goto __F1T;
    __F1B: ;
}
```

**DO-WHILE loop:**
```c
// Input:
do { body } while (cond);

// Output:
{
    __D1T:
    { body }
    __D1C:
    if (cond) goto __D1T;
    __D1B: ;
}
```

**Label naming:**
- `__W<n>T` / `__F<n>T` / `__D<n>T` - Top of loop (condition test)
- `__F<n>C` / `__D<n>C` - Continue target (before increment/condition)
- `__W<n>B` / `__F<n>B` / `__D<n>B` - Break target (after loop)

**Break/Continue:** Resolved to goto statements targeting the appropriate label:
- `break;` → `goto __<L><n>B;`
- `continue;` → `goto __<L><n>T;` (WHILE) or `goto __<L><n>C;` (FOR/DO)

## Local Declaration Initializer Splitting

Splits local variable declarations with initializers into separate declaration and assignment statements:

```c
// Input:
void foo(void) {
    int x = 5;
    char *p = "hello";
}

// Output:
void foo(void) {
    int x;
    x = 5;
    char *p;
    p = "hello";
}
```

This simplification allows pass1 to handle declarations uniformly without tracking initializer expressions during declaration parsing.

## Typedef Tracking

The filter tracks typedef declarations to recognize user-defined type names. When a typedef is encountered:

1. The typedef name is recorded in a lookup table
2. Later occurrences of that identifier are recognized as type tokens
3. This enables proper detection of declarations vs expressions

```c
typedef int error_t;
void gripe(error_t code) { ... }  // error_t recognized as type
```

## Pipeline Stages

Each transformation above is a separate filter stage. Rather than one shared
state machine, every stage keeps its own local state, token buffers, and (where
needed) a context stack for nesting:

| Stage | File | Responsibility | Scope |
|-------|------|----------------|-------|
| `filtknr` | filtknr.c | K&R → ANSI parameter lists | file scope |
| `filtdecl` | filtdecl.c | initializer splitting; typedef tracking | function bodies |
| `filtbrace` | filtbrace.c | brace insertion on control bodies | all |
| `filtctrl` | filtctrl.c | loop lowering; break/continue → goto | all |

The shared helpers (pending buffers, filter stacks, label/goto emission) live in
`filtutil.c`. Per-stage state names and the helper API are documented in
[FILTERS.md](FILTERS.md).

---

# Conditional Compilation

## State Machine

```
                    #if (true)
    ┌─────────────────────────────────────┐
    │                                     ▼
  START ──#if(false)──► FALSE_BLOCK ──#endif──► DONE
    │                       │
    │                       ├──#elif(true)──► TRUE_BLOCK
    │                       │
    │                       └──#else──► TRUE_BLOCK
    │
    └──#if(true)──► TRUE_BLOCK ──#endif──► DONE
                        │
                        ├──#elif──► FALSE_BLOCK (C_TRUESEEN)
                        │
                        └──#else──► FALSE_BLOCK (C_TRUESEEN)
```

## Flag Semantics

| Flag | Meaning |
|------|---------|
| `C_TRUE` | Current block is active (emit tokens) |
| `C_TRUESEEN` | At least one branch was true |
| `C_ELSESEEN` | `#else` already seen (error if another) |

## False Block Handling

When `!(cond->flags & C_TRUE)`:
- Only process conditional directives
- Skip `#define`, `#include`, `#undef`
- Skip all other tokens via `skiptoeol()`
- Nested `#if` blocks tracked correctly

---

# Limits and Constraints

| Constant | Value | Description |
|----------|-------|-------------|
| `TBSIZE` | 512 | Text buffer size (matches Micronix disk block) |
| `STRBUFSIZE` | 256 | String/symbol/identifier buffer |
| `BIGBUFSIZE` | 1024 | Asm blocks and concatenated strings |
| `MAXPARMS` | 10 | Maximum macro parameters |
| `MAXSYMLEN` | 16 | Symbol buffer size (15 chars + null) |
| Identifier | 14 | C-level limit (object file: 15 with `_` prefix) |
| `MAX_TOKENS` | 256 | K&R token buffer (dynamic) |
| `MAX_LOOP_TOKS` | 64 | Loop condition/init/incr buffers (dynamic) |
| `LOOP_STACK_SIZE` | 8 | Nested loop depth |

## Memory Management

To minimize BSS for the Z80 target, large buffers are allocated dynamically on first use:

- **params** - K&R parameter list (linked list, ~28 bytes/param)
- **typedefs** - Typedef name tracking (linked list)
- **tokbuf** - K&R token buffer (allocated on demand)
- **loop_cond/init/incr** - Loop lowering buffers (allocated on demand)
- **decl_toks** - K&R declaration parsing (allocated on demand)

Static BSS reduced from ~18KB to ~2.7KB through dynamic allocation.

---

# Error Codes

| Code | Name | Description |
|------|------|-------------|
| ER_C_NX | 1 | Invalid escape sequence |
| ER_C_BC | 2 | Bad character constant |
| ER_C_CD | 3 | Bad numeric constant |
| ER_C_TL | 4 | Token too long |
| ER_C_MN | 5 | Macro name expected |
| ER_C_CU | 6 | #elif without #if |
| ER_C_ME | 7 | Missing #endif |
| ER_C_ID | 8 | Invalid directive |
| ER_C_BD | 9 | Bad digit |
| ER_C_UT | 10 | Unknown token |
| ER_C_DP | 11 | defined requires identifier |
| ER_W_SYMTRUNC | 12 | Symbol truncated (warning) |

---

# Debug Support

When compiled with `-DDEBUG`, verbose output controlled by `VERBOSE()` macro:

| Flag | Description |
|------|-------------|
| V_IO | Character stream I/O |
| V_TOKEN | Token recognition |
| V_CPP | Preprocessor directives |
| V_MACRO | Macro expansion |
| V_STR | String parsing |
| V_SYM | Symbol recognition |

---

# Known Limitations

1. **Macro text restrictions** - Values typically single tokens (more restrictive than standard cpp)
2. **Function-like detection** - `(` must immediately follow name, no whitespace
3. **No `signed` keyword** - Inherited from compiler limitation
4. **Nested comments** - Not supported (first `*/` closes)

---

# Integration with Compiler

The preprocessor produces:
- `.x` file: binary lexeme stream consumed by pass1 (always written)
- `.i` file: human-readable form for debugging, produced on demand by decoding
  `.x` with `xdump` (`-p` writes it, `-E` dumps it to stdout)

Token encoding designed for:
- Minimal size (raw binary, no ASCII-hex expansion)
- Fast parsing (fixed-format fields, token codes shared via `lexeme.h`)
- Embedded nulls in strings (counted format)

---

# Testing

- `make test` — language-conformance sweep (`langtest`) followed by the
  filter/loop-lowering suite (`test/runtest.sh`).
- `make langtest` — the self-compile contract: every file in `SOURCES` is
  linted against the input language ([INPUT.md](INPUT.md), `validate_src.py`),
  preprocessed with `-DCCC`, and the resulting `.x`/`.i` checked against the
  output language ([OUTPUT.md](OUTPUT.md), `validate_x.py` wire format +
  normalized grammar, `validate_i.py`). Failures leave details in
  `langtest/<file>.err`.
- `make regression` — byte-exact baseline harness (`tests/regress.sh`).
- `test/sweep.sh` (part of `make test`) — input-phase invariance: shifts
  a construct-rich payload through all 512 phases of the input-buffer
  block; output must be byte-identical at every phase.  Catches
  position-dependent lexer/refill bugs.
- `test/runtest.sh -g` regenerates the expected outputs after an intentional
  change to lowering shapes or `xdump` rendering.
