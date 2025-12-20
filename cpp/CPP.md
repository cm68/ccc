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
| cpp.c | 161 | Main entry point, command-line processing |
| lex.c | 1460 | Lexer/tokenizer with embedded CPP directive handling |
| macro.c | 552 | Macro definition, lookup, and expansion |
| io.c | 567 | Unified character stream (files, includes, macros) |
| emit.c | 280 | Token output to .x and .i files |
| kw.c | 276 | Compressed keyword lookup tables |
| util.c | 157 | Error reporting, expression parsing, utilities |
| cpp.h | 245 | Common definitions and data structures |

## Command Line

```bash
cpp [options] <source.c>
```

**Options:**
- `-o <base>` - Output base name (produces `<base>.x` and `<base>.i`)
- `-I<dir>` - Add user include directory
- `-i<dir>` - System include directory (default: `/usr/include` or `libsrc/include`)
- `-D<name>[=val]` - Define macro (defaults to value `1` if no `=val`)
- `-E` - Preprocess only (TODO: not yet implemented)

**Output Files:**
- `<base>.x` - Binary lexeme stream for pass1 compiler
- `<base>.i` - Human-readable preprocessed source

## Architecture

### Processing Pipeline

```
Source File → Character Stream → Lexer → Token Stream → Emitter → .x/.i
                    ↑                         |
              Include Files                   |
              Macro Expansions ←──────────────┘
```

### Program Flow (cpp.c)

```c
main() {
    1. Parse command-line arguments
    2. Create output files: <basename>.x and <basename>.i
    3. Add include paths (current directory first, then -I paths)
    4. Call process(sourcefile)
}

process(sourcefile) {
    1. pushfile(sourcefile)    // Push source onto textbuf stack
    2. ioinit()                // Prime lexer with first two characters
    3. gettoken(); gettoken(); // Fill cur and next tokens
    4. Loop: emitCurToken() and gettoken() until E_O_F
    5. Emit final E_O_F token
}
```

---

# Lexeme Stream Format (.x file)

The lexeme stream is a compact binary format optimized for fast parsing by pass1. All numeric values are encoded as ASCII hexadecimal characters.

## Token Encoding Summary

| Token Type | Prefix | Format | Description |
|------------|--------|--------|-------------|
| Simple | (none) | `<byte>` | Single byte token value |
| Symbol | `5` | `5` + 2-hex-len + bytes | Identifier |
| Number | `9` | `9` + 8-hex-value | Integer constant |
| Float | `b` | `b` + 8-hex-ieee754 | IEEE 754 float bits |
| String | `"` | `"` + 2-hex-len + bytes | String literal |
| Label | `3` | `3` + 2-hex-len + bytes | Statement label |
| Asm | `A` | `A` + 4-hex-len + bytes | Inline assembly block |
| EOF | `\0` | `<0x00>` | End of file |

## Detailed Token Formats

### Simple Tokens (1 byte)

Keywords, operators, and punctuation are emitted as single bytes. The token value is typically the ASCII character or a predefined enum value.

**C Keywords:**
```
Token   Byte  Char    Token     Byte  Char
------  ----  ----    ------    ----  ----
ASM     0x41  'A'     AUTO      0x6f  'o'
BREAK   0x42  'B'     CASE      0x43  'C'
CHAR    0x63  'c'     CONST     0x6b  'k'
CONTINUE 0x4e 'N'     DEFAULT   0x4f  'O'
DO      0x44  'D'     DOUBLE    0x64  'd'
ELSE    0x45  'E'     ENUM      0x65  'e'
EXTERN  0x78  'x'     FLOAT     0x66  'f'
FOR     0x46  'F'     GOTO      0x47  'G'
IF      0x49  'I'     INT       0x69  'i'
LONG    0x6c  'l'     REGISTER  0x72  'r'
RETURN  0x52  'R'     SHORT     0x73  's'
SIZEOF  0x7a  'z'     STATIC    0x70  'p'
STRUCT  0x61  'a'     SWITCH    0x53  'S'
TYPEDEF 0x74  't'     UNION     0x6d  'm'
UNSIGNED 0x75 'u'     VOID      0x76  'v'
VOLATILE 0x34 '4'     WHILE     0x57  'W'
```

**Punctuation (literal ASCII):**
```
{  }  [  ]  (  )  ;  ,  .  :  ?
```

**Operators:**
```
Token    Byte  Char    Description
------   ----  ----    -----------
ASSIGN   0x3d  '='     Assignment
PLUS     0x2b  '+'     Addition
MINUS    0x2d  '-'     Subtraction
STAR     0x2a  '*'     Multiply/pointer
DIV      0x2f  '/'     Division
MOD      0x25  '%'     Modulo
AND      0x26  '&'     Bitwise AND
OR       0x7c  '|'     Bitwise OR
XOR      0x5e  '^'     Bitwise XOR
LT       0x3c  '<'     Less than
GT       0x3e  '>'     Greater than
BANG     0x21  '!'     Logical NOT
TWIDDLE  0x7e  '~'     Bitwise NOT

INCR     0x55  'U'     ++
DECR     0x56  'V'     --
ARROW    0x71  'q'     ->
EQ       0x51  'Q'     ==
NEQ      0x6e  'n'     !=
LE       0x4c  'L'     <=
GE       0x67  'g'     >=
LAND     0x6a  'j'     &&
LOR      0x68  'h'     ||
LSHIFT   0x79  'y'     <<
RSHIFT   0x77  'w'     >>

PLUSEQ   0x50  'P'     +=
SUBEQ    0xdf  (n/a)   -=
MULTEQ   0x54  'T'     *=
DIVEQ    0x32  '2'     /=
MODEQ    0xfe  (n/a)   %=
ANDEQ    0xc6  (n/a)   &=
OREQ     0x31  '1'     |=
XOREQ    0x58  'X'     ^=
LSHIFTEQ 0x30  '0'     <<=
RSHIFTEQ 0x36  '6'     >>=
```

### Symbol Token (SYM = `5`)

Format: `5` + 2-hex-length + symbol-bytes

```
5 LL BB BB BB ...
│ │  └──────────── Symbol characters (LL bytes)
│ └───────────────── Length as 2 hex digits (00-ff)
└─────────────────── Token type marker

Example: identifier "count"
  5 05 c o u n t
  │ │  └────────── "count" (5 bytes)
  │ └───────────── Length = 05 hex
  └─────────────── SYM token

Hex bytes: 35 30 35 63 6f 75 6e 74
          '5''0''5''c''o''u''n''t'
```

### Number Token (NUMBER = `9`)

Format: `9` + 8-hex-value (32-bit unsigned, zero-padded)

```
9 HH HH HH HH
│ └───────────── Value as 8 hex digits
└─────────────── Token type marker

Example: 255
  9 00 00 00 ff
  Hex bytes: 39 30 30 30 30 30 30 66 66

Example: 0x1234
  9 00 00 12 34
  Hex bytes: 39 30 30 30 30 31 32 33 34

Example: 'A' (character literal = 65)
  9 00 00 00 41
  Hex bytes: 39 30 30 30 30 30 30 34 31
```

### Float Token (FNUMBER = `b`)

Format: `b` + 8-hex-ieee754-bits (32-bit IEEE 754 single precision)

```
b HH HH HH HH
│ └───────────── IEEE 754 bits as 8 hex digits
└─────────────── Token type marker

Example: 3.14159 (IEEE 754: 0x40490fd0)
  b 40 49 0f d0
  Hex bytes: 62 34 30 34 39 30 66 64 30

Example: 1.0 (IEEE 754: 0x3f800000)
  b 3f 80 00 00
  Hex bytes: 62 33 66 38 30 30 30 30 30
```

### String Token (STRING = `"`)

Format: `"` + 2-hex-length + string-bytes

**Note:** Strings use a counted format (first byte is length) internally, and can contain embedded null bytes.

```
" LL BB BB BB ...
│ │  └───────────── String content (LL bytes, no null terminator)
│ └────────────────── Length as 2 hex digits
└──────────────────── Token type marker (ASCII 0x22)

Example: "hello"
  " 05 h e l l o
  Hex bytes: 22 30 35 68 65 6c 6c 6f

Example: "a\0b" (string with embedded null)
  " 03 a <NUL> b
  Hex bytes: 22 30 33 61 00 62
```

### Label Token (LABEL = `3`)

Format: `3` + 2-hex-length + label-bytes

```
3 LL BB BB BB ...
│ │  └───────────── Label name (LL bytes)
│ └────────────────── Length as 2 hex digits
└──────────────────── Token type marker

Example: label "loop:"
  3 04 l o o p
  Hex bytes: 33 30 34 6c 6f 6f 70
```

### Asm Token (ASM = `A`)

Format: `A` + 4-hex-length + asm-bytes

Used for inline assembly blocks `asm { ... }`. Uses 4 hex digits for length to support larger assembly blocks.

```
A LL LL BB BB BB ...
│ │    └───────────── Assembly text (LLLL bytes)
│ └───────────────────── Length as 4 hex digits
└─────────────────────── Token type marker

Example: asm { nop }
  A 00 03 n o p
  Hex bytes: 41 30 30 30 33 6e 6f 70
```

### EOF Token

Format: Single null byte `0x00`

```
Hex bytes: 00
```

## Complete Lexeme Stream Example

Source code:
```c
int x = 42;
```

Lexeme stream breakdown:
```
i            INT keyword (0x69)
5 01 x       SYM "x"
=            ASSIGN (0x3d)
9 00 00 00 2a NUMBER 42
;            SEMI (0x3b)
\0           EOF (0x00)
```

Raw hex bytes:
```
69 35 30 31 78 3d 39 30 30 30 30 30 30 32 61 3b 00
```

## Source with Multiple Tokens

Source code:
```c
char *p = "hello";
```

Lexeme stream:
```
c                    CHAR (0x63)
*                    STAR (0x2a)
5 01 p               SYM "p"
=                    ASSIGN (0x3d)
" 05 h e l l o       STRING "hello"
;                    SEMI (0x3b)
\0                   EOF (0x00)
```

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
| `TBSIZE` | 1024 | Text buffer size for files/macros |
| `STRBUFSIZE` | 128 | String/symbol/identifier buffer |
| `MAXPARMS` | 10 | Maximum macro parameters |
| `MAXSYMLEN` | 32 | Maximum symbol length (internal) |
| Identifier | 13 | Practical limit (project constraint) |

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
4. **Anonymous struct/union** - Don't work properly
5. **Nested comments** - Not supported (first `*/` closes)
6. **-E mode** - Not yet implemented

---

# Integration with Compiler

The preprocessor produces:
- `.x` file: Consumed by pass1 (cc1) parser
- `.i` file: Human-readable for debugging

Token encoding designed for:
- Minimal size (compact hex encoding)
- Fast parsing (fixed-format fields)
- Embedded nulls in strings (counted format)
