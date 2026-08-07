# Ritchie C Compiler Intermediate Format

This document describes the command-line usage for the pass1 (c0) and pass2 (c1)
compilers, and the binary intermediate format used between them.

## Command-Line Usage

### Pass 1 (c0) - Front-End Compiler

```
c0 [-u] source.x temp1 temp2 [options...]
```

**Required Arguments:**
- `source.x` - Pre-tokenized binary input from cpp
- `temp1` - Output file for intermediate code
- `temp2` - Output file for string literals (read by c1 after temp1)

**Options:**
| Flag | Description |
|------|-------------|
| `-u` | Treat `char` as unsigned (must be first argument) |
| `-P` | Enable profiling |
| `-V` | Overlays (default, no-op) |
| `-w` or `-W` | Suppress warning messages |

**Example:**
```
c0 -u prog.x /tmp/c0a.tmp /tmp/c0b.tmp -P
```

### Pass 2 (c1) - Back-End Code Generator

```
c1 temp1 temp2 output.s
```

**Arguments:**
- `temp1` - Input intermediate code from pass1
- `temp2` - Input string data from pass1 (use "-" to skip)
- `output.s` - Output Z80 assembly file

**Example:**
```
c1 /tmp/c0a.tmp /tmp/c0b.tmp prog.s
```

## Intermediate Format Overview

The intermediate format is a binary stream. Pass1 writes two files:
- **temp1**: Main intermediate code (control flow, expressions, declarations)
- **temp2**: String literal data (processed after temp1)

Both files use the same encoding format.

## Binary Encoding

### Output Encoding (c04.c `outcode()`)

The `outcode(format, ...)` function encodes data according to format characters:

| Format | Encoding | Description |
|--------|----------|-------------|
| `B` | byte, 0xFE | Opcode byte followed by sync marker 0376 (0xFE) |
| `N` | lo, hi | 16-bit word, little-endian |
| `S` | `_` + chars + NUL | Symbol name with underscore prefix |
| `F` | chars + NUL | Float constant string (max 1000 chars) |
| `1` | 0x01, 0x00 | Constant 1 as 16-bit word |
| `0` | 0x00, 0x00 | Constant 0 as 16-bit word |

### Input Decoding (c11.c `geti()`, `outname()`)

```c
geti() {
    i = getchar() & 0xff;           // Low byte
    i |= (getchar() & 0xff) << 8;   // High byte
    return(i);
}
```

Opcodes are validated: `(op & 0177400) == 0177000` (high byte must be 0xFE).

## Opcode Reference

All opcodes use `B` format: one byte value followed by 0xFE marker.

### Section Directives

| Opcode | Value | Format | Description |
|--------|-------|--------|-------------|
| `EOFC` | 0 | `B` | End of file marker |
| `PROG` | 202 | `B` | Switch to .text section |
| `DATA` | 203 | `B` | Switch to .data section |
| `BSS` | 204 | `B` | Switch to .bss section |
| `EVEN` | 210 | `B` | Align to word boundary (no-op on Z80) |

### Symbol Definitions

| Opcode | Value | Format | Description |
|--------|-------|--------|-------------|
| `SYMDEF` | 207 | `B` `S` | Define global symbol |
| `CSPACE` | 205 | `B` `S` `N` | Allocate common/bss space |
| `SSPACE` | 206 | `B` `N` | Allocate static space (bytes) |

### Function Structure

| Opcode | Value | Format | Description |
|--------|-------|--------|-------------|
| `SAVE` | 208 | `B` | Function prologue (push IY, set frame) |
| `RETRN` | 209 | `B` | Function epilogue (restore IY, ret) |
| `SETSTK` | 219 | `B` `N` | Allocate local stack space (bytes) |
| `SETREG` | 105 | `B` `N` | Set available register count |
| `PROFIL` | 212 | `B` `N` `S` | Profiling call |

### Labels and Branches

| Opcode | Value | Format | Description |
|--------|-------|--------|-------------|
| `LABEL` | 112 | `B` `N` | Numeric label (L###:) |
| `NLABEL` | 113 | `B` `S` | Named label |
| `RLABEL` | 114 | `B` `S` | Return label (function entry) |
| `BRANCH` | 111 | `B` `N` | Unconditional jump to label |

### Data Initialization

| Opcode | Value | Format | Description |
|--------|-------|--------|-------------|
| `SINIT` | 220 | `B` `N` | Word constant (.dw) |
| `BDATA` | 200 | `B` (N N)* | Byte data sequence |

**BDATA format:** After the opcode, pairs of 16-bit values:
- First value: 1 = more bytes follow, 0 = end
- Second value: byte value (when first == 1)

```
BDATA <1> <byte1> <1> <byte2> ... <0> <ignored>
```

### Debug Information

| Opcode | Value | Format | Description |
|--------|-------|--------|-------------|
| `SNAME` | 215 | `B` `S` `N` | Static name → label mapping |
| `ANAME` | 217 | `B` `S` `N` | Auto name → stack offset |
| `RNAME` | 216 | `B` `S` `N` | Register name → register number |

### Control Flow

| Opcode | Value | Format | Description |
|--------|-------|--------|-------------|
| `SWIT` | 213 | `B` `N` `N` (N N)* | Switch table |
| `CBRANCH` | 103 | `B` `N` `N` `N` + tree | Conditional branch |
| `C3BRANCH` | 222 | `B` `N` `N` `N` `N` + tree | Three-way branch |
| `EXPR` | 214 | `B` `N` + tree | Expression for effect |
| `ASSEM` | 223 | `B` `S` | Inline assembly string |

**SWIT format:**
```
SWIT <default-label> <line-number> <label1> <value1> <label2> <value2> ... <0>
```

**CBRANCH format:**
```
CBRANCH <target-label> <condition> <line-number>
```
Followed by expression tree. Condition: 0 = branch if false, 1 = branch if true.

**EXPR format:**
```
EXPR <line-number>
```
Followed by expression tree.

## Expression Tree Encoding

Expression trees are built using a stack machine. Leaf nodes push values,
operators pop operands and push results.

### Leaf Nodes

| Opcode | Value | Format | Description |
|--------|-------|--------|-------------|
| `NAME` | 20 | `B` `N` `N` [`N`\|`S`] | Variable reference |
| `CON` | 21 | `B` `N` `N` | Integer constant |
| `LCON` | 25 | `B` `N` `N` `N` | Long constant |
| `FCON` | 23 | `B` `N` `F` | Float constant |
| `NULLOP` | 218 | `B` | Null operand (empty tree) |

**NAME format:**
```
NAME <class> <type> <location>
```
- class = EXTERN (12): location is symbol name (S format)
- class = STATIC (13): location is label number (N format)
- class = AUTO (11): location is stack offset (N format)
- class = REG (14): location is register number (N format)

**LCON format:**
```
LCON <type> <high-word> <low-word>
```

### Operators

All operators use format: `B` `N` (opcode + type)

**Unary Operators:**
| Opcode | Value | Description |
|--------|-------|-------------|
| `STAR` | 36 | Dereference (*p) |
| `AMPER` | 35 | Address-of (&x) |
| `NEG` | 37 | Unary minus (-x) |
| `COMPL` | 38 | Bitwise complement (~x) |
| `EXCLA` | 34 | Logical not (!x) |
| `INCBEF` | 30 | Pre-increment (++x) |
| `DECBEF` | 31 | Pre-decrement (--x) |
| `INCAFT` | 32 | Post-increment (x++) |
| `DECAFT` | 33 | Post-decrement (x--) |

**Binary Operators:**
| Opcode | Value | Description |
|--------|-------|-------------|
| `PLUS` | 40 | Addition |
| `MINUS` | 41 | Subtraction |
| `TIMES` | 42 | Multiplication |
| `DIVIDE` | 43 | Division |
| `MOD` | 44 | Modulo |
| `LSHIFT` | 46 | Left shift |
| `RSHIFT` | 45 | Right shift |
| `AND` | 47 | Bitwise AND |
| `OR` | 48 | Bitwise OR |
| `EXOR` | 49 | Bitwise XOR |
| `LOGAND` | 53 | Logical AND |
| `LOGOR` | 54 | Logical OR |

**Comparison Operators:**
| Opcode | Value | Description |
|--------|-------|-------------|
| `EQUAL` | 60 | Equal (==) |
| `NEQUAL` | 61 | Not equal (!=) |
| `LESS` | 63 | Less than (<) signed |
| `LESSEQ` | 62 | Less or equal (<=) signed |
| `GREAT` | 65 | Greater than (>) signed |
| `GREATEQ` | 64 | Greater or equal (>=) signed |
| `LESSP` | 67 | Less than (<) unsigned |
| `LESSEQP` | 66 | Less or equal (<=) unsigned |
| `GREATP` | 69 | Greater than (>) unsigned |
| `GREATQP` | 68 | Greater or equal (>=) unsigned |

**Assignment Operators:**
| Opcode | Value | Description |
|--------|-------|-------------|
| `ASSIGN` | 80 | Simple assignment |
| `ASPLUS` | 70 | Add-assign (+=) |
| `ASMINUS` | 71 | Subtract-assign (-=) |
| `ASTIMES` | 72 | Multiply-assign (*=) |
| `ASDIV` | 73 | Divide-assign (/=) |
| `ASMOD` | 74 | Modulo-assign (%=) |
| `ASLSH` | 76 | Left-shift-assign (<<=) |
| `ASRSH` | 75 | Right-shift-assign (>>=) |
| `ASSAND` | 77 | AND-assign (&=) |
| `ASOR` | 78 | OR-assign (\|=) |
| `ASXOR` | 79 | XOR-assign (^=) |

**Type Conversion Operators:**
| Opcode | Value | Description |
|--------|-------|-------------|
| `ITOF` | 51 | Int to float |
| `FTOI` | 52 | Float to int |
| `ITOL` | 58 | Int to long |
| `LTOI` | 59 | Long to int |
| `LTOF` | 57 | Long to float |
| `FTOL` | 56 | Float to long |
| `ITOP` | 13 | Int to pointer |
| `PTOI` | 14 | Pointer to int |
| `ITOC` | 109 | Int to char |

**Special Operators:**
| Opcode | Value | Description |
|--------|-------|-------------|
| `CALL` | 100 | Function call |
| `QUEST` | 90 | Ternary (?:) |
| `SEQNC` | 97 | Comma operator |
| `COMMA` | 9 | Argument separator |
| `RFORCE` | 110 | Force to return register |
| `FSEL` | 10 | Bit-field select |
| `STRASG` | 115 | Structure assignment |

### Field Select (FSEL)

```
FSEL <type> <tree> <bitoffset> <fieldlength>
```

### Structure Assignment (STRASG)

```
STRASG <type> <size-in-bytes>
```
Tree on stack contains assignment expression.

## Type Encoding

Types are encoded as 16-bit integers:

**Base Types:**
| Value | Type |
|-------|------|
| 0 | int |
| 1 | char |
| 2 | float |
| 3 | double |
| 4 | struct |
| 6 | long |
| 7 | unsigned int |
| 8 | unsigned char |
| 9 | unsigned long |
| 10 | void |

**Type Modifiers (OR'd with base):**
| Value | Modifier |
|-------|----------|
| 020 (16) | Pointer |
| 040 (32) | Function |
| 060 (48) | Array |

Modifiers stack: `int **p` = 020 | 020 | 0 = 040 (PTR to PTR to INT)

## Storage Classes

| Value | Class | Description |
|-------|-------|-------------|
| 11 | AUTO | Local variable (stack) |
| 12 | EXTERN | External symbol |
| 13 | STATIC | Static variable |
| 14 | REG | Register variable |
| 16 | ARG | Function argument |

## Example with Hex Dump

C source:
```c
int x;
int main() {
    x = 5;
    return x;
}
```

### Raw Hex Dump (temp1)

```
00000000: cf fe 5f 6d 61 69 6e 00  ca fe 72 fe 5f 6d 61 69  .._main...r._mai
00000010: 6e 00 d0 fe 69 fe 05 00  6f fe 01 00 70 fe 02 00  n...i...o...p...
00000020: 14 fe 0c 00 00 00 5f 78  00 15 fe 00 00 05 00 50  ......_x.......P
00000030: fe 00 00 d6 fe 03 00 14  fe 0c 00 00 00 5f 78 00  ............._x.
00000040: 6e fe 00 00 d6 fe 04 00  6f fe 03 00 70 fe 03 00  n.......o...p...
00000050: d1 fe 70 fe 01 00 db fe  00 00 6f fe 02 00 cd fe  ..p.......o.....
00000060: 5f 78 00 02 00 00 fe                              _x.....
```

### Annotated Breakdown

```
Offset  Bytes                   Decoded
------  ----------------------  ----------------------------------
0x00    cf fe                   SYMDEF (207)
0x02    5f 6d 61 69 6e 00       "_main\0"
0x08    ca fe                   PROG (202) - switch to .text
0x0a    72 fe                   RLABEL (114)
0x0c    5f 6d 61 69 6e 00       "_main\0"
0x12    d0 fe                   SAVE (208) - function prologue
0x14    69 fe                   SETREG (105)
0x16    05 00                   N=5 (register count)
0x18    6f fe                   BRANCH (111)
0x1a    01 00                   N=1 (to label 1)
0x1c    70 fe                   LABEL (112)
0x1e    02 00                   N=2 (label 2: loop entry)

        --- Expression: x = 5 ---
0x20    14 fe                   NAME (20)
0x22    0c 00                   N=12 (EXTERN class)
0x24    00 00                   N=0 (INT type)
0x26    5f 78 00                "_x\0"
0x29    15 fe                   CON (21)
0x2b    00 00                   N=0 (INT type)
0x2d    05 00                   N=5 (value)
0x2f    50 fe                   ASSIGN (80)
0x31    00 00                   N=0 (INT type)
0x33    d6 fe                   EXPR (214)
0x35    03 00                   N=3 (line number)

        --- Expression: return x ---
0x37    14 fe                   NAME (20)
0x39    0c 00                   N=12 (EXTERN class)
0x3b    00 00                   N=0 (INT type)
0x3d    5f 78 00                "_x\0"
0x40    6e fe                   RFORCE (110)
0x42    00 00                   N=0 (INT type)
0x44    d6 fe                   EXPR (214)
0x46    04 00                   N=4 (line number)

        --- Control flow ---
0x48    6f fe                   BRANCH (111)
0x4a    03 00                   N=3 (to label 3)
0x4c    70 fe                   LABEL (112)
0x4e    03 00                   N=3 (label 3: return point)
0x50    d1 fe                   RETRN (209) - function epilogue
0x52    70 fe                   LABEL (112)
0x54    01 00                   N=1 (label 1: stack setup)
0x56    db fe                   SETSTK (219)
0x58    00 00                   N=0 (0 bytes locals)
0x5a    6f fe                   BRANCH (111)
0x5c    02 00                   N=2 (to label 2)

        --- BSS allocation ---
0x5e    cd fe                   CSPACE (205)
0x60    5f 78 00                "_x\0"
0x63    02 00                   N=2 (2 bytes)
0x65    00 fe                   EOFC (0) - end of file
```

### Pretty-Printed Form

The `ppic` utility decodes the binary to human-readable form:

```
SYMDEF     _main
PROG
RLABEL     _main
SAVE
SETREG     5
BRANCH     1
LABEL      2
NAME       EXTERN int _x
CON        int 5
ASSIGN     int
EXPR       line=3
NAME       EXTERN int _x
RFORCE     int
EXPR       line=4
BRANCH     3
LABEL      3
RETRN
LABEL      1
SETSTK     0
BRANCH     2
CSPACE     _x 2
EOFC
```

### Expression Tree Stack Trace

For the statement `x = 5`:
```
Stack operation          Stack contents (bottom to top)
--------------------     ------------------------------
NAME EXTERN int _x       [_x]
CON int 5                [_x, 5]
ASSIGN int               [(_x = 5)]
EXPR                     [] (tree evaluated, code emitted)
```

For the statement `return x`:
```
Stack operation          Stack contents
--------------------     ------------------------------
NAME EXTERN int _x       [_x]
RFORCE int               [(rforce _x)]
EXPR                     [] (tree evaluated, result in HL)
```

### Generated Z80 Assembly (c1 output)

```asm
.globl	_main
.text
_main:
	push	iy
	ld	iy,0
	add	iy,sp
	jp	L1
L2:	ld	hl,5
	ld	(_x),hl
	ld	hl,(_x)
	jp	L3
L3:	ld	sp,iy
	pop	iy
	ret
L1:	jp	L2
.bss
_x:	.ds 2
.data
```

**Code walkthrough:**

| Assembly | Source | Intermediate |
|----------|--------|--------------|
| `push iy` / `ld iy,0` / `add iy,sp` | function entry | SAVE |
| `jp L1` | jump to stack setup | BRANCH 1 |
| `L2:` | function body start | LABEL 2 |
| `ld hl,5` | load constant 5 | CON int 5 |
| `ld (_x),hl` | store to x | ASSIGN (NAME _x) |
| `ld hl,(_x)` | load x for return | NAME _x + RFORCE |
| `jp L3` | jump to epilogue | BRANCH 3 |
| `L3:` | return point | LABEL 3 |
| `ld sp,iy` / `pop iy` / `ret` | function exit | RETRN |
| `L1:` | stack setup point | LABEL 1 |
| `jp L2` | enter function body | BRANCH 2 (after SETSTK 0) |
| `_x: .ds 2` | allocate global | CSPACE _x 2 |

## Example: While Loop and Function Call

C source:
```c
int sum;

int add(int a, int b) {
    return a + b;
}

int main() {
    int i;
    i = 0;
    while (i < 10) {
        sum = add(sum, i);
        i = i + 1;
    }
    return sum;
}
```

### Pretty-Printed Intermediate

```
=== Function: add ===
SYMDEF     _add
PROG
RLABEL     _add
SAVE
ANAME      _a offset=4           ; parameter a at IY+4
ANAME      _b offset=6           ; parameter b at IY+6
SETREG     5
BRANCH     1
LABEL      2
NAME       AUTO int offset=4     ; push a
NAME       AUTO int offset=6     ; push b
PLUS       int                   ; a + b
RFORCE     int                   ; move to return register
EXPR       line=4
BRANCH     3
LABEL      3
RETRN
LABEL      1
SETSTK     0
BRANCH     2

=== Function: main ===
SYMDEF     _main
PROG
RLABEL     _main
SAVE
SETREG     5
BRANCH     4
LABEL      5
ANAME      _i offset=-10         ; local i at IY-10

--- i = 0 ---
NAME       AUTO int offset=-10   ; push &i
CON        int 0                 ; push 0
ASSIGN     int                   ; i = 0
EXPR       line=9

--- while (i < 10) ---
LABEL      7                     ; loop top
NAME       AUTO int offset=-10   ; push i
CON        int 10                ; push 10
LESS       int                   ; i < 10
CBRANCH    L8 cond=0 line=10     ; if false, exit loop

--- sum = add(sum, i) ---
NAME       EXTERN int _sum       ; push &sum (lvalue for assign)
NAME       EXTERN int() _add     ; push function address
NAME       EXTERN int _sum       ; push sum (arg 1)
NAME       AUTO int offset=-10   ; push i (arg 2)
COMMA      int                   ; combine args
CALL       int                   ; call add(sum, i)
ASSIGN     int                   ; sum = result
EXPR       line=11

--- i = i + 1 ---
NAME       AUTO int offset=-10   ; push &i
NAME       AUTO int offset=-10   ; push i
CON        int 1                 ; push 1
PLUS       int                   ; i + 1
ASSIGN     int                   ; i = result
EXPR       line=12
BRANCH     7                     ; back to loop top

--- loop exit ---
LABEL      8

--- return sum ---
NAME       EXTERN int _sum       ; push sum
RFORCE     int                   ; to return register
EXPR       line=14
BRANCH     6
LABEL      6
RETRN
LABEL      4
SETSTK     2                     ; allocate 2 bytes for i
BRANCH     5

=== BSS ===
CSPACE     _sum 2
EOFC
```

### Hex Dump with Annotations

```
=== add function ===
0x00  cf fe 5f 61 64 64 00        SYMDEF "_add"
0x07  ca fe                       PROG
0x09  72 fe 5f 61 64 64 00        RLABEL "_add"
0x10  d0 fe                       SAVE
0x12  d9 fe 5f 61 00 04 00        ANAME "_a" 4
0x19  d9 fe 5f 62 00 06 00        ANAME "_b" 6
0x20  69 fe 05 00                 SETREG 5
0x24  6f fe 01 00                 BRANCH 1
0x28  70 fe 02 00                 LABEL 2
0x2c  14 fe 0b 00 00 00 04 00     NAME AUTO int 4
0x34  14 fe 0b 00 00 00 06 00     NAME AUTO int 6
0x3c  28 fe 00 00                 PLUS int
0x40  6e fe 00 00                 RFORCE int
0x44  d6 fe 04 00                 EXPR line=4
...

=== CBRANCH example (while condition) ===
0x9c  70 fe 07 00                 LABEL 7 (loop top)
0xa0  14 fe 0b 00 00 00 f6 ff     NAME AUTO int -10 (i)
0xa8  15 fe 00 00 0a 00           CON int 10
0xae  3f fe 00 00                 LESS int
0xb2  67 fe 08 00 00 00 0a 00     CBRANCH label=8 cond=0 line=10

=== Function call (add(sum, i)) ===
0xba  14 fe 0c 00 00 00           NAME EXTERN int
0xc0  5f 73 75 6d 00              "_sum"
0xc5  14 fe 0c 00 20 00           NAME EXTERN int() (type=0x20=FUNC)
0xcb  5f 61 64 64 00              "_add"
0xd0  14 fe 0c 00 00 00           NAME EXTERN int
0xd6  5f 73 75 6d 00              "_sum"
0xdb  14 fe 0b 00 00 00 f6 ff     NAME AUTO int -10 (i)
0xe3  09 fe 00 00                 COMMA int (combine args)
0xe7  64 fe 00 00                 CALL int
0xeb  50 fe 00 00                 ASSIGN int
0xef  d6 fe 0b 00                 EXPR line=11
```

### Key Observations

**Function Parameters:** Parameters are accessed via positive offsets from IY
(frame pointer). `a` is at IY+4, `b` is at IY+6.

**Local Variables:** Locals use negative offsets. `i` is at IY-10 (shown as
`0xfff6` = -10 in two's complement).

**While Loop Pattern:**
```
LABEL <top>           ; loop entry point
<condition expr>      ; evaluate condition
CBRANCH <exit> 0      ; branch to exit if false
<body statements>
BRANCH <top>          ; jump back to top
LABEL <exit>          ; loop exit point
```

**Function Call Pattern:**
```
NAME <func>           ; push function address (type includes FUNC=040)
<arg1 expr>           ; evaluate first argument
<arg2 expr>           ; evaluate second argument
COMMA                 ; combine arguments (right-to-left)
CALL                  ; perform call, result on stack
```

**CBRANCH Encoding:**
```
CBRANCH <label> <cond> <line>
```
- `cond=0`: branch if expression is false (used for while/if)
- `cond=1`: branch if expression is true

### Generated Z80 Assembly

```asm
.globl	_add
.text
_add:
	push	iy
	ld	iy,0
	add	iy,sp
	jp	L1
L2:	ld	l,(iy+4)        ; load a
	ld	h,(iy+5)
	ld	de,(iy+6)       ; load b into DE
	add	hl,de           ; a + b
	jp	L3
L3:	ld	sp,iy
	pop	iy
	ret
L1:	jp	L2

.globl	_main
.text
_main:
	push	iy
	ld	iy,0
	add	iy,sp
	jp	L4
L5:                             ; function body
	ld	(iy-10),0       ; i = 0
L7:                             ; while loop top
	ld	l,(iy-10)       ; load i
	ld	h,(iy-9)
	ld	de,10           ; load 10
	or	a
	sbc	hl,de           ; compare i - 10
	jp	p,L8            ; if i >= 10, exit loop
	; ... loop body: call add, increment i ...
	jp	L7              ; back to loop top
L8:                             ; loop exit
	ld	hl,(_sum)       ; return sum
	jp	L6
L6:	ld	sp,iy
	pop	iy
	ret
L4:	dec	sp              ; SETSTK 2
	dec	sp
	jp	L5
.bss
_sum:	.ds 2
```

## File Structure Summary

```
temp1:
    [section directives]
    [global definitions]
    [function definitions]
        SYMDEF
        PROG
        RLABEL
        SAVE
        SETSTK
        [statements as EXPR/CBRANCH/BRANCH/LABEL]
        RETRN
    ...
    EOFC

temp2:
    [BDATA blocks for string literals]
    EOFC
```
