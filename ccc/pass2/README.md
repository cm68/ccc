# pass2 Architecture

The pass2 code generator translates binary AST input from pass1 into Z80 assembly.

## Streaming Model

The AST is **not** ingested into a complete tree. Instead, processing is
streaming: each expression is parsed, annotated, emitted, and freed before
the next. This keeps memory footprint minimal for the 64KB target.

```
AST stream ──┬── globals (Z) ──→ emit .db/.dw/.ds
             │
             ├── strings (U) ──→ emit .db with quoted/hex data
             │
             ├── inline asm (A) ──→ pass through verbatim
             │
             └── functions (F) ──→ for each statement:
                                      parseExpr() → build tree
                                      annotate() → detect patterns
                                      emitExpr() → generate assembly
                                      freeExpr() → release memory
```

**Parse-time work:**
- Symbol resolution: `SYM` nodes become `REGVAR` (register variable),
  `LOCALVAR` (stack variable via IY/IX+offset), or stay `SYM` (global)
- Type/size computation: `e->size` set from type suffix via `TSIZE()`
- Argument list reversal: `CALL` args built in reverse order via `ARGNODE` chain
- Pattern collapsing: `DEREF[LOCALVAR]` and `+p[REGVAR #ofs]` collapse to `LOCALVAR`

**No intermediate representation** - the expression tree exists only
briefly between parseExpr() and freeExpr().

## Expression Tree

The core data structure is `struct expr`:

```c
struct expr {
    unsigned char op;   /* operator: lexeme.h tokens */
    char size;          /* result size in bytes (1, 2, 4) */
    char type;          /* type suffix from AST ('b' 's' 'l' 'B' 'S' 'L') */
    struct expr *left, *right;
    union { long l; short s; char c; } v;  /* constant value */
    char *sym;          /* symbol name (malloc'd) */
    unsigned char aux;  /* nargs for call, width for bitfield, register */
    short aux2;         /* offset for bitfield, incr amount, label */
    unsigned char demand;   /* (unused - for future scheduling) */
    unsigned char dest;     /* destination register for specials */
    unsigned char spill;    /* (unused - for future scheduling) */
    unsigned char unused;   /* result is unused (expr stmt, void call) */
    unsigned char cond;     /* used as condition (emit flags, not value) */
    unsigned char special;  /* optimization pattern type */
    char offset;            /* IY/IX-relative offset */
    short incr;             /* increment amount for inc/dec specials */
};
```

### Operator Codes (from lexeme.h)

**Primary:**
- `NUMBER` - constant (value in `v`)
- `SYM` - global symbol (name in `sym`)
- `REGVAR` - register variable (register in `aux`: R_B, R_C, R_BC, R_IX)
- `LOCALVAR` - local/stack variable (IY/IX offset in `offset`, register in `aux`)

**Unary:**
- `DEREF` - memory dereference
- `WIDEN` - zero-extend byte to word
- `NARROW` - truncate word to byte
- `SEXT` - sign-extend byte to word
- `TWIDDLE` - bitwise complement (~)
- `BANG` - logical not (!)
- `NEG` - unary minus (negation)

**Inc/Dec:**
- `PREINC` - pre-increment (++x)
- `POSTINC` - post-increment (x++)
- `PREDEC` - pre-decrement (--x)
- `POSTDEC` - post-decrement (x--)

**Binary Arithmetic:**
- `PLUS MINUS STAR DIV MOD` - arithmetic (+, -, *, /, %)

**Binary Bitwise:**
- `AND OR XOR` - bitwise (&, |, ^)
- `LSHIFT RSHIFT` - shifts (<<, >>)

**Comparisons:**
- `LT EQ NEQ` - (<, ==, !=) - pass1 normalizes >, <=, >= to these

**Logical:**
- `LAND LOR` - logical and/or (&&, ||)

**Assignment:**
- `ASSIGN` - simple assignment (=)

**Compound Assignment:**
- `PLUSEQ SUBEQ` - (+=, -=)
- `OREQ ANDEQ XOREQ MODEQ` - (|=, &=, ^=, %=)
- `MULTEQ DIVEQ` - (*=, /=)
- `LSHIFTEQ RSHIFTEQ` - (<<=, >>=)

**Other:**
- `CALL` - function call (nargs in `aux`, args in ARGNODE chain via `right`)
- `QUES` - ternary (TERNBRANCH node holds then/else branches)
- `COMMA` - comma operator
- `BFEXTRACT BFASSIGN` - bitfield access

## Pattern Detection

The `annotate()` function walks the tree detecting optimization patterns,
setting `e->special`:

| Pattern | Code | Description |
|---------|------|-------------|
| SP_SYMOFS | `+p $sym #const` | `ld hl,sym+ofs` |
| SP_SYMOFD | `M[+p $sym #const]` | `ld hl,(sym+ofs)` |
| SP_MSYM | `M $sym` | `ld hl,(sym)` |
| SP_MUL2 | `* expr #pow2` | `add hl,hl` repeated |
| SP_STCONST | `= [target] #const` | `ld (hl),n` or direct store |
| SP_INCR/DECR | `++/-- regvar` | `inc/dec reg` (incr <= 4) |
| SP_INCGLOB | `++/-- $sym` | load/inc/store global word |
| SP_BITTEST | `& M[(ix+ofs)] #pow2` | `bit n,(ix+ofs)` |
| SP_CMPEQ | `== expr #0/1/-1` | inc/dec then test HL |
| SP_CMPV | `cmp Vb #const` | `ld a,const; cp (iy+off)` |
| SP_CMPR | `cmp Rb #const` | `ld a,const; cp reg` |
| SP_CMPHL | `cmp Mb[addr] simple` | `ld a,(hl); cp operand` |
| SP_ADDBC | `+p M[Rp bc] #const` | `ld hl,const; add hl,bc` |
| SP_SIGN | `>= M$sym #0` | `bit 7,(sym+n)` |
| SP_SIGNREG | `>= M[Rs bc] #0` | `bit 7,b` |

## Symbol Table

Locals discovered during function parsing are stored in `locals[]` array:

```c
struct sym {
    char name[14];
    char type;      /* type suffix */
    char reg;       /* 0=stack, R_B/R_C/R_BC/R_IX for regvar */
    char off;       /* IY offset for stack vars */
};
```

During expression parsing, `SYM` references are resolved:
- If local with reg!=0 → `REGVAR` node (register variable)
- If local with reg=0 → `LOCALVAR` node (stack variable via IY+offset)
- Otherwise → `SYM` node (global symbol)

## Register Model

- `R_HL` (6) - primary accumulator for word results
- `R_DE` (5) - secondary (left operand saved here before right)
- `R_A` (7) - byte accumulator
- `R_BC` (3) - register variable (word)
- `R_B/R_C` (1/2) - register variable (byte)
- `R_IX` (4) - struct pointer register variable
- `R_IY` (8) - frame pointer (IY+offset for locals)
- `R_IYO/R_IXO` (11/12) - indexed addressing mode (iy+d)/(ix+d)
- `R_TOS` (13) - push to stack (function arguments)

## Calling Convention

- Arguments pushed right-to-left (parser builds arg list in reverse)
- Word results in HL
- Byte results in A
- Long results in lR (memory location, with HL:HL' for some ops)
- Caller cleans stack after call returns
- Frame allocation via `framealloc` helper, cleanup via `framefree`

## Statement Processing

Statements are processed by `dumpStmt()`:

| Code | Statement | Handling |
|------|-----------|----------|
| B | block | Parse decls (add to locals), process stmts |
| I | if | Parse cond, emit conditional jump, then/else bodies |
| E | expression | Parse expr, emit, mark `unused=1` |
| R | return | Parse value, emit, jump to `framefree` or `ret` |
| L | label | Emit label |
| G | goto | Emit `jp label` |
| S | switch | Emit expr to A, jump to table, process cases |
| C | case | Record value/label in switch context |
| O | default | Record default label in switch context |
| ; | empty | Nothing |
| A | inline asm | Skip (length-prefixed) |
| U | string | Parse inline string literal |

## Condition Code Generation

For `if` statements, conditions use short-circuit evaluation:

- `cond` flag propagates through LAND/LOR trees
- `aux2` encodes jump target: positive = FALSE jump to `no{n}`, negative = TRUE jump to `ht{n}`
- Comparisons emit conditional jumps directly when `cond=1`
- LAND: both sides must be true, FALSE jumps to `no{label}`
- LOR: either side true, TRUE jumps to `ht{label}`, FALSE falls through

## File Organization

| File | Purpose |
|------|---------|
| cc2.h | Shared definitions, struct expr, struct sym |
| cc2.c | Main, symbol table management |
| astio.c | Binary AST reading (read1, read2, read4, readName) |
| parseast.c | Expression/statement parsing, global/function handling |
| codegen.c | annotate() pattern detection, helper functions |
| emit.c | emit(), emitLabel(), comment() output formatting |
| emitexpr.c | emitExpr() main expression emission |
| emitcmp.c | emitCompare(), emitCondJmp() comparison emission |
| emitincdec.c | emitPreIncDec(), emitPostInc() inc/dec emission |
| emitops.c | emitCmpArith(), emitCmpShift(), emitCmpMulDiv() compound ops |
| emitpat.c | Table-driven helpers: emitBOp(), emitWBit(), emitLLoad(), etc. |
| pattern.c | Pattern string builder for -p debug mode |

## Output Format

Assembly output uses custom format specifiers in `emit()`:
- `%o` - signed offset with explicit sign: `+5` or `-3`
- `%r` - register name with optional offset: `bc` or `(iy+5)`
- `%d` - decimal integer
- `%s` - string
- `%c` - character

Comments include expression structure:
```asm
; +s [                ; operator, type
;   $foo              ; symbol foo
;   #s 10             ; const 10
; ]
```

## Long (32-bit) Support

Long values use memory temporaries `lL` and `lR`:
- `emitLLoad()` - load 4 bytes from (HL) to lL or lR
- `emitLStore()` - store 4 bytes from lL to (HL)
- `emitLImm()` - load immediate to lL or lR
- Runtime helpers: `ladd`, `lsub`, `land`, `lor`, `lxor`, `lneg`, `lcom`, `lcmp`
- Shift helpers: `lshl`, `lashr`, `lshr`

## Switch Statement Implementation

Switch uses a runtime `switch` helper with inline jump table:
```asm
    ld a,l              ; expression value to A
    ld hl,sw{n}_{fn}    ; table address
    jp switch           ; runtime dispatch
...
sw{n}_{fn}:
    .db {ncases}        ; case count
    .db {val0}          ; case value
    .dw swc{lbl}_{fn}   ; case label
    ...
    .dw {default/end}   ; default or end label
```
