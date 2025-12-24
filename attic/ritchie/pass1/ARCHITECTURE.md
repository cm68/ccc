# 2.11BSD C Compiler Architecture

This is the **Portable C Compiler (PCC)** for the PDP-11, a classic **two-pass compiler**.

## Table of Contents

1. [Architecture](#architecture)
2. [Source Files](#source-files)
3. [Compilation Flow](#compilation-flow)
4. [Key Design Points](#key-design-points)
5. [Front-End Parser (c0)](#front-end-parser-c0)
   - [Lexer](#lexer-symbol-in-c00c)
   - [Expression Parser](#expression-parser-tree-in-c00c)
   - [Tree Building](#tree-building-build-in-c01c)
   - [Statement Parser](#statement-parser-statement-in-c02c)
   - [Control Flow Compilation](#control-flow-compilation)
   - [External Definitions](#external-definitions-extdef-in-c02c)
   - [Symbol Table](#symbol-table-lookup-in-c00c)
   - [Memory Layout](#memory-layout)
   - [Intermediate Code Output](#intermediate-code-output)
   - [Parser Data Flow](#parser-data-flow)
6. [Optable Pattern Matching](#optable-pattern-matching)
   - [Pattern Syntax](#pattern-syntax)
   - [Operand Degree Codes](#operand-degree-complexity-codes)
   - [Type Modifier Flags](#type-modifier-flags)
   - [Assembly Template Macros](#assembly-template-macros)
   - [Code Tables](#code-tables)
   - [Pattern Matching Flow](#pattern-matching-flow)
   - [Pattern Reuse](#pattern-reuse)
7. [Expression Optimizer (c12.c)](#expression-optimizer-c12c)
   - [Constant Folding](#constant-folding)
   - [Algebraic Simplifications](#algebraic-simplifications)
   - [PDP-11 Specific Transformations](#pdp-11-specific-transformations)
   - [Strength Reduction](#strength-reduction)
   - [Commutative Reordering](#commutative-reordering-acommute)
   - [Distributive Factoring](#distributive-factoring-distrib)
   - [Address Mode Collapsing](#address-mode-collapsing)
   - [Optimizer Flow](#optimizer-flow)
8. [Code Generator (c1)](#code-generator-c1)
   - [Intermediate Code Reader](#intermediate-code-reader-getree-in-c11c)
   - [Expression Compiler](#expression-compiler-rcexpr-in-c10c)
   - [Pattern Matcher](#pattern-matcher-match-in-c10c)
   - [Degree Calculation](#degree-calculation-dcalc-in-c11c)
   - [Code Emission](#code-emission-cexpr-in-c10c)
   - [Address Output](#address-output-pname-in-c11c)
   - [Expression Reordering](#expression-reordering-reorder-sreorder)
   - [Conditional Branch](#conditional-branch-cbranch-in-c11c)
   - [Switch Statement](#switch-statement-pswitch-in-c11c)
   - [Register Allocation](#register-allocation)
   - [Code Generation Flow](#code-generation-flow)
9. [Type Encoding](#type-encoding)
10. [Symbol Table Structure](#symbol-table-structure)
11. [Expression Tree Node](#expression-tree-node)
12. [Memory Constraints](#memory-constraints)

## Architecture

| Pass | Executable | Purpose |
|------|------------|---------|
| **c0** | Front-end | Lexing, parsing, type checking, intermediate code |
| **c1** | Back-end | Pattern matching, optimization, PDP-11 assembly output |

## Source Files

### Pass 1 (c0) - ~3,000 lines

- `c00.c` - Main driver, lexer, symbol table, expression parsing
- `c01.c` - Expression tree building, type conversions
- `c02.c` - Function/statement parsing (`extdef()`, `statement()`)
- `c03.c` - Declaration processing
- `c04.c` - Type utilities
- `c05.c` - Operator precedence tables

### Pass 2 (c1) - ~5,300 lines

- `c10.c` - Code generation via pattern matching (`match()`, `rcexpr()`)
- `c11.c` - PDP-11 assembly emission, addressing modes
- `c12.c` - Expression optimization
- `c13.c` - Operator tables

### Headers

- `c0.h` - Definitions for pass 0 (symbol tables, tree nodes, types)
- `c1.h` - Definitions for pass 1 (code generation tables)

### Resources

- `optable` - Machine-readable operation table for PDP-11
- `cvopt.c` - Converter utility for optable

## Compilation Flow

```
source.c --> c0 --> temp1 (intermediate) + temp2 (strings)
                            |
                            v
                 c1 --> PDP-11 assembly (.s)
                            |
                            v
                     as --> object (.o)
```

## Key Design Points

1. **Recursive descent parser** with operator precedence climbing for expressions
2. **Pattern-matching code generator** - `optable` defines assembly templates that `match()` selects based on operand types and complexity
3. **Type system** uses 2-bit encoding per level (PTR=020, FUNC=040, ARRAY=060)
4. **Register allocation** - 3 general registers (R0-R2), R5 is frame pointer
5. **Memory management** - declarations grow up, expression trees grow down

## Front-End Parser (c0)

The front-end uses a **hybrid parsing strategy**:
- **Recursive descent** for statements and declarations
- **Operator precedence** (bottom-up) for expressions

### Lexer (symbol() in c00.c)

Character-based lexical analysis with one-character lookahead:

```
Input -> peekc -> ctab[] classification -> token
              \-> peeksym (pushed-back token)
```

**Character Classes (`ctab[]`):**
| Class | Characters |
|-------|------------|
| `LETTER` | a-z, A-Z, _ |
| `DIGIT` | 0-9 |
| `SPACE` | space, tab |
| `NEWLN` | newline |
| `PLUS`, `MINUS`, etc. | operators |

**Multi-character operators:**
```c
subseq(c, a, b)  // if next char is c return b, else a

case PLUS:  subseq(c, PLUS, INCBEF)     // + or ++
case LESS:  subseq(c,0,1) -> LSHIFT     // < or <<
            else subseq('=',LESS,LESSEQ) // < or <=
```

### Expression Parser (tree() in c00.c)

Classic **operator precedence parsing** with two stacks:

```c
int opst[SSIZE];           // operator stack
int prst[SSIZE];           // precedence stack
union tree *cmst[CMSIZ];   // operand stack (cp points here)
```

**Algorithm:**
```
1. Get token
2. If operand (NAME, CON, STRING):
   - Push onto operand stack
   - Set andflg=1 (expect operator next)

3. If operator:
   - While stack top has higher precedence:
       - Pop operator, call build()
   - Push new operator

4. On end: flush stack, return tree
```

**Precedence levels (from opdope[]):**
```
() [] -> .        highest
! ~ ++ -- - * &   unary
* / %
+ -
<< >>
< <= > >=
== !=
&
^
|
&&
||
?:
= += -= ...       right-associative
,                 lowest
```

### Tree Building (build() in c01.c)

Pops operands, creates tree nodes, inserts type conversions:

```c
build(op) {
    p2 = *--cp;  // pop right operand
    p1 = *--cp;  // pop left operand

    // Special transformations
    if (op == LBRACK) { build(PLUS); op = STAR; }  // a[i] -> *(a+i)
    if (op == DOT) { build(AMPER); op = ARROW; }   // a.b -> (&a)->b

    // Insert conversions from cvtab[]
    cvn = cvtab[lintyp(t1)][lintyp(t2)];
    if (cvn) p2 = convert(p2, t, cvn, len);

    // Create node
    *cp++ = block(op, t, subsp, strp, p1, p2);
}
```

**Conversion table (cvtab[from][to]):**
```
        INT   FLOAT  LONG   PTR
INT      -    ITF    ITL    ITP
FLOAT   FTI    -     FTL     -
LONG    LTI   LTF     -     LTP
PTR     PTI    -      -      -
```

### Statement Parser (statement() in c02.c)

**Recursive descent** with switch on token:

```c
statement() {
    switch (symbol()) {
        case SEMI:   return;
        case LBRACE: blockhead(); while(...) statement(); blkend();
        case KEYW:
            switch (cval) {
                case IF:     cbranch(pexpr(), L1, 0); statement(); ...
                case WHILE:  label(contlab); cbranch(...); statement(); branch(contlab);
                case FOR:    forstmt();
                case SWITCH: pswitch();
                case RETURN: doret();
                case GOTO:   dogoto();
            }
        case NAME:   if (nextchar()==':') { /* label */ }
        default:     rcexpr(tree(1));  // expression statement
    }
}
```

### Control Flow Compilation

```c
// IF statement
cbranch(pexpr(), L1, 0);   // branch to L1 if false
statement();               // then-clause
branch(L2);
label(L1);
statement();               // else-clause
label(L2);

// WHILE statement
label(contlab);
cbranch(pexpr(), brklab, 0);
statement();
branch(contlab);
label(brklab);
```

### External Definitions (extdef() in c02.c)

```c
extdef() {
    getkeywords(&sclass, &typer);  // storage class, type

    do {
        decl1(EXTERN, &typer, 0, NULL);  // parse declarator

        if (function_definition)
            cfunc();           // compile function body
        else if (initializer)
            cinit(ds, ...);    // process initializer
    } while (symbol() == COMMA);
}
```

### Symbol Table (lookup() in c00.c)

```c
struct nmlist *hshtab[HSHSIZ];  // hash table (300 buckets)

lookup() {
    ihash = hash(symbuf);

    // Check keyword first
    if (kwhash[ihash/16] & (1 << ihash%16))
        if (findkw()) return KEYW;

    // Search chain
    for (rp = hshtab[ihash]; rp; rp = rp->nextnm)
        if (strcmp(symbuf, rp->name) == 0)
            return NAME;

    // Create new entry
    rp = Dblock(sizeof(struct nmlist));
    rp->nextnm = hshtab[ihash];
    hshtab[ihash] = rp;
    return NAME;
}
```

### Memory Layout

Two regions growing toward each other:

```
locbase                              coremax
   |                                    |
   v                                    v
   [declarations --->    <--- trees]
                    ^
                DCLSLOP (512 bytes)
```

```c
Dblock(n)   // allocate declaration (grows up)
Tblock(n)   // allocate tree node (grows down)
starttree() / endtree()  // bracket expression
```

### Intermediate Code Output

```c
outcode(fmt, ...)
// B=byte  N=number  S=string  F=asm string

outcode("BS", SYMDEF, name);     // symbol definition
outcode("BN", LABEL, isn++);     // label
outcode("BNN", SWIT, deflab, n); // switch table
```

### Parser Data Flow

```
source.c
    |
symbol() --- lexer ---------> tokens
    |
extdef() --- declarations --> symbol table
    |
cfunc() ---- function body
    |
statement() - recursive descent --> control flow
    |
tree() ----- operator precedence -> expression trees
    |
build() ---- type checking -------> typed trees
    |
rcexpr() --- output ------------> temp1, temp2
```

## Optable Pattern Matching

The code generator is **table-driven** using a domain-specific language in `optable`. The `cvopt` tool compiles this DSL into C arrays linked into c1.

### Pattern Syntax

Each pattern has this structure:
```
%operand1,operand2
    assembly template lines...
```

Example:
```
%a,n
    mov    A1,R
```
Meaning: "if operand1 is addressable and operand2 needs a register, emit `mov <addr>,<reg>`"

### Operand Degree (Complexity) Codes

| Code | Value | Meaning |
|------|-------|---------|
| `z`  | 4     | Zero constant |
| `1`  | 5     | Constant 1 |
| `c`  | 8     | Any constant |
| `r`  | 9     | Register |
| `a`  | 14    | Addressable (name, constant, etc.) |
| `e`  | 20    | Already in a register |
| `n`  | 63    | Needs computation (any complexity) |
| `*`  | +0100 | Indirect (requires STAR op) |

### Type Modifier Flags

| Code | Meaning |
|------|---------|
| `w`  | word/int |
| `b`  | byte/char |
| `u`  | unsigned |
| `l`  | long |
| `f`  | float |
| `d`  | double |

Combined: `ub` = unsigned byte, `ul` = unsigned long, etc.

### Assembly Template Macros

| Macro | Meaning |
|-------|---------|
| `A1`  | Operand 1 address |
| `A2`  | Operand 2 address |
| `R`   | Result register |
| `R1`  | Alternate register |
| `R+`  | Next register (for longs) |
| `R-`  | Previous register |
| `B1`  | "b" suffix if operand 1 is byte |
| `BF`  | "b" or "f" suffix based on tree type |
| `I`   | Instruction name (from instab) |
| `I'`  | Instruction name, special form |
| `F`   | Recursively compile left subtree |
| `S`   | Recursively compile right subtree |
| `F*`  | Compile left subtree, dereference |
| `#1`  | Offset from collapsed addressing |
| `V`   | Emit adc/sbc/sxt for longs |
| `Z`  | Field mask |

### Code Tables

| Table     | Purpose |
|-----------|---------|
| `regtab`  | Compile expression into a register |
| `cctab`   | Set condition codes (for branches) |
| `efftab`  | Compile for side effects only |
| `sptab`   | Push result onto stack |

### Pattern Matching Flow

1. `match()` finds table entry for operator
2. Iterates patterns checking:
   - Operand complexity (degree) acceptable?
   - Type compatibility?
   - Enough registers available?
3. Returns first matching pattern
4. `cexpr()` expands the template macros

### Example: Assignment `a = b + c`

Pattern:
```
%a,n
    S          <- compile right subtree into R
    movB1 R,A1 <- move result to destination
```

Generated:
```asm
    mov  b,r0     ; from compiling b+c
    add  c,r0
    mov  r0,a     ; final assignment
```

### Pattern Reuse

Labels allow sharing patterns:
```
%[add1:]
%n,aw
    F
    IB2  A2,R

%n,e
%   [add1]     <- reuse add1 pattern
```

## Expression Optimizer (c12.c)

The optimizer performs **tree transformations** before code generation. It's called repeatedly as trees are built.

### Entry Points

| Function | Purpose |
|----------|---------|
| `optim()` | Optimize binary operators |
| `unoptim()` | Optimize unary operators |
| `acommute()` | Reorder commutative expressions |

### Constant Folding

Evaluates operations on constants at compile time:
```c
3 + 5      ->  8
~0xFF      ->  0xFF00
1 << 4     ->  16
```

Handled by `const()` for int and `lconst()` for long.

### Algebraic Simplifications

```c
x + 0      ->  x
x * 1      ->  x
x >> 0     ->  x
x / 1      ->  x
x & ~0     ->  x
```

### PDP-11 Specific Transformations

**AND to AND-NOT:**
```c
// PDP-11 has BIC (bit clear) but no AND instruction
a & b      ->  a ANDN ~b
```

**Right shift to negative left shift:**
```c
// PDP-11 ASH uses negative count for right shift
x >> n     ->  x << (-n)
```

**Subtraction to addition:**
```c
// ADD is more flexible than SUB on PDP-11
x - 5      ->  x + (-5)
```

### Strength Reduction

Multiply/divide by power of 2 becomes shift:
```c
x * 8      ->  x << 3
x / 4      ->  x >> 2    (unsigned only)
x % 16     ->  x & 15    (unsigned only)
```

### Commutative Reordering (acommute)

For +, *, |, &, ^:
1. Flatten tree into list
2. Combine all constants
3. Sort by complexity (simpler first)
4. Rebuild tree

```c
// Before
(a + 3) + (b + 5)
// After
(a + b) + 8
```

### Distributive Factoring (distrib)

```c
// Before
6*a + 3*b
// After
3*(2*a + b)
```

### Address Mode Collapsing

Combines pointer arithmetic into PDP-11 addressing modes:
```c
*(reg + const)   ->  const(reg)     // Indexed mode
*(p++)           ->  (reg)+         // Auto-increment
*(--p)           ->  -(reg)         // Auto-decrement
```

### Pointer/Address Cancellation

```c
&*p        ->  p
*&x        ->  x
```

### Relational Optimizations

Swap operands to reduce register pressure:
```c
// Put simpler operand on right
if (degree(left) < degree(right))
    swap and adjust operator
```

Eliminate trivial comparisons:
```c
(a < b) >= 0   ->  1    // Always true
(a < b) < 0    ->  0    // Always false
```

### Type Conversion Simplification

```c
// Cancel inverse conversions
(long)(int)x   ->  x    // if x fits
(int)(long)x   ->  x

// Push conversions down
(int)(a + b)   ->  (int)a + (int)b
```

### Degree (Register Pressure) Tracking

Each node has a **degree** indicating registers needed:
- Leaf nodes: based on addressability
- Binary ops: `max(left, right)` or `max + 1` if equal
- Complex ops (calls, long math): degree = 10 (spill)

### Optimizer Flow

```
optim(tree)
    |
    +-- Leaf? -> Optimize float constants
    |
    +-- Unary? -> unoptim()
    |            +-- Cancel &* and *&
    |            +-- Fold constants
    |            +-- Collapse address modes
    |            +-- Recognize auto-inc/dec
    |
    +-- Binary? ->
            +-- Transform AND -> ANDN
            +-- Transform MINUS -> PLUS
            +-- Commutative? -> acommute()
            |                   +-- Flatten
            |                   +-- Fold constants
            |                   +-- distrib()
            |                   +-- Rebuild sorted
            +-- Optimize children recursively
            +-- Constant fold if both constants
            +-- Strength reduce * / to shifts
            +-- Calculate degree
```

## Code Generator (c1)

The code generator reads intermediate code from c0 and emits PDP-11 assembly using **pattern matching** with **Sethi-Ullman register allocation**.

### Source Files

| File | Purpose |
|------|---------|
| `c10.c` | Pattern matching, expression compilation, macro expansion |
| `c11.c` | Address output, degree calculation, intermediate code reader |
| `c12.c` | Expression optimization |
| `c13.c` | Operator tables, instruction tables |

### Intermediate Code Reader (getree() in c11.c)

Reads binary intermediate code, reconstructs expression trees:

```c
getree() {
    union tree *expstack[STKS];

    for (;;) {
        op = geti();  // read 2-byte opcode

        switch (op) {
            case EXPR:
                tree = optim(*--sp);
                rcexpr(tree, efftab, 0);
                break;

            case CBRANCH:
                cbranch(tree, lbl, cond, 0);
                break;

            case NAME:
            case CON:
                *sp++ = build_leaf();
                break;

            default:  // operators
                *sp++ = tnode(op, type, sp[-2], sp[-1]);
        }
    }
}
```

### Expression Compiler (rcexpr() in c10.c)

Main workhorse - compiles expression using a code table:

```c
rcexpr(tree, table, reg) {
    // Handle special cases
    switch (tree->t.op) {
        case STRASG: strasg(tree); return;
        case RFORCE: rcexpr(tr1, regtab, 0); movreg(r, 0); return;
        case SEQNC:  rcexpr(tr1, efftab); rcexpr(tr2, table); return;
        case CALL:   compile_call(); return;
    }

    // Extract postfix ++/-- for later
    if (delay(&tree, table, reg)) table = efftab;

    // Reorder: reg=x+y -> reg=x; reg+=y
    reorder(&tree, table, reg);

    // Try pattern match
    if ((r = cexpr(tree, table, reg)) >= 0)
        return r;

    // Fall back through tables
    if ((r = cexpr(tree, regtab, reg)) >= 0) {
        if (table == sptab) printf("mov r%d,-(sp)\n", r);
        if (table == cctab) printf("tst r%d\n", r);
        return r;
    }

    error("No code table for op");
}
```

### Pattern Matcher (match() in c10.c)

Finds code table entry matching the tree:

```c
match(tree, table, nrleft) {
    d1 = dcalc(p1, nrleft);  // left operand complexity
    d2 = dcalc(p2, nrleft);  // right operand complexity

    // Find table for operator
    for (; table->tabop != op; table++)
        if (table->tabop == 0) return NULL;

    // Search patterns
    for (opt = table->tabp; opt->tabdeg1; opt++) {
        if (d1 > (opt->tabdeg1 & 077)) continue;
        if (notcompat(p1, opt->tabtyp1)) continue;
        if (d2 > (opt->tabdeg2 & 077)) continue;
        if (notcompat(p2, opt->tabtyp2)) continue;
        return opt;  // match!
    }
    return NULL;
}
```

### Degree Calculation (dcalc() in c11.c)

Computes operand complexity for pattern matching:

| Degree | Meaning |
|--------|---------|
| 4 | Zero constant |
| 5 | Constant 1 |
| 8 | Positive constant |
| 9 | Register variable |
| 12 | Addressable (name, amper, autoi/d) |
| 20 | Needs register, have enough |
| 24 | Needs register, must spill |

### Code Emission (cexpr() in c10.c)

Expands matched pattern into assembly:

```c
cexpr(tree, table, reg) {
    opt = match(tree, table, nreg - reg);
    if (!opt) return -1;

    string = opt->tabstring;

    loop:
    c = *string++;
    if (c & 0200) putchar('\t');  // high bit = tab

    switch (c & 0177) {
        case '\0': return reg;
        case 'A': pname(p1); break;           // operand 1
        case 'B': pname(p2); break;           // operand 2
        case 'M': prins(op, instab); break;   // instruction
        case 'C': if (byte) putchar('b');     // B1
        case 'G': rcexpr(p1, regtab, reg);    // F (compile left)
        case 'K': rcexpr(p2, sptab, 0);       // S (compile right)
        case 'I': printf("r%d", reg);         // R (result reg)
        case 'J': printf("r%d", reg1);        // R1
        default: putchar(c);
    }
    goto loop;
}
```

### Address Output (pname() in c11.c)

Generates PDP-11 addressing mode syntax:

```c
pname(p) {
    switch (p->t.op) {
        case CON:    printf("$%o", value);        // $17
        case NAME:
            switch (class) {
                case REG:    printf("r%d", nloc);        // r2
                case OFFS:   printf("%o(r%d)", off, rn); // 4(r5)
                case STATIC: printf("L%d", nloc);        // L123
                case EXTERN: printf("%s", name);         // _foo
            }
        case AMPER:  printf("$"); pname(tr1);     // $_foo
        case AUTOI:  printf("(r%d)+", nloc);      // (r2)+
        case AUTOD:  printf("-(r%d)", nloc);      // -(r2)
        case STAR:   printf("*"); pname(tr1);     // *_foo
    }
}
```

### Expression Reordering (reorder(), sreorder())

Transforms for better code generation:

```c
// reg = x + y  ->  reg = x; reg += y
if (p1->n.class == REG && p->t.op == ASSIGN) {
    if (p2->t.op == PLUS) {
        p->t.tr2 = p2->t.tr1;
        rcexpr(p, efftab, reg);    // reg = x
        p->t.tr2 = p2->t.tr2;
        p->t.op = ASPLUS;          // reg += y
    }
}
```

### Postfix ++/-- Extraction (delay(), sdelay())

```c
// x + y++  ->  compile x+y, then y++
delay(treep, table, reg) {
    if (p->t.op == INCAFT && p->t.tr1->t.op == NAME) {
        r = rcexpr(p->t.tr1, table, reg);
        *treep = p;  // leave y++ for later
        return r + 1;
    }
}
```

### Conditional Branch (cbranch() in c11.c)

```c
cbranch(tree, lbl, cond, reg) {
    switch (tree->t.op) {
        case LOGAND:
            if (cond) {
                cbranch(tr1, L1, 0, reg);  // short-circuit
                cbranch(tr2, lbl, 1, reg);
                label(L1);
            } else {
                cbranch(tr1, lbl, 0, reg);
                cbranch(tr2, lbl, 0, reg);
            }
            return;

        case LOGOR:  // similar, inverted
        case EXCLA:  cbranch(tr1, lbl, !cond, reg); return;
    }

    rcexpr(tree, cctab, reg);
    if (isfloat(tree)) printf("cfcc\n");
    branch(lbl, maprel[op], !cond);
}
```

### Switch Statement (pswitch() in c11.c)

Three strategies based on case count and density:

```c
pswitch(fp, lp, deflab) {
    range = lp->swval - fp->swval;
    ncase = lp - fp;

    if (range <= 3 * ncase) {
        // Direct jump table
        printf("asl r0\njmp *L%d(r0)\n", tbl);
    } else if (ncase < 10) {
        // Linear search
        for (each case) printf("cmp r0,$%o\njeq L%d\n");
    } else {
        // Hash table
        printf("div $%o,r0\njmp *L%d(r1)\n", tabs, tbl);
    }
}
```

### Register Allocation

Simple Sethi-Ullman style:

- `nreg = 3` available (R0, R1, R2)
- R5 = frame pointer
- `degree` field tracks registers needed per subtree
- `oddreg()` handles div/mod (need odd register)
- `regpanic` triggers restart if allocation fails

### Code Generation Flow

```
temp1 (intermediate)
         |
    getree() --- read opcodes, build tree stack
         |
    optim() --- tree optimization
         |
    rcexpr(tree, table, reg)
         |
    +----+----+----+
    |    |    |    |
special reorder delay
cases    |     |
         +--+--+
            |
       match() --- find pattern
            |
       cexpr() --- expand template
            |
    +-------+-------+
    |       |       |
  pname() prins() recurse
    |       |
    +---+---+
        |
   PDP-11 assembly
        |
      stdout
```

## Type Encoding

```c
#define INT    0
#define CHAR   1
#define FLOAT  2
#define DOUBLE 3
#define STRUCT 4
#define LONG   6
#define UNSIGN 7

#define PTR    020   // pointer - next level
#define FUNC   040   // function - next level
#define ARRAY  060   // array - next level
```

## Symbol Table Structure

```c
struct nmlist {
    char hclass;      // Storage class (EXTERN, STATIC, AUTO, REG, TYPEDEF)
    char hflag;       // Flags (FMOS, FTAG, FENUM, etc.)
    int htype;        // Type encoding
    int *hsubsp;      // Subscript list for arrays
    union str *hstrp; // Structure description
    int hoffset;      // Offset/location
    char *name;       // ASCII symbol name
    int hblklev;      // Block level
};
```

## Expression Tree Node

```c
struct tnode {
    int op;           // Operator
    int type;         // Result type
    union tree *tr1;  // Left operand
    union tree *tr2;  // Right operand
};
```

## Memory Constraints

- `CMSIZ = 40` - expression stack depth limit
- `HSHSIZ = 300` - symbol hash table size
- `MAXCPS = 32` - symbol name length cutoff
- `SWSIZ = 300` - switch case table size
