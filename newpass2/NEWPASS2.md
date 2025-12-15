# newpass2 Architecture

The newpass2 code generator translates AST input from cc1 into Z80 assembly.

## Pipeline Overview

```
AST input → Parse → Schedule → Emit → Assembly output
             ↓         ↓         ↓
           Build    Demand    Walk tree,
           expr    calc +     emit
           tree    dest       instructions
                   assign
```

## Expression Tree

The core data structure is `struct expr`:

```c
struct expr {
    char op;          /* operator: '#' '$' 'R' 'V' 'M' '=' '+' '@' ... */
    char size;        /* result size in bytes (1, 2, 4) */
    char type;        /* type suffix from AST ('b' 's' 'l' 'B' 'S' 'L') */
    struct expr *left, *right;
    union { long l; short s; char c; } v;  /* constant value */
    char *sym;        /* symbol name */
    unsigned char aux, aux2;  /* operator-specific */
    unsigned char demand;     /* temp register demand */
    unsigned char dest;       /* destination register */
    unsigned char special;    /* optimization pattern */
    char offset;              /* indexed addressing offset */
    short incr;               /* inc/dec amount */
};
```

### Operator Codes

**Primary:**
- `#` - constant (value in `v`)
- `$` - global symbol (name in `sym`)
- `R` - register variable (register in `aux`)
- `V` - local/stack variable (IY offset in `aux2`)

**Unary:**
- `M` - memory dereference
- `W` - widen (zero-extend byte to word)
- `N` - narrow (truncate word to byte)
- `x` - sign-extend (byte to word)
- `~` - bitwise complement
- `!` - logical not
- `\` - unary minus (negation)

**Inc/Dec:**
- `(` - pre-increment
- `)` - post-increment
- `{` - pre-decrement
- `}` - post-decrement

**Binary:**
- `+ - * / %` - arithmetic
- `& | ^` - bitwise
- `y` - left shift (`<<`)
- `w` - signed right shift (`>>`)
- `z` - unsigned right shift
- `< > Q n L g` - comparisons (`< > == != <= >=`)
- `j h` - logical and/or (`&& ||`)
- `=` - assignment

**Compound Assignment:**
- `P o 1 a X m` - `+= -= |= &= ^= %=`
- `T 2` - `*= /=`
- `0 6` - `<<= >>=`

**Other:**
- `@` - function call (nargs in `aux`)
- `?` - ternary (`:` node holds then/else branches)
- `,` - comma operator
- `Y` - memory copy (length in `aux`)
- `F` - bitfield access

## Three-Phase Code Generation

### Phase 1: Demand Calculation (bottom-up)

`calcDemand()` walks the tree bottom-up computing register pressure:

- **Primaries** (`# $ R V`): demand = 1
- **Unary ops**: demand = child's demand
- **Binary ops**: demand = max(left, right+1)
- **Function calls**: demand = max of all args

Sets `e->spill = 1` when right subtree needs more than 1 temp
(indicating DE must be pushed before evaluating right child).

Also detects optimization patterns, setting `e->special`:

| Pattern | Code |
|---------|------|
| SP_SYMOFS | `+s $sym #const` → `ld hl,sym+ofs` |
| SP_SYMOFD | `Ms[+s $sym #const]` → `ld hl,(sym+ofs)` |
| SP_MSYM | `Ms $sym` → `ld hl,(sym)` |
| SP_IXOD | `M[+s Ms[Rs ix] #ofs]` → `(ix+ofs)` |
| SP_MUL2 | `*s expr #pow2` → `add hl,hl` repeated |
| SP_STCONST | `= [M addr] #const` → `ld (hl),n` |
| SP_STIX | `= [+s #ofs Rs ix] expr` → `ld (ix+ofs),src` |
| SP_INCR/DECR | `(s Rs reg` → `inc reg` |
| SP_INCGLOB | `(s $sym` → `ld hl,(sym); inc hl; ld (sym),hl` |
| SP_SIGN | `gs Ms $sym #0` → `bit 7,(sym+n)` |
| SP_SIGNREG | `gs Ms[Rs bc] #0` → `bit 7,b` |
| SP_BITTEST | `&B M[(ix+ofs)] #pow2` → `bit n,(ix+ofs)` |
| SP_CMPxx | Byte compare with indexed/indirect operand |
| SP_ADDBC | `+s Ms[Rs bc] #const` → `ld hl,const; add hl,bc` |

### Phase 2: Destination Assignment (top-down)

`assignDest()` walks top-down assigning target registers:

**Register model:**
- `R_HL` (6) - primary accumulator for word results
- `R_DE` (5) - secondary accumulator (left operand of binops)
- `R_A` (7) - byte accumulator
- `R_BC` (3) - register variable (word)
- `R_B/R_C` (1/2) - register variable (byte)
- `R_IX` (4) - struct pointer register variable
- `R_IY` (8) - frame pointer (IY+offset for locals)
- `R_TOS` (13) - push to stack (function arguments)
- `R_IXO/R_IYO` - indexed addressing mode

**Assignment rules:**
- Binary ops: left→DE, right→HL (commutative ops may swap based on demand)
- Comparisons: left→A (bytes) or HL (words), right→HL
- Function args: all→TOS
- Unary ops: child gets parent's dest
- Byte results: dest=A unless parent expects word
- Word results: dest=HL

### Phase 3: Emission

`emitExpr()` walks the tree with destinations already assigned:

1. Check `special` pattern, emit optimized code if set
2. Otherwise emit children based on demand ordering
3. Emit operation instruction
4. Result lands in assigned `dest` register

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

During expression parsing, `$symbol` references are resolved:
- If local with reg≠0 → `R` node (register variable)
- If local with reg=0 → `V` node (stack variable via IY+offset)
- Otherwise → `$` node (global symbol)

## Calling Convention

- Arguments pushed right-to-left (parser builds arg list in reverse)
- Word results in HL
- Byte results in A
- Long results in HL:HL' (using shadow registers)
- Caller cleans stack after call returns

## File Organization

| File | Purpose |
|------|---------|
| cc2.h | Shared definitions, struct expr |
| newcc2.c | Main, symbol table management |
| astio.c | Low-level AST reading |
| parseast.c | Expression/statement parsing |
| codegen.c | calcDemand(), assignDest() |
| emitexpr.c | emitExpr() and helpers |
| emit.c | emit(), comment(), output formatting |

## Output Format

Assembly output uses custom format specifiers in `emit()`:
- `%o` - signed offset with explicit sign: `+5` or `-3`
- `%r` - register name with optional offset: `bc` or `(iy+5)`

Comments include scheduling annotations:
```asm
; +s d=2 hl [        ; op=+, type=s, demand=2, dest=hl
;   $foo d=1 hl      ; symbol foo, demand 1, dest hl
;   #s 10 d=1 de     ; const 10, demand 1, dest de
; ]
```
