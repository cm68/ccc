# Stack Frame and Calling Convention

This document describes the stack layout and calling convention used by pass2.

## Stack Frame Layout

For functions with local variables (frame size > 0):

```
High addresses
    +----------------+
    | arg N          |  IY + 4 + (N-1)*2
    | ...            |
    | arg 1          |  IY + 4
    +----------------+
    | return address |  IY + 2
    +----------------+
    | saved IY       |  IY + 0  <- IY points here after framealloc
    +----------------+
    | local N        |  IY - 2
    | ...            |
    | local 1        |  IY - framesize
    +----------------+  <- SP after framealloc
Low addresses
```

## Frame Allocation

Functions with locals call `framealloc` with frame size in A:

```asm
foo:
    ld a,12           ; frame size (bytes for locals)
    call framealloc
    ; function body, locals at (iy-1), (iy-2), etc.
    jp framefree      ; or just 'ret' for void functions
```

The `framealloc` helper:
1. Pushes IY (saves caller's frame pointer)
2. Copies SP to IY (establishes new frame)
3. Subtracts A from SP (allocates local space)

```asm
framealloc:
    push iy
    ld iy,0
    add iy,sp         ; IY = SP (at saved IY)
    ld l,a
    ld h,0
    ex de,hl
    ld hl,0
    add hl,sp
    or a
    sbc hl,de         ; HL = SP - framesize
    ld sp,hl          ; allocate locals
    ret
```

## Frame Deallocation

The `framefree` helper restores SP and IY, then returns:

```asm
framefree:
    ld sp,iy          ; discard locals
    pop iy            ; restore caller's IY
    ret
```

Return statements in framed functions use `jp framefree` instead of `ret`.

## Leaf Functions

Functions without locals (frame size = 0) skip frame setup:

```asm
bar:
    ; no framealloc call
    ; function body
    ret               ; direct return
```

## Argument Passing

Arguments are pushed right-to-left before the call:

| Type | Size | Push Method |
|------|------|-------------|
| byte | 1 | `push af` (value in A, F is garbage) |
| word | 2 | `push hl` |
| long | 4 | `push hl` twice (high word first) |

```asm
    ; call foo(a, b, c) where a=byte, b=word, c=word
    ; push right to left: c, b, a
    ld hl,(c)
    push hl           ; arg 3 (word)
    ld hl,(b)
    push hl           ; arg 2 (word)
    ld a,(a)
    push af           ; arg 1 (byte, in high byte of stack word)
    call foo
```

### Byte Parameter Stack Position

Byte parameters pushed via `push af` have the value in the high byte of the
stack word (A is pushed, F occupies low byte). Pass2 adjusts the IY offset:

```c
// In parseFunc():
if (ISBYTE(ptype) && preg == 0 && poff > 0)
    poff++;  // access byte at IY+offset+1 (high byte of word)
```

### Interop warning: zc3/hitech uses the opposite byte convention

The hitech compiler (zc3) puts a prototyped byte argument in the LOW
byte of the stack word, with a junk high byte (it emits `ld l,(hl)` /
`push hl`); unprototyped calls widen to a full 16-bit value.  The
push-af convention above puts the value in the HIGH byte.  The two
cannot share hand-written callees that take byte arguments.

The libsrc/libu syscall wrappers currently implement the ZC3
convention (low byte), because the running native toolchain is
zc3-compiled - a `pop af` wrapper under zc3 reads the junk byte (this
was a real bug: close() closed garbage descriptors and leaked fds).
When ccc becomes the system compiler, the byte-argument wrappers
(close, dup, read, write, seek, gtty, fstat, stty) need a
ccc-convention variant tree built to this document.  Keep the two
conventions in separate source trees rather than conditional
assembly - asz stays a minimal back-end assembler.

## Return Values

| Type | Location |
|------|----------|
| byte | A register |
| word | HL register |
| long | lR memory (4 bytes at fixed address) |

## Argument Access

Within a function, arguments are accessed via positive IY offsets:

```asm
    ld a,(iy+5)       ; first byte arg (at IY+4, +1 for high byte)
    ld l,(iy+6)       ; second word arg low byte
    ld h,(iy+7)       ; second word arg high byte
```

## Local Variable Access

Locals are accessed via negative IY offsets:

```asm
    ld a,(iy-1)       ; first local byte
    ld l,(iy-2)       ; second local word low byte
    ld h,(iy-1)       ; second local word high byte (overlaps!)
```

Note: Pass1 assigns offsets to avoid overlap; the example above is illustrative.

## Register Variables

Some locals are allocated to registers instead of the stack:

| Register | Type | Usage |
|----------|------|-------|
| B | byte | First eligible byte local |
| C | byte | Second eligible byte local |
| BC | word | First eligible word local |
| IX | pointer | Struct pointer parameter |

Register variables don't consume stack space. Pass1 decides allocation based
on usage patterns and loop nesting.

## IX Register Usage

IX is reserved for struct pointer parameters. When a function has a struct
pointer parameter in IX:

```c
void foo(struct bar *p) {
    p->field = 5;  // access via IX+offset
}
```

```asm
foo:
    ; p is in IX (not on stack)
    ld (ix+4),5       ; p->field at offset 4
    ret
```

Fields are accessed via `(ix+offset)` addressing mode, which pass2 optimizes
by collapsing `DEREF[+p DEREF[REGVAR(ix)] #ofs]` patterns to LOCALVAR nodes.

## Stack Cleanup

The caller is responsible for cleaning up pushed arguments. However, pass2
does not emit explicit cleanup code after calls. Instead:

1. **Framed functions**: `framefree` resets SP to IY, discarding both locals
   and any accumulated argument pushes
2. **Leaf functions**: Typically don't make calls, so no cleanup needed

This means argument space accumulates on the stack during nested calls within
a function, but is reclaimed when the function returns.

## Long (32-bit) Operations

Long values use memory temporaries since Z80 lacks 32-bit registers:

- `lL` - left operand (4 bytes)
- `lR` - right operand / result (4 bytes)

Long arguments are pushed as two words (high first):
```asm
    ld hl,(longval+2) ; high word
    push hl
    ld hl,(longval)   ; low word
    push hl
    call func_taking_long
```

Long locals are accessed by computing address, then using helper:
```asm
    push iy
    pop hl
    ld de,-8          ; offset to long local
    add hl,de
    call lldHLR       ; load 4 bytes from (HL) to lR
```

## Helper Functions

| Helper | Purpose |
|--------|---------|
| `framealloc` | Allocate stack frame (A = size) |
| `framefree` | Deallocate frame and return |
| `callhl` | Call function pointer in HL |
| `switch` | Dispatch switch via table at HL |
| `lldHL` | Load long from (HL) to lL |
| `lldHLR` | Load long from (HL) to lR |
| `lstHL` | Store long from lL to (HL) |
| `lstHLR` | Store long from lR to (HL) |
| `ladd/lsub` | Long add/subtract (lR = lL op lR) |
| `land/lor/lxor` | Long bitwise ops |
| `lneg/lcom` | Long negate/complement |
| `lcmp` | Long compare (sets flags) |
| `lshl/lashr/lshr` | Long shifts (A = count) |
| `imul/idiv/imod` | Word multiply/divide/modulo |
| `imulb/idivb/imodb` | Byte multiply/divide/modulo |
| `imula` | Multiply HL by A |

## Example: Complete Function

```c
int add(int a, int b) {
    int sum;
    sum = a + b;
    return sum;
}
```

```asm
    .globl add
add:
    ld a,2            ; frame size for 'sum'
    call framealloc
    ; sum at (iy-2), a at (iy+4), b at (iy+6)
    ld l,(iy+4)
    ld h,(iy+5)       ; load a
    ld e,(iy+6)
    ld d,(iy+7)       ; load b
    add hl,de         ; a + b
    ld (iy-2),l
    ld (iy-1),h       ; store to sum
    ; return sum (already in HL)
    jp framefree
```
