# Known CC2 Code Generator Bugs

This file documents known bugs in the cc2 code generator that cause incorrect
assembly output. These bugs manifest as unresolved external symbols when
linking stage1 object files.

## Bug 1: Store Destination Emit Instructions

### Symptom
Unresolved external symbols like `strname`, `e`, `parms` in cc1.o.
Assembly contains incorrect instructions like `ld a, (strname)` where
`strname` is a local variable that should use IY-relative addressing.

### Example
In stage1/parse.s around line 6350:
```asm
call _strdup
inc sp
inc sp
ex de, hl
ld a, (strname)    ; BUG: should not be here
ld (hl), e         ; HL is garbage at this point
inc hl
ld (hl), d
```

Should be:
```asm
call _strdup
inc sp
inc sp
ex de, hl
ld l, (iy - 9)     ; load strname pointer low byte
ld h, (iy - 8)     ; load strname pointer high byte
ld (hl), e         ; store to strname->name
inc hl
ld (hl), d
```

### AST Pattern
The bug occurs with struct member stores through local pointer variables.
cc2 sees the assignment as an expression tree:
```
=:2                    ; store word
  M:2                  ; DEREF - destination address
    $:2 sym=strname    ; local pointer variable
  @:2                  ; function call result (strdup)
    ...
```

The `M` node for the store destination incorrectly gets scheduled with
emit instructions, when it should be handled entirely by the parent `=`.

### Root Cause
In codegen.c, the sched2Store() function correctly schedules the store
instruction on the `=` node, but child destination nodes (`M` and `$`)
incorrectly receive emit instructions.

Debug output shows:
```
=:2 flags=[VIY] loc=reg(-) dest=HL emit={(IY+n)<-HL}   ; correct
  M:2 flags=[VIY] loc=stk(-9) dest=HL emit={A<-(mem)}  ; BUG: should be empty
    $:2 loc=mem dest=A emit={A<-const} sym=strname     ; BUG: should be empty
```

The `M:2` node has `emit={A<-(mem)}` (EO_A_MEM) when it should have no
emit instructions since the parent `=` node handles the store directly.

### Location in Code
- codegen.c: sched2Store() - store destination handling
- codegen.c: sched2Expr() case 'M' - DEREF scheduling
- emitexpr.c: case EO_A_MEM - emits `ld a, (symbol)`

### Workaround
None known. Affected code patterns will generate incorrect assembly.

### Files Affected
- stage1/parse.s (strname)
- stage1/expr.s (e)
- stage1/macro.s (parms)

### Source Location
The `strname` bug is triggered by **parse.c:1051** in `declaration()`:
```c
struct name *strname = (struct name *)v->u.init->var;
char fullname[256];
...
free(strname->name);
strname->name = strdup(fullname);  // BUG HERE
```

The AST for `strname->name = strdup(fullname)` is:
```
=:2
  M:2
    +:2                     ; strname + offsetof(name)
      M:2
        $:2 sym=strname     ; <-- gets EO_A_MEM incorrectly
      C:2 val=0
  @:2 sym=_strdup
    $:2 sym=fullname
```

The `$:2 sym=strname` node under the store destination gets `EO_A_MEM`
which emits `ld a, (strname)` - treating the local as a global symbol.

---

## Bug 2: Static Array Symbol Naming

### Symptom
Unresolved external symbol `basictype0` in cc1.o.
Static file-scope arrays get `_0` suffix as if they were local variables.

### Example
In type.c:
```c
static struct {
    char *name;
    short size;
    unsigned char flags;
} basictype[] = {
    { "_char_", 1, 0 },
    ...
};
```

Generated AST contains `basictype0` instead of `_basictype` or `basictype`.
Assembly references `ld de, basictype0` but symbol is never defined.

### Root Cause
The `_0` suffix pattern is used for local variables mangled with function
index. Static file-scope arrays should not receive this suffix.

In the AST:
```
Z$0abasictype0a000b000501[...
```

The symbol is emitted as `basictype0` (with `_0` suffix) instead of the
correct `_basictype` for a static global.

### Location in Code
- cc1 (pass1): outast.c or wherever static array names are emitted
- The bug is in cc1's AST emission, not cc2

### Workaround
None known.

### Files Affected
- stage1/type.s (basictype0)

---

## Bug 3: Emit Instruction on Destination Subtree

### Symptom
Some local variables are treated as globals in functions with many locals.

### Theory
Bug 1 and Bug 3 may be the same underlying issue. The destination subtree
of a store (`=`) node incorrectly receives emit instructions. The `M` and
`$` nodes under the destination should have no emit - the parent `=` node
should handle the entire store operation.

When sched2Store() processes a store, it should mark the destination
subtree as "don't emit" but something causes emit instructions to leak
through in certain register allocation states.

### Evidence
Debug output shows correct local variable lookup, but destination nodes
have emit instructions like `EO_A_MEM` that generate global references.

### Location in Code
- codegen.c: sched2Store() - should suppress destination emit
- codegen.c: sched2Deref() - may incorrectly add emit to destination M nodes

---

## Testing Notes

Simple test cases DO NOT reproduce these bugs:
```c
void test(char *fullname) {
    struct name *strname;
    strname->name = strdup(fullname);  // Works correctly
}
```

The bugs only manifest in functions with:
- Many local variables (10+) in the F record
- Complex expression trees with multiple function calls
- Specific register allocation states (which registers are live)

cc2 has no concept of C scope - it sees a flat list of locals from the F
record. The "complexity" that triggers these bugs is likely:
1. More locals = more IY offsets to track
2. More expressions = more register pressure
3. Certain sequences of register states expose the scheduling bug

The stage1 build triggers these patterns because compiler source files
have many locals and complex expressions.

---

## Impact

These bugs cause unresolved external symbols at link time:
```
$ wsnm stage1/cc1.o | grep ' U ' | grep -v '_'
  basictype0
  strname
  e
  parms
```

The test suite passes because test cases don't trigger these edge cases.
Full linking of stage1 binaries would fail due to these unresolved symbols.

---

## Debugging Tips

1. Enable cc2 debug tracing:
   ```
   ./cc2 -v 0x10 file.ast 2>&1 | grep findVar
   ```

2. Check scheduled emit instructions in generated .s files:
   ```
   grep 'emit={' stage1/parse.s | grep -v 'emit={?}\|emit={NOP}'
   ```

3. Look for bare symbol references that should be IY-relative:
   ```
   grep 'ld.*([a-z_]*[0-9]*)' stage1/*.s | grep -v 'iy\|ix\|hl\|bc\|de'
   ```
