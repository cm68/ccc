# Stack Calling Convention Experiment

This document describes an experiment comparing two stack cleanup strategies
for function calls.

## Background

When a function is called with arguments:
1. Arguments are pushed onto the stack (right-to-left)
2. CALL instruction pushes return address
3. Callee executes and returns value in HL
4. **Someone** must clean up the pushed arguments

The question: who cleans up?

## Option 1: CALLER_FREE (Current Model)

```
caller:
    push arg2
    push arg1
    call func
    pop de        ; cleanup arg1
    pop de        ; cleanup arg2
    ; use result in HL
```

**Advantages:**
- No coordination needed between caller/callee
- Callee doesn't need to know argument count
- Works with variadic functions naturally
- Simple function epilog (just RET)

**Disadvantages:**
- Cleanup code at every call site (code size)
- If cleanup is deferred (via framefree), args accumulate on stack

**Current implementation:**
- cc2 generates push instructions for args
- For functions with frames: framefree does bulk cleanup via `ld sp, iy`
- For leaf functions: explicit pop instructions after call

## Option 2: CALLEE_FREE

```
caller:
    push arg2
    push arg1
    call func
    ; args already cleaned by callee
    ; use result in HL

func:
    ; ... body ...
    ret 4         ; return and pop 4 bytes of args
```

**Advantages:**
- Single cleanup site per function (smaller code overall)
- Stack stays cleaner during execution
- Z80 has no `ret n` but can be emulated

**Disadvantages:**
- Callee must know exact argument size
- Variadic functions need special handling
- Mismatched arg counts cause stack corruption
- More complex function epilog

**Implementation approach for Z80 (no RET n instruction):**
```asm
; Option A: Manual cleanup before return
func_epilog:
    pop hl        ; save return address
    pop de        ; discard arg1
    pop de        ; discard arg2
    jp (hl)       ; return

; Option B: Adjust SP then return
func_epilog:
    ld hl, 4      ; arg size
    add hl, sp
    ld sp, hl
    ret           ; but this returns to wrong address!

; Option C: Use IX/IY to hold return address
func_epilog:
    pop ix        ; save return address
    ld sp, iy     ; restore SP (cleans locals + args if IY set right)
    jp (ix)       ; return via IX
```

## Interaction with framealloc/framefree

Current framealloc sets IY to point at saved-IY location:
```
SP -> [locals]
      [saved IY]    <- IY points here
      [ret addr]    <- IY + 2
      [args]        <- IY + 4
```

For CALLEE_FREE, framefree would need to also skip args:
```asm
; framefree with arg cleanup
; A = arg bytes to clean
framefree_args:
    ld l, a
    ld h, 0
    add hl, sp      ; HL = SP + args
    ex de, hl       ; DE = new SP target
    ld sp, iy       ; SP = IY (at saved IY)
    pop iy          ; restore IY, SP now at ret addr
    pop hl          ; HL = return address
    ld sp, de       ; skip args
    jp (hl)         ; return
```

## Build Configuration

Use preprocessor flags to select model:

```c
#ifdef CALLER_FREE
// Current model - caller pops args after call
#endif

#ifdef CALLEE_FREE
// Experimental - callee pops args before return
#endif
```

## Variadic Functions (stdarg)

CALLER_FREE is natural for variadics - caller knows how many args it pushed.

CALLEE_FREE requires variadics to use CALLER_FREE convention, or pass arg
count explicitly. Common approaches:
1. Mark variadic functions to use caller-cleanup
2. Last fixed parameter tells callee the arg count
3. Sentinel value marks end of args

## Alloca Consideration

alloca() dynamically allocates stack space. Both models interact:

- CALLER_FREE: alloca'd space must be freed before function returns
- CALLEE_FREE: alloca'd space complicates the callee's cleanup calculation

Not yet addressed in this experiment.

## Test Plan

1. Implement CALLEE_FREE variant in cc2
2. Create framefree_args helper
3. Measure code size difference on test suite
4. Verify correctness in simulator
5. Document findings

## Status

- [x] CALLER_FREE: current working implementation
- [ ] CALLEE_FREE: experimental, not yet implemented
