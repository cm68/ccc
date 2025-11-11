# alloca() Optimization Opportunities

**Goal**: Replace malloc/free pairs in same function scope with stack allocation

**Benefits**: Eliminate free() calls, reduce code size, prevent memory leaks

## High-Priority Conversions

### 1. type.c: struct/union tag names (getbasetype)
**Location**: type.c:697-750
**Pattern**: strdup() + 5 free() calls for tag name
**Solution**: Use 64-byte stack buffer
**Savings**: ~40 bytes, eliminates 5 free() calls

### 2. declare.c: parameter names (parsedecl)
**Location**: declare.c:281, 326
**Pattern**: strdup() in parse_param_name() + 2 free() calls
**Solution**: Modify to accept stack buffer
**Savings**: ~30 bytes, eliminates 2 free() calls

### 3. ccc.c: basename temp (get_basename_no_ext)
**Location**: ccc.c:83-90
**Pattern**: strdup() for basename() temp + 1 free()
**Solution**: Use PATH_MAX stack buffer
**Savings**: ~15 bytes, eliminates 1 free() call

## Total Estimated Savings

**~85 bytes** from eliminating 8 free() calls and simplifying control flow

**Recommendation**: Use fixed-size stack buffers (most portable, sufficient for identifiers and paths)
