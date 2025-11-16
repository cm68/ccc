# Code Duplication Analysis

**Goal**: Reduce code size to fit <64KB total (code + data)

## High-Impact Opportunities

### 1. AST Output Helper Functions (outast.c)

**Pattern**: Repeated child expression emission with space prefix (~10
instances)
**Solution**: Create `emit_child()` helper function
**Savings**: ~750 bytes

### 2. Token Type Checking (parse.c, declare.c, expr.c)

**Pattern**: Repeated token check and advance (~50+ instances)
**Solution**: Add `expect(tok, err_code)` helper function
**Savings**: ~250 bytes

### 3. String Interning

**Pattern**: 32 strdup() calls across codebase
**Solution**: String table with interning to share identical strings
**Savings**: Significant if implemented

## Estimated Savings

**High-priority items**: ~1,050 bytes (emit_child: 750, expect: 250, misc: 50)
**With string interning**: Additional 2,000+ bytes potential
