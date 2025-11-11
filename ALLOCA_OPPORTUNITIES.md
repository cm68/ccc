# alloca() Optimization Opportunities

**Goal**: Replace malloc/free pairs in same function scope with alloca() to reduce code size

**Benefits of alloca()**:
- No need for explicit free() calls
- Automatic deallocation when function returns
- Faster than malloc (no heap management overhead)
- Reduces code size by eliminating free() calls
- Eliminates possibility of memory leaks from error paths

**Risks**:
- Stack overflow if allocating large amounts
- Not portable to all systems (though widely available)
- Can't be used if data needs to persist beyond function scope

## High Confidence Opportunities

### 1. type.c: struct/union tag name handling (getbasetype)

**Location**: type.c:697-750 in getbasetype() function

**Pattern**:
```c
char *s = NULL;

if (cur.type == SYM) {
    s = strdup(cur.v.name);    // line 697 - MALLOC
    // ... use s ...
    free(s);                   // lines 703, 711, 720, 725, 749 - FREE
}
```

**Analysis**:
- Variable `s` is allocated with strdup() to hold struct/union tag name
- Used only within this function for name lookup
- Freed at 5 different exit points (error paths and success paths)
- Tag names are typically short (<32 chars)

**Recommendation**:
```c
char *s = NULL;
char s_buf[64];  // Stack buffer for tag name

if (cur.type == SYM) {
    s = s_buf;
    strncpy(s, cur.v.name, sizeof(s_buf)-1);
    s[sizeof(s_buf)-1] = '\0';
    // ... use s ...
    // No free() needed - automatic cleanup
}
```

Or with alloca():
```c
if (cur.type == SYM) {
    int len = strlen(cur.v.name) + 1;
    s = alloca(len);
    strcpy(s, cur.v.name);
    // ... use s ...
    // No free() needed
}
```

**Impact**:
- Eliminates 5 free() calls
- Simplifies control flow (no error-path cleanup needed)
- Estimated savings: ~40 bytes

### 2. declare.c: parameter name parsing (parsedecl)

**Location**: declare.c:281, 326 in parsedecl() function

**Pattern**:
```c
// Parse parameter name
char *param_name = parse_param_name(allow_anonymous);  // calls strdup() at line 42
// ... use param_name ...
free(param_name);  // lines 281, 326
```

**Analysis**:
- parse_param_name() allocates memory with strdup()
- Used only within parsedecl() for temporary processing
- Freed before function ends (lines 281, 326)
- Parameter names are typically short (<32 chars)

**Recommendation**:
Modify parse_param_name() to accept a buffer:
```c
static char *
parse_param_name(boolean allow_anonymous, char *buf, int bufsize)
{
    if (cur.type == SYM) {
        strncpy(buf, cur.v.name, bufsize-1);
        buf[bufsize-1] = '\0';
        gettoken();
        return buf;
    }
    return allow_anonymous ? "" : NULL;
}

// In parsedecl():
char param_name_buf[64];
char *param_name = parse_param_name(allow_anonymous, param_name_buf, sizeof(param_name_buf));
// ... use param_name ...
// No free() needed
```

**Impact**:
- Eliminates 2 free() calls
- Eliminates strdup() overhead (2 instances)
- Estimated savings: ~30 bytes

### 3. ccc.c: basename temporary (get_basename_no_ext)

**Location**: ccc.c:83-90 in get_basename_no_ext() function

**Pattern**:
```c
char *temp = strdup(filename);   // line 83 - MALLOC
char *base = basename(temp);
char *result;

/* Make a copy since basename() result points into temp */
result = strdup(base);
free(temp);                      // line 90 - FREE
```

**Analysis**:
- temp is allocated to hold filename copy for basename() processing
- basename() returns pointer into temp, so we need temp to stay valid
- Freed immediately after extracting base name
- Filenames are typically short (<256 chars)

**Recommendation**:
```c
char temp[PATH_MAX];
strncpy(temp, filename, sizeof(temp)-1);
temp[sizeof(temp)-1] = '\0';
char *base = basename(temp);
char *result = strdup(base);  // Still need to return allocated result
// No free() needed for temp
```

Or with alloca():
```c
int len = strlen(filename) + 1;
char *temp = alloca(len);
strcpy(temp, filename);
char *base = basename(temp);
char *result = strdup(base);
// No free() needed for temp
```

**Impact**:
- Eliminates 1 free() call
- Eliminates 1 strdup() call
- Estimated savings: ~15 bytes

## Medium Confidence Opportunities

### 4. parse.c: loop label generation (generate_loop_label)

**Location**: parse.c:20-23 in generate_loop_label() function

**Pattern**:
```c
static char *
generate_loop_label(const char *prefix)
{
    char *label = malloc(32);      // line 20 - MALLOC
    sprintf(label, "%s%d", prefix, loop_label_counter++);
    return label;                   // RETURNED - freed elsewhere
}
```

**Analysis**:
- Label is allocated and returned
- Stored in statement structure (st->label)
- Freed later in free_stmt() at parse.c:1026

**Recommendation**:
NOT a good candidate because:
- Memory persists beyond function scope
- Stored in statement tree structure
- Freed much later during cleanup

**Action**: None - this is appropriate use of malloc

### 5. parse.c: static variable mangling (mangle_static_name)

**Location**: parse.c:65-81 in mangle_static_name() function

**Pattern**:
```c
char *mangled;
mangled = malloc(len);             // lines 65, 71, 77 - MALLOC
sprintf(mangled, ...);
return mangled;                    // RETURNED - stored in var->mangled_name
```

**Analysis**:
- Mangled name is allocated and returned
- Stored in name structure (var->mangled_name)
- Persists for lifetime of variable

**Recommendation**:
NOT a good candidate because:
- Memory persists beyond function scope
- Stored in symbol table
- Used during AST emission

**Action**: None - this is appropriate use of malloc

## Low Confidence / Not Recommended

### 6. lex.c: conditional stack (do_cpp)

**Location**: lex.c:409, 429, 444, 469

**Pattern**:
```c
// In IF/IFDEF/IFNDEF cases:
c = malloc(sizeof(*c));    // lines 409, 429, 444 - MALLOC
c->next = cond;
cond = c;

// In ENDIF case:
c = cond;
cond = c->next;
free(c);                   // line 469 - FREE
```

**Analysis**:
- malloc in one switch case (IF/IFDEF/IFNDEF)
- free in different switch case (ENDIF)
- Implements a stack for nested conditionals
- Can span multiple function calls

**Recommendation**:
NOT suitable for alloca because:
- Allocation and deallocation in different code paths
- Stack can persist across multiple calls to do_cpp()
- Implements persistent data structure

**Action**: None - this is appropriate use of malloc

### 7. macro.c: macro structures

All malloc calls in macro.c create persistent data structures (macro definitions, parameter lists) that live beyond function scope.

**Action**: None - appropriate use of malloc

### 8. io.c: file stack and text buffers

All malloc calls in io.c create persistent structures:
- File stack entries (struct incl)
- Text buffers (struct textbuf)
- CPP output buffer

**Action**: None - appropriate use of malloc

## Summary

**High Priority Conversions** (clear wins):

1. **type.c:697-750** - struct/union tag name: ~40 bytes saved
2. **declare.c:281,326** - parameter names: ~30 bytes saved
3. **ccc.c:83-90** - basename temp: ~15 bytes saved

**Total estimated savings: ~85 bytes**

**Additional benefits**:
- Simplified error handling (no cleanup needed)
- Eliminates 8 free() calls total
- Reduces possibility of memory leaks

## Implementation Notes

**Option 1: Fixed-size stack buffers**
- Simplest approach
- Most portable
- Safe for known-small strings (identifiers, tag names)
- Example: `char buf[64]` for identifier names

**Option 2: alloca()**
- More flexible (sizes computed at runtime)
- Less portable (not in C standard, but widely available)
- Requires careful size validation
- May need `#ifdef` guards for portability

**Recommendation**: Start with fixed-size buffers (Option 1) for the three high-priority cases. This is simpler, more portable, and sufficient for all identified cases since:
- Tag names are identifiers (typically <32 chars)
- Parameter names are identifiers (typically <32 chars)
- Filenames can use PATH_MAX constant

alloca() can be considered for future optimizations where size is truly dynamic.
