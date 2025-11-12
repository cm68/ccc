# cc2 Architecture - Code Generation Pass

## Current State (Streaming Parser)

The current cc2 implementation uses a streaming parser approach:
- Reads AST from input file character by character
- Parses S-expressions on-the-fly
- Emits assembly code incrementally as it parses
- No in-memory representation of the function body
- Limited ability to analyze or optimize

## New Architecture (Tree-Based Code Generation)

### Overview

Replace streaming parser with a tree-based approach:
1. **Parse phase**: Read entire function AST into memory data structures
2. **Analysis phase**: Walk the tree, annotate with code generation info
3. **Emission phase**: Convert operation nodes to code emission nodes
4. **Output phase**: Walk code emission tree and generate assembly

### Key Benefits

- **Complete function view**: All statements and expressions in memory
- **Multi-pass optimization**: Can analyze before committing to code sequences
- **Better register allocation**: See all variable uses before allocation
- **Deferred code emission**: Generate optimal instruction sequences
- **Expression tree manipulation**: Optimize before code generation

### Data Structures

#### Expression Tree Nodes
```c
struct expr {
    unsigned char op;           // Operation ('+', '-', 'M', '=', etc.)
    struct expr *left;          // Left operand
    struct expr *right;         // Right operand
    struct type *type;          // Type information
    long value;                 // Constant value (if E_CONST)
    char *symbol;              // Symbol name (if SYM)
    int flags;                  // E_CONST, E_RESOLVED, etc.

    // Code generation fields
    char *asm_block;           // Assembly code for this node (or NULL)
    int label;                 // Label number (if needed for this node)
};
```

#### Statement Tree Nodes
```c
struct stmt {
    unsigned char type;         // Statement type ('I', 'W', 'R', 'B', etc.)
    struct expr *expr;          // Expression (for if/while condition, etc.)
    struct expr *expr2;         // Second expression (for assignments, etc.)
    struct stmt *then_branch;   // Then/body branch
    struct stmt *else_branch;   // Else branch
    struct stmt *next;          // Next statement in sequence

    // Code generation fields
    int label;                  // Label number for this statement (if needed)
    int label2;                 // Second label (for if/else end label)
    char *asm_block;           // Assembly code for this statement (or NULL)
};
```

### Phases

#### Phase 1: Parse AST into Memory Trees

Parse entire function from S-expression AST into memory:
- `parse_function_ast()`: Read entire (f ...) form into stmt/expr trees
- Assign label numbers to nodes during parsing:
  - If statements get `label` (and `label2` for if/else)
  - While/for loops get `label` for loop start
  - Switch statements get labels for cases
- Build complete tree before any code generation
- Store in function context structure

**Key point**: Label numbers are stored IN the tree nodes themselves

#### Phase 2: Code Generation

Walk the tree and replace operation nodes with ASM blocks:
- Visit each stmt/expr node
- Generate assembly text for the operation
- Store assembly text in `asm_block` field
- Use label numbers from nodes to emit jumps/labels
- After this phase, every node has its `asm_block` set

**Key point**: Nodes are transformed from operation descriptions to ASM blocks

#### Phase 3: Emit Assembly

Walk the tree with ASM blocks and output:
- Traverse stmt/expr trees
- Emit each node's `asm_block` text
- Emit labels using the label numbers in nodes
- Format with proper indentation
- Add comments for readability

**Key point**: This is a simple tree walk that outputs the prepared ASM blocks

### Implementation Strategy

1. **Reuse parsing machinery from parseast.c**:
   - Keep all low-level parsing: nextchar(), skip(), expect(), read_symbol(), read_number(), etc.
   - Keep buffer management and S-expression parsing infrastructure
   - These functions are solid and well-tested

2. **Modify handlers to return parse nodes**:
   - Change `parse_expr()` from `void` to `struct expr*`
   - Change `parse_stmt()` from `void` to `struct stmt*`
   - Handler functions return allocated tree nodes instead of printing
   - Assign label numbers during parsing

3. **Implement incrementally**:
   - Phase 1: Modify parseast.c handlers to build trees
   - Phase 2: Add code generation pass (trees → ASM blocks)
   - Phase 3: Add emission pass (ASM blocks → stdout)
4. **Test with simple programs** at each phase
5. **Add optimizations** once basic generation works

### Handler Signature Changes

**Current (parseast.c):**
```c
static void parse_expr(void);           // Prints diagnostics, no return value
static void parse_stmt(void);           // Prints diagnostics, no return value
static void handle_if(void);            // Prints diagnostics
static void handle_binary_op(unsigned char op);  // Prints diagnostics
```

**New (modified parseast.c):**
```c
static struct expr* parse_expr(void);   // Returns allocated expr tree
static struct stmt* parse_stmt(void);   // Returns allocated stmt tree
static struct stmt* handle_if(void);    // Returns if statement node with label
static struct expr* handle_binary_op(unsigned char op);  // Returns binary op node
```

**Key changes:**
- Allocate nodes with `malloc(sizeof(struct expr))` or `malloc(sizeof(struct stmt))`
- Build tree by setting left/right/then_branch/else_branch pointers
- Assign label numbers from global label_counter during parsing
- Return root of subtree instead of printing
- Diagnostic output (fdprintf to stderr) can remain for debugging

### Function Context Structure

```c
struct function_context {
    char *name;                 // Function name
    struct stmt *body;          // Function body statement tree
    int label_counter;          // For generating unique labels
};
```

### Example: Handler Transformation

**Current handle_if() (void return, prints diagnostics):**
```c
static void handle_if(void) {
    int if_label = label_counter++;
    int else_label;

    fdprintf(2, "IF (");
    skip();
    parse_expr();  /* condition - just prints */
    fdprintf(2, ") ");

    skip();
    parse_stmt();  /* then branch - just prints */

    skip();
    if (curchar != ')') {
        else_label = label_counter++;
        fdprintf(out_fd, "\tjp _if_end_%d\n", else_label);
        fdprintf(out_fd, "_if_%d:\n", if_label);
        fdprintf(2, " ELSE ");
        parse_stmt();  /* else branch - just prints */
        fdprintf(out_fd, "_if_end_%d:\n", else_label);
    } else {
        fdprintf(out_fd, "_if_%d:\n", if_label);
    }
    expect(')');
}
```

**New handle_if() (returns struct stmt*):**
```c
static struct stmt* handle_if(void) {
    struct stmt *s = malloc(sizeof(struct stmt));
    s->type = 'I';
    s->label = label_counter++;
    s->label2 = 0;
    s->asm_block = NULL;

    skip();
    s->expr = parse_expr();  /* condition - returns expr tree */

    skip();
    s->then_branch = parse_stmt();  /* then branch - returns stmt tree */

    skip();
    if (curchar != ')') {
        s->label2 = label_counter++;  /* Need second label for else */
        s->else_branch = parse_stmt();  /* else branch - returns stmt tree */
    } else {
        s->else_branch = NULL;
    }
    expect(')');

    return s;  /* Return the if statement node */
}
```

### Example: If Statement Processing

**Input AST:**
```lisp
(I (> (M:s $x) 0)
   (B (E (=:s $x (+ (M:s $x) 1)))))
```

**After Phase 1 (Parse with labels - handle_if returns this tree):**
```
stmt (type='I', label=0):
  expr (op='>', ...)
  then_branch:
    stmt (type='B', ...):
      stmt (type='E', ...):
        expr (op='=', ...):
          ...
```

**After Phase 2 (Generate ASM blocks):**
```
stmt (type='I', label=0, asm_block=""):
  expr (op='>', asm_block="ld hl,($x); ld de,0; ..."):
    ...
  then_branch:
    stmt (asm_block=""):
      stmt (asm_block=""):
        expr (op='=', asm_block="ld hl,($x); inc hl; ld ($x),hl"):
          ...
```

**Phase 3 (Emit Assembly):**
```asm
    ; Evaluate condition
    ld hl,($x)
    ld de,0
    or a
    sbc hl,de
    jp z,_if_0
    ; Then branch
    ld hl,($x)
    inc hl
    ld ($x),hl
_if_0:
```

### Migration Path

1. **Commit current streaming implementation** ✓
2. **Document new architecture** (this file) ✓
3. **Create new codegen.c with tree builders**
4. **Implement phase 1: Parse AST into stmt/expr trees with label assignment**
5. **Implement phase 2: Walk trees and generate ASM blocks**
6. **Implement phase 3: Walk ASM-annotated trees and emit assembly**
7. **Test and validate**
8. **Replace parseast.c usage in cc2.c**

## Key Architectural Insight

**Emit nodes ARE asm blocks** - they're just strings of assembly code stored in the tree nodes. The tree structure itself defines the control flow and emission order. No need for a separate linked list of code emission structures.

**Label numbers live in operator nodes** - during parsing, we assign label numbers to statement nodes that need them (if, while, etc.). During code generation, we use these label numbers to emit jumps and labels.

**Three simple phases**:
1. Parse → trees with labels
2. Trees → trees with ASM blocks
3. Trees with ASM blocks → assembly text

## Future Enhancements

With tree-based approach, these become possible:
- Constant folding and propagation (at tree level)
- Dead code elimination (tree pruning)
- Common subexpression elimination (tree manipulation)
- Register allocation improvements (multi-pass analysis)
- Instruction scheduling (ASM block reordering)
- Peephole optimization (ASM block analysis)

## Stack Frame Layout and Calling Convention

### Stack Frame Structure

The compiler uses a frame pointer (FP) based calling convention. Each function has a stack frame with the following layout:

```
Higher addresses
    +----------------+
    | Last parameter |  FP + (4 + sum of earlier param sizes)
    +----------------+
    | ...            |
    +----------------+
    | Param 2        |  FP + 6 (if 2-byte param)
    +----------------+
    | Param 1        |  FP + 4 (first parameter, pushed last)
    +----------------+
    | Return address |  FP + 2 (2 bytes)
    +----------------+
    | Saved FP       |  FP + 0 ← Frame Pointer points here
    +----------------+
    | Local 1        |  FP - 2 (first local, size depends on type)
    +----------------+
    | Local 2        |  FP - (2 + size1)
    +----------------+
    | ...            |
    +----------------+
    | Last local     |  FP - frame_size
    +----------------+
Lower addresses (stack grows downward)
```

### Parameter Layout

Parameters are pushed in **reverse order** (right-to-left):
- For `func(a, b, c)`: push c, push b, push a
- This makes the first parameter closest to the frame pointer
- Parameters have **positive offsets** from FP

**Example: `add(int x, int y)`**
```
FP + 6: y (second parameter, pushed first)
FP + 4: x (first parameter, pushed last)
FP + 2: return address
FP + 0: saved FP
FP - 2: result (local variable)
```

**Example: `test_multi(char a, int b, long c)`**
```
FP + 10: a (1 byte, third parameter)
FP + 8:  b (2 bytes, second parameter)
FP + 4:  c (4 bytes, first parameter)
FP + 2:  return address
FP + 0:  saved FP
FP - 1:  local_a (1 byte)
FP - 3:  local_b (2 bytes)
```

### Local Variable Layout

Local variables have **negative offsets** from FP:
- Stack grows downward
- First local at FP - size1
- Second local at FP - (size1 + size2)
- etc.

### Frame Allocation

Every function with parameters or locals calls `framealloc` and `framefree`:

**Prologue:**
```asm
_funcname:
    ld a, <frame_size>    ; Size in bytes (locals only)
    call framealloc       ; Sets up frame pointer
```

**Epilogue:**
```asm
    call framefree        ; Restores frame pointer
    ret
```

**Frame allocation rules:**
- Emit `framealloc`/`framefree` if: `frame_size > 0` OR function has parameters
- Functions with no parameters and no locals omit frame setup
- Frame size passed in register A is the **local variable space only** (not parameters)

**Examples:**

Function with parameters and locals: `add(int x, int y)` with local `result`
```asm
_add:
    ld a, 2           ; 2 bytes for local 'result'
    call framealloc   ; FP needed for params at FP+4, FP+6
    ...
    call framefree
    ret
```

Function with parameters but no locals: `identity(int x)`
```asm
_identity:
    ld a, 0           ; 0 bytes for locals
    call framealloc   ; FP still needed to access param at FP+4
    ...
    call framefree
    ret
```

Function with no parameters or locals: `empty()`
```asm
_empty:
    ...               ; No frame setup needed
    ret
```

### Runtime Library Functions

**framealloc** (implemented in runtime library):
- Input: A register = frame size (local variable space)
- Saves current frame pointer to stack
- Allocates stack space for locals
- Sets up new frame pointer
- Returns with FP pointing to saved FP location

**framefree** (implemented in runtime library):
- Restores previous frame pointer
- Deallocates local variable stack space
- Leaves return address at top of stack for ret instruction

### Variable Access

All variable access is frame-relative:

**Parameters:** Use positive offsets
```asm
; Access first parameter (int x) at FP+4
ld hl, (ix+4)    ; Assuming IX is frame pointer
```

**Locals:** Use negative offsets
```asm
; Access first local (int result) at FP-2
ld (ix-2), hl    ; Assuming IX is frame pointer
```

### Type Sizes

Variables are allocated based on their type:
- `char`: 1 byte
- `short`, `int`, pointers: 2 bytes
- `long`, `float`: 4 bytes
- `double`: 8 bytes

### Current Implementation Status

**Implemented:**
- Stack frame offset assignment for parameters (Phase 1.5)
- Stack frame offset assignment for local variables (Phase 1.5)
- Frame size calculation
- `framealloc`/`framefree` emission in prologue/epilogue
- Proper handling of functions with/without parameters/locals
- Helper function calls for all arithmetic operations
- Width and signedness tracking for code generation

**In Progress:**
- Actual variable address calculation using frame offsets
- Code emission using frame-relative addressing

**Next Steps:**
- Implement variable lookup by name to get frame offset
- Generate frame-relative addressing (IX+offset or IX-offset)
- Complete memory operation code generation (load/store)

## Notes

- Simpler than complex code_emit structures
- ASM blocks are just strings in tree nodes
- Label numbers stored directly in tree nodes
- Tree walk for emission is straightforward
- Memory usage increases but manageable for typical functions
- Enables optimization at tree level before ASM generation
