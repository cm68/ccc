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
    struct code_emit *code;     // Generated code for this node
    int reg_hint;              // Register allocation hint
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
    int label;                  // Label number for jumps
    struct code_emit *code;     // Generated code for this statement
};
```

#### Code Emission Nodes
```c
struct code_emit {
    enum {
        CODE_LABEL,             // Label definition
        CODE_JUMP,              // Unconditional jump
        CODE_COND_JUMP,         // Conditional jump
        CODE_LOAD,              // Load value to register
        CODE_STORE,             // Store register to memory
        CODE_ALU_OP,            // ALU operation
        CODE_CALL,              // Function call
        CODE_RETURN,            // Return from function
        CODE_COMMENT,           // Assembly comment
    } type;

    char *asm_text;             // Assembly instruction text
    int label;                  // Label number (for jumps)
    struct code_emit *next;     // Next instruction
};
```

### Phases

#### Phase 1: Parse AST into Memory Trees

Replace current streaming parser with tree builder:
- `parse_function_ast()`: Read entire function into stmt/expr trees
- Build complete tree before any code generation
- Store in function context structure

#### Phase 2: Analysis and Annotation

Walk the tree and annotate:
- Type propagation (already done by cc1, verify)
- Register hints based on expression structure
- Label assignment for control flow
- Variable liveness analysis (future)

#### Phase 3: Code Generation

Walk tree and convert to code emission nodes:
- Replace operation nodes with instruction sequences
- Generate labels for control flow
- Allocate registers (simple scheme initially)
- Build linked list of code_emit nodes

#### Phase 4: Emit Assembly

Walk code emission list and output:
- Format instructions with proper indentation
- Emit labels at correct positions
- Add comments for readability

### Implementation Strategy

1. **Keep existing parseast.c as reference**
2. **Create new codegen.c module** for tree-based approach
3. **Implement incrementally**:
   - Phase 1: Parse expressions into trees
   - Phase 2: Parse statements into trees
   - Phase 3: Simple code generation (no optimization)
   - Phase 4: Emit assembly
4. **Test with simple programs** at each phase
5. **Add optimizations** once basic generation works

### Function Context Structure

```c
struct function_context {
    char *name;                 // Function name
    struct stmt *body;          // Function body statement tree
    int label_counter;          // For generating unique labels
    int temp_counter;           // For temporary variables
    struct code_emit *code;     // Generated code list
};
```

### Migration Path

1. **Commit current streaming implementation** ✓
2. **Document new architecture** (this file)
3. **Create new codegen.c with tree builders**
4. **Implement phase 1: AST → trees**
5. **Implement phase 3: trees → code_emit**
6. **Implement phase 4: code_emit → assembly**
7. **Test and validate**
8. **Replace parseast.c usage in cc2.c**

## Future Enhancements

With tree-based approach, these become possible:
- Constant folding and propagation
- Dead code elimination
- Common subexpression elimination
- Register allocation with graph coloring
- Instruction scheduling
- Peephole optimization

## Notes

- This is a significant re-architecture
- Current streaming approach works but limits optimization
- Tree-based approach is standard for modern compilers
- Enables future optimization passes
- Memory usage increases but manageable for typical functions
