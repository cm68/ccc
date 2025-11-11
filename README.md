ccc - full native C compiler

This is a 2-pass C compiler written in C, currently under reconstruction from
a paper printout. Pass 1 (cc1) is substantially complete; pass 2 (cc2) is not
yet implemented.

## Project Status

**Pass 1 (cc1) - Complete** ✓ Tagged as **cc1_complete** and **self-parse**
- Full C preprocessor, type system, expression/statement parsing, AST emission
- 134 tests passing, 18/18 source files self-host
- See CLAUDE.md for detailed architecture and features

**Debugging Tools**
- **AST Interpreter** (interp.lisp): Execute AST without code generation
- **AST Pretty Printer** (astpp.lisp): Format AST for human inspection
- See INTERP.md and ASTPP.md for details

**Pass 2 (cc2) - Work In Progress**
- AST parser foundation in parseast.c
- Code generation not yet started

## Architecture

This is a 2-pass compiler:

**Pass 1 (cc1)**: Recursive descent parser with embedded C preprocessor
- Parses and validates C source code
- Outputs AST in S-expression format (single-char operators)
- Uses Unix syscalls (write) instead of stdio for output
- ~4,800 lines of C code

**Pass 2 (cc2)**: Code generator and assembler (WIP)
- Reads AST from pass 1 (S-expression format)
- Table-driven parser with handler functions for each operation
- Uses Unix syscalls (read/write) instead of stdio
- parseast.c: ~730 lines of parser infrastructure
- Handles memory width annotations (:b :s :l :p :f :d)
- Code generation not yet implemented

## File Organization

**Pass 1 (cc1) files:**
- cc1.c - Main entry point, orchestration
- lex.c - Lexical analyzer (tokenizer)
- parse.c - Statement and declaration parsing
- expr.c - Expression parsing with precedence
- type.c - Type system management
- declare.c - Declaration processing
- outast.c - AST emission in S-expression format
- macro.c - CPP macro definition and expansion
- io.c - Character I/O and file stack management
- error.c - Error reporting
- util.c - Utilities (fdprintf, bitdef, etc.)
- kw.c - Keyword lookup tables

**Pass 2 (cc2) files:**
- cc2.c - Main entry point, command-line processing
- parseast.c - Table-driven AST parser
- util.c - Shared utilities (fdprintf)

**Auto-generated files:**
- tokenlist.c, enumlist.h - Token definitions
- error.h - Error code definitions
- debug.h, debugtags.c - Debug/verbose infrastructure
- op_pri.h - Operator priority table

**Stub system headers (include/):**
- stdio.h, stdlib.h, string.h, stdarg.h - C standard library stubs
- fcntl.h, unistd.h, signal.h - POSIX system call stubs
- libgen.h - Path manipulation stubs
- sys/stat.h, sys/wait.h - System header stubs
- Minimal declarations to avoid GNU libc advanced preprocessor features

## Usage

**Using the ccc driver (recommended):**
```bash
# Full compilation (when cc2 is complete)
./ccc -o program source.c

# Execute with interpreter (debugging/testing)
./ccc -x source.c

# Keep intermediate AST file
./ccc -k -o program source.c
```

**Pass 1 - Parse and output AST:**
```bash
./cc1 -E source.c > output.ast
```

**Pass 2 - Parse AST (code generation not yet implemented):**
```bash
./cc2 output.ast -o executable
```

**Full pipeline (when complete):**
```bash
./cc1 -E source.c | ./cc2 -o executable
```

## Debugging the Parser and AST

The `-x` option executes the generated AST with a Common Lisp interpreter, providing
a way to validate that the parser is producing correct AST without needing a working
code generator.

**Quick validation:**
```bash
./ccc -x tests/arith_widths.c
```

This compiles the source to AST, then executes it with the interpreter. If the program
runs and produces the expected result, the parser is working correctly.

**Debugging workflow:**

1. Write a test program with known expected behavior
2. Compile and execute with `-x`:
   ```bash
   ./ccc -x mytest.c
   ```
3. Check the exit code and output match expectations
4. If incorrect, inspect the AST file (automatically saved as `mytest.ast`)
5. Compare AST structure against expected operations

**Example - verify arithmetic:**
```c
// test.c
int main() {
    int a = 10;
    int b = 20;
    int c = a + b;
    return c;  // Should return 30
}
```

```bash
$ ./ccc -x test.c
=== Pass 1: Parsing test.c ===

=== Executing AST with interpreter ===
Program exited with code: 30

AST saved to: test.ast
```

The exit code of 30 confirms the parser correctly:
- Parsed declarations
- Generated assignment operations
- Performed arithmetic

### AST Pretty Printer

For visual inspection of AST structure, use the standalone pretty printer:

```bash
# Generate AST
./cc1 -E test.c > test.ast

# Pretty print with human-readable formatting
./astpp.lisp test.ast
```

**Output:**
```
FUNCTION main() -> _short_
{
  BLOCK {
    DECL a : _short_
    DECL b : _short_
    DECL c : _short_
    EXPR:
      (ASSIGN:short $a (NARROW:short 10))
    EXPR:
      (ASSIGN:short $b (NARROW:short 20))
    EXPR:
      (ASSIGN:short $c (ADD (DEREF:short $a) (DEREF:short $b)))
    RETURN (DEREF:short $c)
  }
}
```

The pretty printer translates single-char operators to readable names (M→DEREF, =→ASSIGN, +→ADD, etc.) and shows type width annotations, making it easy to verify the AST structure at a glance.

**Use cases:**
- Debug parser output by visual inspection
- Understand AST structure for complex constructs
- Compare AST between different versions
- Learn the AST format

See [ASTPP.md](ASTPP.md) for complete documentation.

**Benefits of interpreter-based debugging:**
- Test parser without implementing code generator
- Validate type conversions and promotions
- Verify control flow (loops, conditionals, function calls)
- Confirm expression evaluation and constant folding
- Quick iteration on parser changes

**Interpreter limitations:**
- Simplified memory model (doesn't simulate real memory addresses)
- No pointer arithmetic validation
- Type conversions are pass-through (no actual narrowing/widening)
- Some operations simplified for interpretation

See INTERP.md for complete interpreter documentation.
