# Changelog

## [much_progress] - 2025-12-07

Major milestone release with significant improvements to code generation,
AST format, and tooling.

### AST Format Changes

The AST format has been simplified and improved:

**Removed CPP from AST output**:
- cc1 now performs all preprocessing internally
- AST output no longer contains preprocessor directives
- String literals are hex-encoded directly in the AST

**String Literal Encoding**:
```
U<hexname><len><hexdata>
```
- Strings are hex-encoded to avoid escape sequence issues
- Example: `U04str00548656c6c6f00` = "Hello\0"
- readStr() in cc2 properly decodes hex data

**Improved Type Annotations**:
- All expressions carry type/size suffixes (b/B/s/S/l/L/p/f/d/v)
- Lowercase = signed, Uppercase = unsigned
- Memory operations include width annotations: `:b`, `:s`, `:l`, `:p`

**Loop Lowering**:
- All loops (while, for, do-while) lowered to labeled if/goto by cc1
- Labels use format `L<n>_top`, `L<n>_continue`, `L<n>_break`
- Simplifies cc2 code generation

### Code Generation Improvements

**IX-Indexed Pointer Optimization**:
- Pointers allocated to IX register use direct indexed addressing
- `*ptr` generates `ld r,(ix+0)` instead of push/pop through HL
- Struct member access `p->field` uses `ld r,(ix+offset)`
- Eliminates intermediate register transfers

**Register Allocation**:
- BC and IX available for local variable allocation
- Variables with high reference counts get register allocation
- Parameters can be loaded directly into target registers

**Word Inc/Dec Fix**:
- Fixed 16-bit increment/decrement on IY-indexed stack variables
- Uses carry flag propagation: `dec (iy+lo); jr nc,$+3; dec (iy+hi)`
- 8-bit dec/inc affects C flag, 16-bit does not

**Comparison Optimization**:
- `== 0` and `!= 0` comparisons use `ld a,h; or l` instead of `sbc hl,de`
- Works for both signed and unsigned operands

**Dead Code Elimination**:
- DCE deferred to AST emission time
- Properly handles block elimination when IF statements removed
- Reduces generated code size

### Size Optimizations

**cc1 Size Reduction**:
- cc1 binary now under 40KB (target: <64KB for native build)
- Debug code wrapped in `#ifdef DEBUG`
- Trivial single-call functions inlined
- Static fdprintf strings converted to emit() table lookups

**cc2 Size Reduction**:
- Reduced by ~2,500 bytes
- TRACE blocks wrapped in `#ifdef DEBUG`
- Shared emit string table

### Whitesmith's Object Tools (ws/)

New toolchain for Whitesmith's relocatable object format:

**asz** - Assembler:
- Assembles Z80 source to relocatable object files
- Supports `.text`, `.data`, `.bss` segments
- Generates relocation records for linker
- Fixed `-o` flag handling for output filename

**wsld** - Linker:
- Links relocatable object files
- Resolves external references
- Generates executable or library output
- Supports `-r` for relocatable output

**wsnm** - Symbol table utility:
- Displays symbol information from object files
- Unified `-d` and `-g` disassembly modes
- Uses relocation-based symbol resolution
- Shows segment information (text/data/bss)

**wslib** - Library manager:
- Creates and manages static libraries
- Archive format compatible with original tools

**wsobj.c/h** - Shared object file support:
- Common functions for reading/writing object format
- Relocation encoding constants (REL_BUMP_*, REL_ABS, etc.)
- Segment name handling

### Build System

**GitHub Actions**:
- Added `gcc-multilib` for 32-bit ws tools build
- CI builds both native (32-bit) and host tools

**Debug Builds**:
- `make` builds with `-DDEBUG` for development
- Native build omits debug code for size
- `-v` verbose flags only available in debug builds

### Bug Fixes

- Fixed `char[] = "string"` array size inference when extern declaration precedes definition
- Fixed SC_EXTERN flag not cleared on definition
- Fixed string data freed before array size inference
- Fixed readHex2() to use consistent pattern with readHex4/8
- Fixed CALL return type handling
- Fixed block count mismatch when DCE eliminates IF statements

### Documentation

- AST_FORMAT.md: Complete AST binary format specification
- CC2_ARCHITECTURE.md: Code generator implementation details
- ws/README.md: Whitesmith's tools documentation
- CLAUDE.md: Project conventions and build instructions

### Files Changed

**Modified**:
- codegen.c: IX-indexed optimization, register targeting
- emitexpr.c: LOC_IX handler for byte/word loads
- emitops.c: Word inc/dec fix, comparison optimization
- emithelper.c: Local variable info in function headers
- parse.c: Clear SC_EXTERN on definition
- expr.c: Preserve string data for array size inference
- outast.c: Don't free string data in emitStrLit

**New**:
- ws/wsobj.c, ws/wsobj.h: Shared object file support

---

## Previous Tags

- **cc1_complete**: Pass 1 complete, all 142 tests passing
- **self-parse**: cc1 can parse its own source files
