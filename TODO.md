# TODO List

## Current Status

Both compiler passes are complete. Small programs compile and run in simulation.

## Known Limitations

- The 'signed' keyword is not supported
- Anonymous struct/union declarations don't work properly

## Remaining Work

### Testing and Validation
- [ ] More comprehensive test coverage for edge cases
- [ ] Test larger programs in simulation
- [ ] Verify all syscall wrappers work correctly

### Type System (Low Priority)
- [ ] Full type compatibility checking in all contexts
- [ ] Function signature checking at call sites
- [ ] Pointer arithmetic type checking

### Error Handling (Low Priority)
- [ ] Improve error messages and recovery
- [ ] Better diagnostics for common mistakes

### Optimizations
- [ ] Implement alloca (dynamic stack allocation)
- [ ] Investigate callee-pops calling convention
- [ ] Pass first argument in HL register

### Native Build (Future)
- [ ] Build and run compiler natively on Z80
- [ ] Verify code fits in 64KB constraint
